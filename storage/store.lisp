;;;; storage/store.lisp
;;;;
;;;; Persistent key-value store for daemon state.
;;;; Age-encrypted JSON backend with namespace support.
;;;;
;;;; Storage location: ~/.crichton/data/<namespace>.age
;;;; Each namespace is a separate encrypted JSON file.
;;;;
;;;; Use cases:
;;;;   - "meters" namespace: token usage statistics
;;;;   - "rss" namespace: seen-item tracking
;;;;   - "scheduler" namespace: task persistence
;;;;   - "knowledge" namespace: local knowledge base
;;;;
;;;; Unlike skills/kv-store.lisp (which is skill-scoped), this is
;;;; daemon-wide persistent state.

(in-package #:crichton/storage)

;;; --- Global state ---

(defvar *storage-lock* (bt:make-lock "storage-lock"))

(defvar *storage-cache* (make-hash-table :test #'equal)
  "In-memory cache: namespace string → hash-table of key→value.")

(defvar *dirty-namespaces* (make-hash-table :test #'equal)
  "Set of namespace names that have unsaved changes.")

;;; --- Path helper ---

(defun storage-file-path (namespace)
  "Return the .age file path for NAMESPACE: ~/.crichton/data/<namespace>.age"
  (merge-pathnames (make-pathname :name namespace :type "age")
                   (merge-pathnames #p"data/" crichton/config:*agent-home*)))

;;; --- JSON serialization ---

(defun storage-to-json-string (namespace data-ht)
  "Serialize DATA-HT into JSON envelope string with version and metadata."
  (let ((envelope (make-hash-table :test #'equal)))
    (setf (gethash "version" envelope) 1
          (gethash "namespace" envelope) namespace
          (gethash "updated_at" envelope) (crichton/config:iso8601-now)
          (gethash "data" envelope) data-ht)
    (let ((*print-pretty* t))
      (with-output-to-string (s)
        (shasht:write-json envelope s)))))

(defun json-string-to-storage (json-string)
  "Parse JSON envelope, returning (values namespace data-ht).
   DATA-HT is a hash-table with string keys and arbitrary JSON values."
  (let ((envelope (shasht:read-json json-string)))
    (unless (hash-table-p envelope)
      (error "Storage: expected JSON object, got ~S" (type-of envelope)))
    (let ((data (gethash "data" envelope)))
      (unless (hash-table-p data)
        (setf data (make-hash-table :test #'equal)))
      (values (gethash "namespace" envelope) data))))

;;; --- File I/O (reuses crypto/age.lisp) ---

(defun save-namespace (namespace data-ht)
  "Serialize DATA-HT to JSON, encrypt via age, and write to disk."
  (let* ((json (storage-to-json-string namespace data-ht))
         (plaintext (sb-ext:string-to-octets json :external-format :utf-8))
         (path (storage-file-path namespace)))
    (ensure-directories-exist path)
    (crichton/crypto:encrypt-to-file plaintext path)
    (log:debug "Storage saved for namespace ~A" namespace)
    path))

(defun load-namespace (namespace)
  "Decrypt and parse the storage file for NAMESPACE.
   Returns a hash-table (string→value). Returns empty hash-table if file not found."
  (let ((path (storage-file-path namespace)))
    (unless (probe-file path)
      (return-from load-namespace (make-hash-table :test #'equal)))
    (handler-case
        (let* ((plaintext (crichton/crypto:decrypt-from-file path))
               (json-string (sb-ext:octets-to-string plaintext :external-format :utf-8)))
          (multiple-value-bind (parsed-namespace data-ht)
              (json-string-to-storage json-string)
            (declare (ignore parsed-namespace))
            data-ht))
      (error (c)
        (log:warn "Failed to load storage for namespace ~A: ~A" namespace c)
        (make-hash-table :test #'equal)))))

;;; --- Cache management ---

(defun ensure-namespace (namespace)
  "Return the cached hash-table for NAMESPACE, loading from disk if needed.
   Caller must hold *storage-lock*."
  (or (gethash namespace *storage-cache*)
      (setf (gethash namespace *storage-cache*) (load-namespace namespace))))

(defun mark-dirty (namespace)
  "Mark NAMESPACE as having unsaved changes.
   Caller must hold *storage-lock*."
  (setf (gethash namespace *dirty-namespaces*) t))

(defun flush-namespace (namespace)
  "Save the cached data for NAMESPACE to disk.
   Caller must hold *storage-lock*."
  (let ((data-ht (gethash namespace *storage-cache*)))
    (when data-ht
      (save-namespace namespace data-ht)
      (remhash namespace *dirty-namespaces*))))

(defun clear-storage-cache ()
  "Wipe the in-memory storage cache. Does NOT save to disk."
  (bt:with-lock-held (*storage-lock*)
    (clrhash *storage-cache*)
    (clrhash *dirty-namespaces*)
    t))

;;; --- Core API ---

(defun store-get (namespace key &optional default)
  "Get the value for KEY in NAMESPACE. Returns DEFAULT if not found."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace)))
      (multiple-value-bind (value found-p)
          (gethash key data-ht)
        (if found-p value default)))))

(defun store-set (namespace key value)
  "Set KEY to VALUE in NAMESPACE. Marks namespace as dirty.
   Call FLUSH-NAMESPACE or FLUSH-ALL-STORAGE to persist to disk.
   Returns VALUE."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace)))
      (setf (gethash key data-ht) value)
      (mark-dirty namespace)
      value)))

(defun store-delete (namespace key)
  "Delete KEY from NAMESPACE. Returns T if the key existed."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace)))
      (when (nth-value 1 (gethash key data-ht))
        (remhash key data-ht)
        (mark-dirty namespace)
        t))))

(defun store-exists-p (namespace key)
  "Return T if KEY exists in NAMESPACE."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace)))
      (nth-value 1 (gethash key data-ht)))))

(defun store-list-keys (namespace &optional prefix)
  "List all keys in NAMESPACE. If PREFIX is given, filter by it."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace))
          (keys nil))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (or (null prefix)
                           (and (<= (length prefix) (length k))
                                (string= prefix k :end2 (length prefix))))
                   (push k keys)))
               data-ht)
      (sort keys #'string<))))

(defun store-list-namespaces ()
  "Return a sorted list of all cached namespace names."
  (bt:with-lock-held (*storage-lock*)
    (let (names)
      (maphash (lambda (k v) (declare (ignore v)) (push k names)) *storage-cache*)
      (sort names #'string<))))

;;; --- Batch operations ---

(defun store-get-all (namespace)
  "Return all key-value pairs in NAMESPACE as an alist."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace))
          (result nil))
      (maphash (lambda (k v) (push (cons k v) result)) data-ht)
      (sort result #'string< :key #'car))))

(defun store-set-all (namespace alist)
  "Replace all data in NAMESPACE with ALIST of (key . value) pairs.
   Marks namespace as dirty. Returns T."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (make-hash-table :test #'equal)))
      (dolist (pair alist)
        (setf (gethash (car pair) data-ht) (cdr pair)))
      (setf (gethash namespace *storage-cache*) data-ht)
      (mark-dirty namespace)
      t)))

(defun store-clear-namespace (namespace)
  "Delete all data in NAMESPACE (cache and disk)."
  (bt:with-lock-held (*storage-lock*)
    (remhash namespace *storage-cache*)
    (remhash namespace *dirty-namespaces*)
    (let ((path (storage-file-path namespace)))
      (when (probe-file path)
        (delete-file path)))
    (log:info "Storage cleared for namespace ~A" namespace)
    t))

;;; --- Persistence ---

(defun flush-all-storage ()
  "Save all dirty namespaces to disk."
  (bt:with-lock-held (*storage-lock*)
    (let ((count 0))
      (maphash (lambda (namespace dirty-p)
                 (declare (ignore dirty-p))
                 (handler-case
                     (progn
                       (flush-namespace namespace)
                       (incf count))
                   (error (c)
                     (log:warn "Failed to flush storage for ~A: ~A" namespace c))))
               *dirty-namespaces*)
      (log:info "Flushed ~D storage namespace~:P" count)
      count)))

(defun preload-storage ()
  "Scan ~/.crichton/data/*.age and load all into cache."
  (bt:with-lock-held (*storage-lock*)
    (let* ((data-dir (merge-pathnames #p"data/" crichton/config:*agent-home*))
           (pattern (merge-pathnames (make-pathname :name :wild :type "age") data-dir))
           (files (directory pattern))
           (count 0))
      (dolist (file files)
        (let ((namespace (pathname-name file)))
          (handler-case
              (progn
                (setf (gethash namespace *storage-cache*) (load-namespace namespace))
                (incf count))
            (error (c)
              (log:warn "Failed to load storage for ~A: ~A" namespace c)))))
      (log:info "Preloaded ~D storage namespace~:P" count)
      count)))

;;; --- Reporting ---

(defun storage-usage (namespace)
  "Return usage plist for NAMESPACE: (:NAMESPACE name :KEY-COUNT n :SIZE-BYTES n)."
  (bt:with-lock-held (*storage-lock*)
    (let ((data-ht (ensure-namespace namespace)))
      (list :namespace namespace
            :key-count (hash-table-count data-ht)
            :dirty-p (gethash namespace *dirty-namespaces*)
            :size-bytes (let ((json (storage-to-json-string namespace data-ht)))
                          (length (sb-ext:string-to-octets json :external-format :utf-8)))))))

(defun storage-report (&key (stream *standard-output*))
  "Print a formatted text report of all storage usage."
  (bt:with-lock-held (*storage-lock*)
    (if (zerop (hash-table-count *storage-cache*))
        (format stream "No storage namespaces loaded.~%")
        (progn
          (format stream "~&Storage Usage Report~%")
          (format stream "~A~%" (make-string 60 :initial-element #\-))
          (let ((total-keys 0)
                (total-bytes 0))
            (maphash (lambda (namespace data-ht)
                       (let* ((keys (hash-table-count data-ht))
                              (json (storage-to-json-string namespace data-ht))
                              (bytes (length (sb-ext:string-to-octets json :external-format :utf-8)))
                              (dirty-p (gethash namespace *dirty-namespaces*)))
                         (format stream "  ~A: ~D key~:P, ~:D byte~:P~A~%"
                                 namespace keys bytes
                                 (if dirty-p " [DIRTY]" ""))
                         (incf total-keys keys)
                         (incf total-bytes bytes)))
                     *storage-cache*)
            (format stream "~A~%" (make-string 60 :initial-element #\-))
            (format stream "  Total: ~D key~:P, ~:D byte~:P across ~D namespace~:P~%"
                    total-keys total-bytes (hash-table-count *storage-cache*)))))))
