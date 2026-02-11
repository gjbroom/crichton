;;;; skills/kv-store.lisp
;;;;
;;;; Age-encrypted KV store backend for WASM skills.
;;;; One age-encrypted JSON file per skill: ~/.crichton/kv/<skill-id>.age
;;;; In-memory write-through cache with per-skill isolation and quota enforcement.

(in-package #:crichton/skills)

;;; --- Conditions ---

(define-condition kv-quota-exceeded (error)
  ((skill-id   :initarg :skill-id   :reader kv-quota-exceeded-skill-id)
   (quota-type :initarg :quota-type :reader kv-quota-exceeded-quota-type)
   (limit      :initarg :limit      :reader kv-quota-exceeded-limit)
   (current    :initarg :current    :reader kv-quota-exceeded-current))
  (:report (lambda (c stream)
             (format stream "KV quota exceeded for skill ~A: ~A limit ~A, current ~A"
                     (kv-quota-exceeded-skill-id c)
                     (kv-quota-exceeded-quota-type c)
                     (kv-quota-exceeded-limit c)
                     (kv-quota-exceeded-current c)))))

;;; --- Global state ---

(defvar *kv-lock* (bt:make-lock "kv-store-lock"))

(defvar *kv-cache* (make-hash-table :test #'equal)
  "In-memory cache: skill-id string → hash-table of key→value.")

;;; --- Timestamp ---

(defun kv-iso8601-now ()
  "Return current UTC time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

;;; --- Path helper ---

(defun kv-file-path (skill-id)
  "Return the .age file path for SKILL-ID: ~/.crichton/kv/<skill-id>.age"
  (merge-pathnames (make-pathname :name skill-id :type "age")
                   (merge-pathnames #p"kv/" crichton/config:*agent-home*)))

;;; --- JSON serialization ---

(defun kv-to-json-string (skill-id data-ht)
  "Serialize DATA-HT into JSON envelope string with version and metadata."
  (let ((envelope (make-hash-table :test #'equal)))
    (setf (gethash "version" envelope) 1
          (gethash "skill_id" envelope) skill-id
          (gethash "updated_at" envelope) (kv-iso8601-now)
          (gethash "data" envelope) data-ht)
    (unless (gethash "created_at" envelope)
      (setf (gethash "created_at" envelope) (kv-iso8601-now)))
    (let ((*print-pretty* nil))
      (with-output-to-string (s)
        (shasht:write-json envelope s)))))

(defun json-string-to-kv (json-string)
  "Parse JSON envelope, returning (values skill-id data-ht).
   DATA-HT is a hash-table with string keys and string values."
  (let ((envelope (shasht:read-json json-string)))
    (unless (hash-table-p envelope)
      (error "KV store: expected JSON object, got ~S" (type-of envelope)))
    (let ((data (gethash "data" envelope)))
      (unless (hash-table-p data)
        (setf data (make-hash-table :test #'equal)))
      (values (gethash "skill_id" envelope) data))))

;;; --- File I/O (reuses crypto/age.lisp) ---

(defun save-skill-kv (skill-id data-ht)
  "Serialize DATA-HT to JSON, encrypt via age, and write to disk."
  (let* ((json (kv-to-json-string skill-id data-ht))
         (plaintext (sb-ext:string-to-octets json :external-format :utf-8))
         (path (kv-file-path skill-id)))
    (ensure-directories-exist path)
    (crichton/crypto:encrypt-to-file plaintext path)
    (log:debug "KV store saved for skill ~A" skill-id)
    path))

(defun load-skill-kv (skill-id)
  "Decrypt and parse the KV file for SKILL-ID.
   Returns a hash-table (string→string). Returns empty hash-table if file not found."
  (let ((path (kv-file-path skill-id)))
    (unless (probe-file path)
      (return-from load-skill-kv (make-hash-table :test #'equal)))
    (let* ((plaintext (crichton/crypto:decrypt-from-file path))
           (json-string (sb-ext:octets-to-string plaintext :external-format :utf-8)))
      (multiple-value-bind (parsed-skill-id data-ht)
          (json-string-to-kv json-string)
        (declare (ignore parsed-skill-id))
        data-ht))))

;;; --- Cache management ---

(defun ensure-skill-kv (skill-id)
  "Return the cached hash-table for SKILL-ID, loading from disk if needed.
   Caller must hold *kv-lock*."
  (or (gethash skill-id *kv-cache*)
      (setf (gethash skill-id *kv-cache*) (load-skill-kv skill-id))))

(defun flush-skill-kv (skill-id)
  "Save the cached data for SKILL-ID to disk.
   Caller must hold *kv-lock*."
  (let ((data-ht (gethash skill-id *kv-cache*)))
    (when data-ht
      (save-skill-kv skill-id data-ht))))

(defun clear-kv-cache ()
  "Wipe the in-memory KV cache."
  (bt:with-lock-held (*kv-lock*)
    (clrhash *kv-cache*)
    t))

;;; --- Quota enforcement ---

(defun kv-max-keys ()
  (let ((v (crichton/config:config-section-get :kv :max-keys 100)))
    (if (numberp v) v (parse-integer v))))

(defun kv-max-value-bytes ()
  (let ((v (crichton/config:config-section-get :kv :max-value-bytes 10240)))
    (if (numberp v) v (parse-integer v))))

(defun kv-max-total-bytes ()
  (let ((v (crichton/config:config-section-get :kv :max-total-bytes 1048576)))
    (if (numberp v) v (parse-integer v))))

(defun kv-data-total-bytes (data-ht)
  "Compute total byte size of all keys and values in DATA-HT."
  (let ((total 0))
    (maphash (lambda (k v)
               (incf total (length (sb-ext:string-to-octets k :external-format :utf-8)))
               (when (stringp v)
                 (incf total (length (sb-ext:string-to-octets v :external-format :utf-8)))))
             data-ht)
    total))

(defun check-kv-quota (skill-id data-ht key value)
  "Signal KV-QUOTA-EXCEEDED if adding KEY=VALUE would exceed quotas."
  (let ((max-keys (kv-max-keys))
        (max-value (kv-max-value-bytes))
        (max-total (kv-max-total-bytes))
        (current-count (hash-table-count data-ht))
        (value-bytes (length (sb-ext:string-to-octets value :external-format :utf-8))))
    (when (and (not (gethash key data-ht))
               (>= current-count max-keys))
      (error 'kv-quota-exceeded
             :skill-id skill-id
             :quota-type :max-keys
             :limit max-keys
             :current current-count))
    (when (> value-bytes max-value)
      (error 'kv-quota-exceeded
             :skill-id skill-id
             :quota-type :max-value-bytes
             :limit max-value
             :current value-bytes))
    (let ((projected-total (+ (kv-data-total-bytes data-ht)
                              (length (sb-ext:string-to-octets key :external-format :utf-8))
                              value-bytes
                              (- (if (gethash key data-ht)
                                     (+ (length (sb-ext:string-to-octets key :external-format :utf-8))
                                        (length (sb-ext:string-to-octets
                                                 (gethash key data-ht)
                                                 :external-format :utf-8)))
                                     0)))))
      (when (> projected-total max-total)
        (error 'kv-quota-exceeded
               :skill-id skill-id
               :quota-type :max-total-bytes
               :limit max-total
               :current projected-total)))))

;;; --- Core API ---

(defun kv-get (skill-id key)
  "Get the value for KEY in SKILL-ID's KV store. Returns NIL if not found."
  (bt:with-lock-held (*kv-lock*)
    (let ((data-ht (ensure-skill-kv skill-id)))
      (gethash key data-ht))))

(defun kv-set (skill-id key value)
  "Set KEY to VALUE in SKILL-ID's KV store. Write-through to disk. Returns T."
  (bt:with-lock-held (*kv-lock*)
    (let ((data-ht (ensure-skill-kv skill-id)))
      (check-kv-quota skill-id data-ht key value)
      (setf (gethash key data-ht) value)
      (save-skill-kv skill-id data-ht)
      t)))

(defun kv-delete (skill-id key)
  "Delete KEY from SKILL-ID's KV store. Returns T if the key existed."
  (bt:with-lock-held (*kv-lock*)
    (let ((data-ht (ensure-skill-kv skill-id)))
      (when (gethash key data-ht)
        (remhash key data-ht)
        (save-skill-kv skill-id data-ht)
        t))))

(defun kv-exists-p (skill-id key)
  "Return T if KEY exists in SKILL-ID's KV store."
  (bt:with-lock-held (*kv-lock*)
    (let ((data-ht (ensure-skill-kv skill-id)))
      (nth-value 1 (gethash key data-ht)))))

(defun kv-list (skill-id &optional prefix)
  "List all keys in SKILL-ID's KV store. If PREFIX is given, filter by it."
  (bt:with-lock-held (*kv-lock*)
    (let ((data-ht (ensure-skill-kv skill-id))
          (keys nil))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (or (null prefix)
                           (and (<= (length prefix) (length k))
                                (string= prefix k :end2 (length prefix))))
                   (push k keys)))
               data-ht)
      (sort keys #'string<))))

;;; --- Admin API ---

(defun kv-clear-skill (skill-id)
  "Wipe all KV data for SKILL-ID (cache and disk)."
  (bt:with-lock-held (*kv-lock*)
    (remhash skill-id *kv-cache*)
    (let ((path (kv-file-path skill-id)))
      (when (probe-file path)
        (delete-file path)))
    (log:info "KV store cleared for skill ~A" skill-id)
    t))

(defun kv-skill-usage (skill-id)
  "Return usage plist for SKILL-ID: (:SKILL-ID id :KEY-COUNT n :TOTAL-BYTES n)."
  (bt:with-lock-held (*kv-lock*)
    (let ((data-ht (ensure-skill-kv skill-id)))
      (list :skill-id skill-id
            :key-count (hash-table-count data-ht)
            :total-bytes (kv-data-total-bytes data-ht)))))

(defun kv-global-usage ()
  "Return a list of per-skill usage plists for all cached skills."
  (bt:with-lock-held (*kv-lock*)
    (let (result)
      (maphash (lambda (skill-id data-ht)
                 (push (list :skill-id skill-id
                             :key-count (hash-table-count data-ht)
                             :total-bytes (kv-data-total-bytes data-ht))
                       result))
               *kv-cache*)
      result)))

;;; --- Lifecycle ---

(defun preload-kv-cache ()
  "Scan ~/.crichton/kv/*.age and load all into cache."
  (bt:with-lock-held (*kv-lock*)
    (let* ((kv-dir (merge-pathnames #p"kv/" crichton/config:*agent-home*))
           (pattern (merge-pathnames (make-pathname :name :wild :type "age") kv-dir))
           (files (directory pattern))
           (count 0))
      (dolist (file files)
        (let ((skill-id (pathname-name file)))
          (handler-case
              (progn
                (setf (gethash skill-id *kv-cache*) (load-skill-kv skill-id))
                (incf count))
            (error (c)
              (log:warn "Failed to load KV store for ~A: ~A" skill-id c)))))
      (log:info "Preloaded ~D KV store~:P" count)
      count)))

(defun flush-all-kv ()
  "Save all cached KV data to disk."
  (bt:with-lock-held (*kv-lock*)
    (let ((count 0))
      (maphash (lambda (skill-id data-ht)
                 (handler-case
                     (progn
                       (save-skill-kv skill-id data-ht)
                       (incf count))
                   (error (c)
                     (log:warn "Failed to flush KV store for ~A: ~A" skill-id c))))
               *kv-cache*)
      (log:info "Flushed ~D KV store~:P" count)
      count)))

;;; --- Reporting ---

(defun kv-usage-report (&key (stream *standard-output*))
  "Print a formatted text report of all skill KV usage."
  (bt:with-lock-held (*kv-lock*)
    (if (zerop (hash-table-count *kv-cache*))
        (format stream "No KV stores loaded.~%")
        (progn
          (format stream "~&KV Store Usage Report~%")
          (format stream "~A~%" (make-string 40 :initial-element #\-))
          (let ((total-keys 0)
                (total-bytes 0))
            (maphash (lambda (skill-id data-ht)
                       (let ((keys (hash-table-count data-ht))
                             (bytes (kv-data-total-bytes data-ht)))
                         (format stream "  ~A: ~D key~:P, ~:D byte~:P~%"
                                 skill-id keys bytes)
                         (incf total-keys keys)
                         (incf total-bytes bytes)))
                     *kv-cache*)
            (format stream "~A~%" (make-string 40 :initial-element #\-))
            (format stream "  Total: ~D key~:P, ~:D byte~:P across ~D skill~:P~%"
                    total-keys total-bytes (hash-table-count *kv-cache*)))))))
