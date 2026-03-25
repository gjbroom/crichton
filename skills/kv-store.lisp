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

;;; --- Path helpers ---

(defun kv-dir-path ()
  "Return the path to the KV store directory: ~/.crichton/kv/"
  (merge-pathnames #p"kv/" crichton/config:*agent-home*))

(defun kv-file-path (skill-id)
  "Return the .age file path for SKILL-ID: ~/.crichton/kv/<skill-id>.age"
  (merge-pathnames (make-pathname :name skill-id :type "age")
                   (kv-dir-path)))

;;; --- JSON serialization ---

(defun kv-to-json-string (skill-id data-ht)
  "Serialize DATA-HT into JSON envelope string with version and metadata."
  (let ((envelope (make-hash-table :test #'equal)))
    (setf (gethash "version" envelope) 1
          (gethash "skill_id" envelope) skill-id
          (gethash "updated_at" envelope) (crichton/config:iso8601-now)
          (gethash "data" envelope) data-ht)
    (unless (gethash "created_at" envelope)
      (setf (gethash "created_at" envelope) (crichton/config:iso8601-now)))
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
    (crichton/config:delete-file-if-exists (kv-file-path skill-id))
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
          (handler-bind
              ((error (lambda (c)
                        (log:warn "Failed to load KV store for ~A: ~A" skill-id c)
                        (let ((r (find-restart :skip-store)))
                          (when r (invoke-restart r))))))
            (restart-case
                (progn
                  (setf (gethash skill-id *kv-cache*) (load-skill-kv skill-id))
                  (incf count))
              (:skip-store ()
                :report (lambda (s) (format s "Skip KV store for ~A" skill-id)))
              (:use-empty-store ()
                :report (lambda (s) (format s "Use empty store for ~A" skill-id))
                (setf (gethash skill-id *kv-cache*)
                      (make-hash-table :test #'equal))
                (incf count))))))
      (log:info "Preloaded ~D KV store~:P" count)
      count)))

(defun flush-all-kv ()
  "Save all cached KV data to disk."
  (bt:with-lock-held (*kv-lock*)
    (let ((count 0))
      (maphash (lambda (skill-id data-ht)
                 (handler-bind
                     ((error (lambda (c)
                               (log:warn "Failed to flush KV store for ~A: ~A"
                                         skill-id c)
                               (let ((r (find-restart :skip-store)))
                                 (when r (invoke-restart r))))))
                   (restart-case
                       (progn
                         (save-skill-kv skill-id data-ht)
                         (incf count))
                     (:skip-store ()
                       :report (lambda (s) (format s "Skip flushing ~A" skill-id)))
                     (:retry ()
                       :report (lambda (s) (format s "Retry flushing ~A" skill-id))
                       (save-skill-kv skill-id data-ht)
                       (incf count)))))
               *kv-cache*)
      (log:info "Flushed ~D KV store~:P" count)
      count)))

;;; --- Operational tools ---

(defun backup-kv-store (backup-dir)
  "Flush all cached KV data and copy every .age file to BACKUP-DIR.
   BACKUP-DIR is created if it does not exist.
   Returns the count of files copied."
  (ensure-directories-exist backup-dir)
  (bt:with-lock-held (*kv-lock*)
    ;; Flush any dirty cache entries first so backup captures current state.
    (maphash (lambda (skill-id data-ht)
               (handler-case (save-skill-kv skill-id data-ht)
                 (error (c)
                   (log:warn "backup-kv-store: failed to flush ~A before backup: ~A"
                             skill-id c))))
             *kv-cache*)
    (let* ((pattern (merge-pathnames (make-pathname :name :wild :type "age")
                                     (kv-dir-path)))
           (files (directory pattern))
           (count 0))
      (dolist (file files)
        (let ((dest (merge-pathnames (file-namestring file) backup-dir)))
          (uiop:copy-file file dest)
          (incf count)))
      (log:info "backup-kv-store: backed up ~D KV store~:P to ~A" count backup-dir)
      count)))

(defun restore-kv-backup (backup-dir)
  "Copy all .age files from BACKUP-DIR into the live KV directory,
   then clear the in-memory cache so they are re-read on next access.
   Returns the count of files restored."
  (bt:with-lock-held (*kv-lock*)
    (let* ((kv-dir (kv-dir-path))
           (pattern (merge-pathnames (make-pathname :name :wild :type "age")
                                     backup-dir))
           (files (directory pattern))
           (count 0))
      (ensure-directories-exist kv-dir)
      (dolist (file files)
        (let ((dest (merge-pathnames (file-namestring file) kv-dir)))
          (uiop:copy-file file dest)
          (incf count)))
      (clrhash *kv-cache*)
      (log:info "restore-kv-backup: restored ~D KV store~:P from ~A" count backup-dir)
      count)))

(defun check-kv-integrity ()
  "Decrypt and parse every .age file in the KV directory.
   Returns a list of result plists, one per file:
     (:SKILL-ID id :STATUS :OK    :ERROR nil)
     (:SKILL-ID id :STATUS :CORRUPT :ERROR \"message\")"
  (let* ((pattern (merge-pathnames (make-pathname :name :wild :type "age")
                                   (kv-dir-path)))
         (files (directory pattern))
         results)
    (dolist (file files)
      (let ((skill-id (pathname-name file)))
        (push
         (handler-case
             (progn
               (load-skill-kv skill-id)
               (list :skill-id skill-id :status :ok :error nil))
           (error (c)
             (list :skill-id skill-id :status :corrupt
                   :error (format nil "~A" c))))
         results)))
    (nreverse results)))

(defun repair-corrupt-kv (skill-id)
  "Rename the on-disk KV file for SKILL-ID to <skill-id>.corrupt.age and
   evict it from the in-memory cache.  Use this after CHECK-KV-INTEGRITY
   identifies a corrupt store.
   Returns the new path, or NIL if no file existed."
  (bt:with-lock-held (*kv-lock*)
    (remhash skill-id *kv-cache*)
    (let ((path (kv-file-path skill-id)))
      (when (probe-file path)
        (let ((new-path (make-pathname :name (format nil "~A.corrupt" skill-id)
                                       :type "age"
                                       :defaults path)))
          (rename-file path new-path)
          (log:warn "repair-corrupt-kv: renamed corrupt store for ~A to ~A"
                    skill-id new-path)
          new-path)))))

(defun kv-health-check (&key (stream *standard-output*))
  "Print a formatted health report for the KV store subsystem to STREAM.
   Checks directory accessibility, runs CHECK-KV-INTEGRITY, reports quotas
   and per-skill usage.  Returns T when no corruption is found, NIL otherwise."
  (let* ((kv-dir (kv-dir-path))
         (dir-exists (ignore-errors
                       (ensure-directories-exist kv-dir)
                       (probe-file kv-dir)))
         (integrity (check-kv-integrity))
         (ok-count      (count :ok      integrity :key (lambda (r) (getf r :status))))
         (corrupt-count (count :corrupt integrity :key (lambda (r) (getf r :status)))))
    (format stream "~&KV Store Health Check~%")
    (format stream "~A~%" (make-string 40 :initial-element #\-))
    (format stream "  Directory  : ~A~%" kv-dir)
    (format stream "  Accessible : ~:[NO~;yes~]~%" dir-exists)
    (format stream "  Files      : ~D (~D OK, ~D corrupt)~%"
            (length integrity) ok-count corrupt-count)
    (when (plusp corrupt-count)
      (format stream "~%  Corrupt files:~%")
      (dolist (r integrity)
        (when (eq (getf r :status) :corrupt)
          (format stream "    ~A: ~A~%" (getf r :skill-id) (getf r :error)))))
    (format stream "~A~%" (make-string 40 :initial-element #\-))
    (format stream "  Quotas     : max-keys=~D, max-value=~D B, max-total=~D B~%"
            (kv-max-keys) (kv-max-value-bytes) (kv-max-total-bytes))
    (format stream "~%")
    (kv-usage-report :stream stream)
    (zerop corrupt-count)))

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
