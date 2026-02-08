;;;; credentials/backend-age-file.lisp
;;;;
;;;; Age-encrypted file backend for credential storage.
;;;; One file per credential: ~/.crichton/credentials/<name>.age
;;;; Plaintext inside: JSON object with string keys.
;;;;
;;;; This is the universal fallback backend — works on any platform
;;;; where the `age` CLI is installed (headless servers, containers, etc.).

(in-package #:crichton/credentials)

(defclass age-file-store (credential-store)
  ((directory :initarg :directory
              :reader store-directory
              :documentation "Directory containing <name>.age files."))
  (:documentation "Credential store backed by age-encrypted files."))

(defun make-age-file-store (&optional dir)
  "Create an age-file credential store.
   DIR defaults to ~/.crichton/credentials/."
  (let ((d (or dir (merge-pathnames #p"credentials/"
                                    crichton/config:*agent-home*))))
    (ensure-directories-exist d)
    #+sbcl (sb-posix:chmod (namestring d) #o700)
    (make-instance 'age-file-store :directory d)))

;;; --- Path helpers ---

(defun cred-file-path (store name)
  "Return the .age file path for credential NAME."
  (merge-pathnames (make-pathname :name name :type "age")
                   (store-directory store)))

;;; --- Serialization ---

(defun plist-to-json-bytes (plist)
  "Serialize a credential plist to JSON bytes.
   Keys are converted from keywords to lowercase strings."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht) v))
    (sb-ext:string-to-octets
     (let ((*print-pretty* nil))
       (with-output-to-string (s)
         (shasht:write-json ht s)))
     :external-format :utf-8)))

(defun safe-intern-keyword (key)
  "Intern KEY as a keyword after validating length and character set.
   Rejects keys that could exhaust the keyword package via untrusted JSON."
  (let ((ukey (string-upcase key)))
    (when (or (> (length ukey) 64)
              (zerop (length ukey))
              (not (every (lambda (c)
                            (or (alpha-char-p c) (digit-char-p c)
                                (char= c #\-) (char= c #\_)))
                          ukey)))
      (error "Invalid JSON key for keyword interning: ~S" key))
    (intern ukey :keyword)))

(defun json-bytes-to-plist (bytes)
  "Deserialize JSON bytes to a credential plist.
   String keys become uppercase keywords after validation."
  (let* ((json-string (sb-ext:octets-to-string bytes :external-format :utf-8))
         (ht (shasht:read-json json-string)))
    (when (hash-table-p ht)
      (let (result)
        (maphash (lambda (k v)
                   (push v result)
                   (push (safe-intern-keyword k) result))
                 ht)
        result))))

;;; --- Protocol implementation ---

(defmethod cred-put ((store age-file-store) name plist &key (overwrite nil))
  (assert-valid-name name)
  (let ((path (cred-file-path store name)))
    (when (and (probe-file path) (not overwrite))
      (error "Credential ~S already exists (use :overwrite t to replace)" name))
    (crichton/crypto:with-secret-bytes (plaintext (plist-to-json-bytes plist))
      (crichton/crypto:encrypt-to-file plaintext path))
    (log:info "Credential stored: ~A" name)
    name))

(defmethod cred-get ((store age-file-store) name)
  (assert-valid-name name)
  (let ((path (cred-file-path store name)))
    (unless (probe-file path)
      (error 'credential-not-found :name name))
    (crichton/crypto:with-secret-bytes (plaintext
                                        (crichton/crypto:decrypt-from-file path))
      (json-bytes-to-plist plaintext))))

(defmethod cred-delete ((store age-file-store) name)
  (assert-valid-name name)
  (let ((path (cred-file-path store name)))
    (when (probe-file path)
      (delete-file path)
      (log:info "Credential deleted: ~A" name)
      t)))

(defmethod cred-exists-p ((store age-file-store) name)
  (assert-valid-name name)
  (and (probe-file (cred-file-path store name)) t))

(defmethod cred-list ((store age-file-store))
  (let ((pattern (merge-pathnames
                  (make-pathname :name :wild :type "age")
                  (store-directory store))))
    (mapcar #'pathname-name (directory pattern))))
