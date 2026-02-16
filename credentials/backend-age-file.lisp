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

;;; --- Protocol implementation ---

(defmethod cred-put ((store age-file-store) name plist &key (overwrite nil))
  (assert-valid-name name)
  (let ((path (cred-file-path store name)))
    (when (and (probe-file path) (not overwrite))
      (error "Credential ~S already exists (use :overwrite t to replace)" name))
    (crichton/crypto:with-secret-bytes (plaintext (crichton/config:plist-to-json-bytes plist))
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
      (crichton/config:json-bytes-to-plist plaintext))))

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
