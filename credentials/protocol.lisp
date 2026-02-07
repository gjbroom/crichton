;;;; credentials/protocol.lisp
;;;;
;;;; CLOS protocol for credential storage backends.
;;;; Credentials are named secret bundles stored as JSON objects.
;;;; The daemon mediates all access — skills never see raw credentials.
;;;;
;;;; Backend implementations:
;;;;   - age-file: ~/.crichton/credentials/<name>.age (always available)
;;;;   - libsecret: Linux desktop keychain via D-Bus (future)
;;;;   - security-framework: macOS Keychain (future)

(in-package #:crichton/credentials)

;;; --- Conditions ---

(define-condition credential-not-found (error)
  ((name :initarg :name :reader credential-name))
  (:report (lambda (c stream)
             (format stream "Credential not found: ~A" (credential-name c)))))

(define-condition credential-backend-error (error)
  ((cause :initarg :cause :reader credential-cause)
   (backend :initarg :backend :reader credential-backend-name :initform nil))
  (:report (lambda (c stream)
             (format stream "Credential backend~@[ (~A)~] error: ~A"
                     (credential-backend-name c)
                     (credential-cause c)))))

;;; --- Protocol ---

(defclass credential-store ()
  ()
  (:documentation "Base class for credential storage backends."))

(defgeneric cred-put (store name plist &key overwrite)
  (:documentation "Store a credential under NAME.
   PLIST contains key-value pairs (e.g. :token \"abc\" :username \"user\").
   If OVERWRITE is NIL (default) and credential exists, signals an error."))

(defgeneric cred-get (store name)
  (:documentation "Retrieve a credential by NAME.
   Returns a plist of key-value pairs.
   Signals CREDENTIAL-NOT-FOUND if the credential does not exist."))

(defgeneric cred-delete (store name)
  (:documentation "Delete a credential by NAME.
   Returns T if deleted, NIL if it did not exist."))

(defgeneric cred-exists-p (store name)
  (:documentation "Return T if a credential with NAME exists in the store."))

(defgeneric cred-list (store)
  (:documentation "Return a list of all credential names in the store."))

;;; --- Credential name validation ---

(defun valid-credential-name-p (name)
  "Return T if NAME is a valid credential name.
   Valid names: alphanumeric, hyphens, underscores, dots. No path separators."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 128)
       (every (lambda (c)
                (or (alphanumericp c)
                    (member c '(#\- #\_ #\.) :test #'char=)))
              name)))

(defun assert-valid-name (name)
  "Signal an error if NAME is not a valid credential name."
  (unless (valid-credential-name-p name)
    (error "Invalid credential name: ~S (must be alphanumeric, hyphens, underscores, dots; max 128 chars)"
           name)))
