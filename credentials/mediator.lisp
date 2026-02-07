;;;; credentials/mediator.lisp
;;;;
;;;; Credential mediation: the daemon-only API that resolves credentials
;;;; on behalf of skills. Skills never see raw secrets — the daemon
;;;; injects credentials into effects (e.g., HTTP headers) internally.
;;;;
;;;; This module provides:
;;;; 1. A singleton credential store (initialized at daemon start)
;;;; 2. resolve-credential — internal API for daemon code
;;;; 3. Allowlist checking — only declared credentials are resolvable per skill

(in-package #:crichton/credentials)

;;; --- Singleton store ---

(defvar *credential-store* nil
  "The active credential store instance. Set during daemon startup.")

(defun ensure-credential-store ()
  "Ensure the credential store is initialized. Creates age-file backend if needed."
  (unless *credential-store*
    (setf *credential-store* (make-age-file-store)))
  *credential-store*)

;;; --- Daemon-internal resolution API ---

(defun resolve-credential (name &optional field)
  "Resolve a credential by NAME, optionally extracting a single FIELD.
   This is daemon-internal — never exposed to skills via RPC.
   Returns the full plist if FIELD is NIL, or the field value if FIELD is given."
  (let* ((store (ensure-credential-store))
         (plist (cred-get store name)))
    (if field
        (let ((value (getf plist field)))
          (unless value
            (error "Credential ~S has no field ~S" name field))
          value)
        plist)))

(defun resolve-credential-for-skill (skill-name credential-name field
                                      &key (manifest nil))
  "Resolve a credential field for a specific skill, checking the manifest allowlist.
   MANIFEST is the parsed skill manifest plist.
   Returns the field value, or signals an error if not allowed.

   The manifest must declare the credential in [capabilities] secrets:
     [capabilities]
     secrets = [\"github-token\"]"
  (when manifest
    (let* ((caps (getf manifest :capabilities))
           (allowed-secrets (getf caps :secrets)))
      (unless (member credential-name allowed-secrets :test #'string-equal)
        (error "Skill ~S is not allowed to access credential ~S (not in manifest secrets)"
               skill-name credential-name))))
  (log:info "Resolving credential ~A field ~A for skill ~A"
            credential-name field skill-name)
  (resolve-credential credential-name field))

;;; --- CLI convenience ---

(defun store-credential (name plist &key (overwrite nil))
  "Store a credential. Convenience wrapper for CLI/REPL use."
  (let ((store (ensure-credential-store)))
    (cred-put store name plist :overwrite overwrite)))

(defun delete-credential (name)
  "Delete a credential. Convenience wrapper for CLI/REPL use."
  (let ((store (ensure-credential-store)))
    (cred-delete store name)))

(defun list-credentials ()
  "List all stored credential names."
  (let ((store (ensure-credential-store)))
    (cred-list store)))
