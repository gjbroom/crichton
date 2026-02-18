;;;; runner/skill-context.lisp
;;;;
;;;; Skill execution context: tracks metadata, capabilities, and credential
;;;; resolver for a single running skill instance.
;;;;
;;;; Allows host functions to access skill identity and capability constraints
;;;; without needing to pass context through the wasmtime FFI layer.

(in-package #:crichton/runner)

;;; --- Skill context thread-local storage ---

(defvar *current-skill* nil
  "The currently executing skill context (bound during skill invocation).")

(defclass skill-context ()
  ((id :initarg :id :accessor skill-context-id :initform "" :type string)
   (name :initarg :name :accessor skill-context-name :initform "" :type string)
   (version :initarg :version :accessor skill-context-version :initform "" :type string)
   (signature :initarg :signature :accessor skill-context-signature :initform "" :type string)
   (signed-p :initarg :signed-p :accessor skill-context-signed-p :initform nil :type boolean)
   (http-allowlist :initarg :http-allowlist :accessor skill-context-http-allowlist :initform nil :type list)
   (max-memory-mb :initarg :max-memory-mb :accessor skill-context-max-memory-mb :initform 64 :type integer)
   (max-cpu-seconds :initarg :max-cpu-seconds :accessor skill-context-max-cpu-seconds :initform 30 :type integer)
   (kv-store :initarg :kv-store :accessor skill-context-kv-store :initform (make-hash-table :test #'equal) :type hash-table)
   (secret-resolver :initarg :secret-resolver :accessor skill-context-secret-resolver :initform nil :type (or null function)))
  (:documentation "Metadata and constraints for a running skill instance."))

(defun make-skill-context-from-manifest (manifest &key secret-resolver)
  "Create a skill context from a parsed manifest plist.
   MANIFEST should be the result of crichton/skills:parse-skill-manifest.
   SECRET-RESOLVER is a function (name) -> secret-string or nil."
  (let* ((skill-info (getf manifest :skill))
         (caps (getf manifest :capabilities)))
    (make-instance 'skill-context
      :id (or (getf skill-info :id) (format nil "skill-~A" (random 1000000)))
      :name (getf skill-info :name)
      :version (getf skill-info :version)
      :signature (getf skill-info :signature)
      :signed-p nil  ; Signature verification not yet implemented
      :http-allowlist (getf caps :http-domains)
      :max-memory-mb (or (getf caps :max-memory-mb) 64)
      :max-cpu-seconds (or (getf caps :max-cpu-seconds) 30)
      :secret-resolver secret-resolver)))

(defun call-with-skill-context (context fn)
  "Execute FN with *CURRENT-SKILL* bound to CONTEXT.
   Returns the result of FN."
  (let ((*current-skill* context))
    (funcall fn)))

;;; --- Host function accessors ---

(defun current-skill-context ()
  "Get the currently executing skill context, or signal an error if none."
  (or *current-skill*
      (error "No skill context available (not running inside skill)")))

(defun skill-http-allowlist ()
  "Get the HTTP domain allowlist for the current skill."
  (skill-context-http-allowlist (current-skill-context)))

(defun skill-kv-store ()
  "Get the KV store for the current skill."
  (skill-context-kv-store (current-skill-context)))

(defun skill-resolve-secret (name)
  "Resolve a secret by name using the skill's secret resolver.
   Returns the secret string, or nil if not found or not allowed."
  (let ((resolver (skill-context-secret-resolver (current-skill-context))))
    (when resolver
      (funcall resolver name))))
