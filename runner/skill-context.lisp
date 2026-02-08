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

(defstruct skill-context
  "Metadata and constraints for a running skill instance."
  (id "" :type string)              ; Skill identifier
  (name "" :type string)            ; Skill name (from manifest)
  (version "" :type string)         ; Skill version
  (signature "" :type string)       ; Ed25519 signature (if signed)
  (signed-p nil :type boolean)      ; T if signature was verified
  (http-allowlist nil :type list)   ; List of allowed HTTP domains
  (max-memory-mb 64 :type integer)  ; Memory limit
  (max-cpu-seconds 30 :type integer) ; CPU time limit
  (kv-store (make-hash-table :test #'equal) :type hash-table) ; Skill's KV store
  (secret-resolver nil :type (or null function))) ; Function to resolve secrets

(defun make-skill-context-from-manifest (manifest &key secret-resolver)
  "Create a skill context from a parsed manifest plist.
   MANIFEST should be the result of crichton/skills:parse-skill-manifest.
   SECRET-RESOLVER is a function (name) -> secret-string or nil."
  (let* ((skill-info (getf manifest :skill))
         (caps (getf manifest :capabilities)))
    (make-skill-context
     :id (or (getf skill-info :id) (format nil "skill-~A" (random 1000000)))
     :name (getf skill-info :name)
     :version (getf skill-info :version)
     :signature (getf skill-info :signature)
     :signed-p nil  ; Will be set to T if verification passes
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
