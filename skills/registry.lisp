;;;; skills/registry.lisp
;;;;
;;;; Skill registry: discovery, loading, and invocation of external WASM skills.
;;;; Scans ~/.crichton/skills/ for skill.toml manifests and loads WASM binaries on demand.

(in-package #:crichton/skills)

;;; --- Data structures ---

(defclass skill-entry ()
  ((name :initarg :name
         :initform ""
         :accessor skill-entry-name
         :type string
         :documentation "Skill name from manifest skill.name")
   (version :initarg :version
            :initform ""
            :accessor skill-entry-version
            :type string
            :documentation "Version from manifest skill.version")
   (description :initarg :description
                :initform ""
                :accessor skill-entry-description
                :type string
                :documentation "Description from manifest skill.description")
   (author :initarg :author
           :initform ""
           :accessor skill-entry-author
           :type string
           :documentation "Author from manifest skill.author")
   (path :initarg :path
         :initform nil
         :accessor skill-entry-path
         :type (or null pathname)
         :documentation "Directory path")
   (manifest :initarg :manifest
             :initform nil
             :accessor skill-entry-manifest
             :documentation "Full parsed manifest plist")
   (loaded-p :initarg :loaded-p
             :initform nil
             :accessor skill-entry-loaded-p
             :type boolean
             :documentation "T if WASM bytes cached")
   (wasm-bytes :initarg :wasm-bytes
               :initform nil
               :accessor skill-entry-wasm-bytes
               :documentation "Cached byte vector (loaded on demand)")
   (entry-point :initarg :entry-point
                :initform "main"
                :accessor skill-entry-entry-point
                :type string
                :documentation "Default export to call"))
  (:documentation "Registry entry for a discovered WASM skill"))

(defvar *skill-registry* (make-hash-table :test #'equal)
  "Registry of discovered skills, keyed by skill name.")

;;; --- Skills directory ---

(defun skills-directory ()
  "Return the pathname for ~/.crichton/skills/ using *agent-home*."
  (merge-pathnames "skills/" crichton/config:*agent-home*))

;;; --- Discovery ---

(defun register-skill-from-manifest (manifest subdir-path)
  "Parse a skill manifest plist and register it in *skill-registry*.
   Returns the skill-entry if successful, NIL if the manifest lacks a name."
  (let* ((skill-info (getf manifest :skill))
         (name (getf skill-info :name)))
    (unless name
      (return-from register-skill-from-manifest nil))
    (let* ((version (or (getf skill-info :version) "0.0.0"))
           (provides-info (getf manifest :provides))
           (functions (getf provides-info :functions))
           (entry (make-instance 'skill-entry
                    :name name
                    :version version
                    :description (or (getf skill-info :description) "")
                    :author (or (getf skill-info :author) "")
                    :path subdir-path
                    :manifest manifest
                    :loaded-p nil
                    :wasm-bytes nil
                    :entry-point (if (and functions (consp functions))
                                     (first functions)
                                     "main"))))
      (setf (gethash name *skill-registry*) entry)
      (register-skill-as-action entry)
      (log:info "Discovered skill: ~A v~A (~A)"
                name version (namestring subdir-path))
      entry)))

(defun discover-skills ()
  "Scan the skills directory for subdirectories containing skill.toml.
   Parse each manifest and populate *skill-registry*.
   Returns the count of skills found."
  (let ((skills-dir (skills-directory))
        (count 0))
    (unless (probe-file skills-dir)
      (log:info "Skills directory does not exist: ~A" (namestring skills-dir))
      (return-from discover-skills 0))
    (dolist (subdir-path (uiop:subdirectories skills-dir))
      (let ((manifest-path (merge-pathnames "skill.toml" subdir-path)))
        (when (probe-file manifest-path)
          (handler-bind
              ((error (lambda (c)
                        (log:warn "Failed to load skill manifest ~A: ~A"
                                  (namestring manifest-path) c)
                        (let ((r (find-restart :skip-skill)))
                          (when r (invoke-restart r))))))
            (restart-case
                (let ((manifest (parse-skill-manifest manifest-path)))
                  (when (register-skill-from-manifest manifest subdir-path)
                    (incf count)))
              (:skip-skill ()
                :report (lambda (s)
                          (format s "Skip skill manifest ~A"
                                  (namestring manifest-path)))))))))
    (log:info "Discovered ~D skill~:P" count)
    count))

;;; --- Listing ---

(defun list-skills ()
  "Return a list of skill-entry snapshot plists (name, version, description, loaded-p).
   Does NOT include wasm-bytes."
  (let (result)
    (maphash (lambda (name entry)
               (declare (ignore name))
               (push (list :name (skill-entry-name entry)
                           :version (skill-entry-version entry)
                           :description (skill-entry-description entry)
                           :author (skill-entry-author entry)
                           :loaded-p (skill-entry-loaded-p entry)
                           :path (namestring (skill-entry-path entry)))
                     result))
             *skill-registry*)
    (sort result #'string< :key (lambda (x) (getf x :name)))))

;;; --- Skill info ---

(defun skill-info (name)
  "Return full skill-entry plist for a specific skill, or NIL if not found."
  (let ((entry (gethash name *skill-registry*)))
    (when entry
      (list :name (skill-entry-name entry)
            :version (skill-entry-version entry)
            :description (skill-entry-description entry)
            :author (skill-entry-author entry)
            :path (namestring (skill-entry-path entry))
            :loaded-p (skill-entry-loaded-p entry)
            :entry-point (skill-entry-entry-point entry)
            :manifest (skill-entry-manifest entry)))))

;;; --- Loading ---

(defun load-skill (name)
  "Load a skill's WASM bytes into memory.
   Looks for skill.wasm first. If not found, looks for skill.wat and compiles via wat-to-wasm.
   Caches in skill-entry-wasm-bytes and sets loaded-p to T.
   Returns the skill-entry.
   Signals error if skill not found in registry."
  (let ((entry (gethash name *skill-registry*)))
    (unless entry
      (error "Skill ~S not found in registry" name))

    ;; Already loaded?
    (when (skill-entry-loaded-p entry)
      (return-from load-skill entry))

    (let* ((manifest (skill-entry-manifest entry))
           (metadata (getf manifest :metadata))
           (wasm-file (or (getf metadata :wasm-file) "skill.wasm"))
           (skill-dir (skill-entry-path entry))
           (wasm-path (merge-pathnames wasm-file skill-dir))
           (wat-path (merge-pathnames "skill.wat" skill-dir))
           (wasm-bytes nil))

      (cond
        ;; Try loading precompiled WASM
        ((probe-file wasm-path)
         (log:info "Loading WASM binary: ~A" (namestring wasm-path))
         (setf wasm-bytes (crichton/crypto:read-file-bytes wasm-path)))

        ;; Try compiling from WAT
        ((probe-file wat-path)
         (log:info "Compiling WAT to WASM: ~A" (namestring wat-path))
         (let ((wat-string (uiop:read-file-string wat-path)))
           (crichton/wasm:ensure-wasmtime-loaded)
           (multiple-value-bind (wasm-data wasm-size wasm-vec)
               (crichton/wasm:wat-to-wasm wat-string)
             (unwind-protect
                  (progn
                    ;; Copy foreign memory to Lisp byte vector
                    (setf wasm-bytes (make-array wasm-size :element-type '(unsigned-byte 8)))
                    (loop for i below wasm-size
                          do (setf (aref wasm-bytes i)
                                   (cffi:mem-aref wasm-data :uint8 i))))
               ;; Clean up foreign memory
               (when wasm-vec
                 (cffi:foreign-funcall "wasm_byte_vec_delete"
                                       :pointer wasm-vec
                                       :void))))))

        (t
         (error "Skill ~S has no skill.wasm or skill.wat file" name)))

      ;; Cache and mark loaded
      (setf (skill-entry-wasm-bytes entry) wasm-bytes
            (skill-entry-loaded-p entry) t)

      (log:info "Loaded skill ~A (~D bytes)" name (length wasm-bytes))
      entry)))

;;; --- ABI detection ---

(defvar *max-skill-json-input-bytes* (* 4 1024 1024)
  "Maximum serialized JSON input size in bytes for skill invocation (default 4MB).")

(defun resolve-effective-abi (abi params manifest entry-point)
  "Resolve the effective ABI (:JSON or :I32) from the caller's ABI spec,
   the presence of PARAMS, and the manifest's function declaration."
  (case abi
    (:json :json)
    (:i32  :i32)
    (otherwise
     (cond
       (params :json)
       ((eq :json (manifest-function-abi manifest entry-point)) :json)
       (t :i32)))))

(defun check-json-input-size (name params)
  "Serialize PARAMS to JSON and signal an error if it exceeds the size limit.
   Returns the params (defaulting to an empty hash-table when NIL)."
  (let* ((params (or params (make-hash-table :test #'equal)))
         (json-size (length (sb-ext:string-to-octets
                             (with-output-to-string (s)
                               (shasht:write-json params s))
                             :external-format :utf-8))))
    (when (> json-size *max-skill-json-input-bytes*)
      (error "Skill ~S JSON input is ~D bytes, exceeding ~D byte limit"
             name json-size *max-skill-json-input-bytes*))
    params))

(defparameter *wasm-execution-timeout* 30
  "Maximum seconds allowed for a single WASM skill execution.")

(defun dispatch-skill-call (entry entry-point effective-abi params)
  "Execute the WASM skill call using the resolved ABI.
   Returns the skill's result value."
  (let ((name (skill-entry-name entry)))
    (ecase effective-abi
      (:json
       (let* ((params (check-json-input-size name params))
              (result (crichton/wasm:run-wasm-bytes-json-call
                       (skill-entry-wasm-bytes entry)
                       entry-point
                       params)))
         (log:info "Skill ~A returned: ~A" name result)
         result))
      (:i32
       (let ((result (crichton/wasm:run-wasm-bytes-with-host-fns
                      (skill-entry-wasm-bytes entry)
                      entry-point
                      :args nil
                      :nresults 1)))
         (log:info "Skill ~A returned: ~A" name result)
         result)))))

;;; --- Invocation ---

(defun invoke-skill (name &key entry-point params (abi :auto))
  "Load (if not loaded) and run the WASM skill.
   Sets up skill context from manifest.

   ENTRY-POINT — override the default export (found in the manifest).
   PARAMS      — Lisp data to pass as JSON input (plist, hash-table, etc.).
                 When non-nil, the JSON ABI is used automatically.
   ABI         — :AUTO (default), :JSON, or :I32.
                 :AUTO selects JSON ABI when PARAMS is non-nil or the manifest
                 declares abi=\"json\" for the entry-point; otherwise i32.

   Returns: parsed JSON result (for JSON ABI) or integer (for i32 ABI)."
  (let* ((entry (load-skill name))
         (entry-point (or entry-point (skill-entry-entry-point entry)))
         (manifest (skill-entry-manifest entry))
         (effective-abi (resolve-effective-abi abi params manifest entry-point))
         (context (crichton/runner:make-skill-context-from-manifest
                   manifest
                   :secret-resolver #'crichton/credentials:resolve-credential-for-skill)))
    (when (and params (eq effective-abi :i32))
      (error "Skill ~S entry-point ~S uses i32 ABI but :params were provided. ~
              Declare abi=\"json\" in the manifest or pass :abi :json."
             name entry-point))
    (log:info "Invoking skill ~A (entry point: ~A, abi: ~A)"
              name entry-point effective-abi)
    (crichton/runner:call-with-skill-context context
      (lambda ()
        (handler-bind ((error (lambda (c)
                                (log:error "Skill ~A failed: ~A" name c))))
          (with-timeout (*wasm-execution-timeout*
                         :error-message (format nil "Skill ~A execution timed out after ~As"
                                                name *wasm-execution-timeout*))
            (dispatch-skill-call entry entry-point effective-abi params)))))))

;;; --- Unloading ---

(defun unload-skill (name)
  "Clear cached wasm-bytes and set loaded-p to NIL.
   Returns T if skill was found and unloaded, NIL otherwise."
  (let ((entry (gethash name *skill-registry*)))
    (when entry
      (setf (skill-entry-wasm-bytes entry) nil
            (skill-entry-loaded-p entry) nil)
      (log:info "Unloaded skill ~A" name)
      t)))

;;; --- Reporting ---

(defun skill-report (&key (stream *standard-output*))
  "Human-readable report of all skills."
  (let ((skills (list-skills)))
    (if (null skills)
        (format stream "No skills discovered.~%")
        (progn
          (format stream "Discovered ~D skill~:P:~%~%" (length skills))
          (dolist (skill skills)
            (format stream "~A v~A~%" (getf skill :name) (getf skill :version))
            (format stream "  Author: ~A~%" (getf skill :author))
            (format stream "  Description: ~A~%" (getf skill :description))
            (format stream "  Path: ~A~%" (getf skill :path))
            (format stream "  Loaded: ~A~%~%" (if (getf skill :loaded-p) "Yes" "No")))))))

;;; --- Schedulable action bridge ---

(defun register-skill-as-action (skill-entry)
  "Register a discovered skill as a schedulable action.
   Action name format: skill:<skill-name>"
  (let ((action-name (format nil "skill:~A" (skill-entry-name skill-entry)))
        (description (or (skill-entry-description skill-entry)
                         (format nil "Run the ~A skill" (skill-entry-name skill-entry)))))
    (register-schedulable-action action-name description
      (lambda ()
        (handler-case
            (let ((result (invoke-skill (skill-entry-name skill-entry))))
              (log:info "Scheduled skill ~A returned: ~A"
                        (skill-entry-name skill-entry) result))
          (error (c)
            (log:warn "Scheduled skill ~A failed: ~A"
                      (skill-entry-name skill-entry) c)))))))
