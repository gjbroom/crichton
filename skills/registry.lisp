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
          (handler-case
              (let* ((manifest (parse-skill-manifest manifest-path))
                     (skill-info (getf manifest :skill))
                     (name (getf skill-info :name))
                     (version (getf skill-info :version))
                     (description (getf skill-info :description))
                     (author (getf skill-info :author))
                     (provides-info (getf manifest :provides))
                     (functions (getf provides-info :functions))
                     (entry-point (if (and functions (consp functions))
                                      (first functions)
                                      "main")))
                (when name
                  (let ((entry (make-instance 'skill-entry
                                :name name
                                :version (or version "0.0.0")
                                :description (or description "")
                                :author (or author "")
                                :path subdir-path
                                :manifest manifest
                                :loaded-p nil
                                :wasm-bytes nil
                                :entry-point entry-point)))
                    (setf (gethash name *skill-registry*) entry)
                    (incf count)
                    ;; Auto-register as schedulable action
                    (register-skill-as-action entry)
                    (log:info "Discovered skill: ~A v~A (~A)"
                              name version (namestring subdir-path)))))
            (error (c)
              (log:warn "Failed to load skill manifest ~A: ~A"
                        (namestring manifest-path) c))))))

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

;;; --- Invocation ---

(defun invoke-skill (name &key (entry-point nil))
  "Load (if not loaded) and run the WASM skill with run-wasm-bytes-with-host-fns.
   Sets up skill context from manifest.  The ENTRY-POINT keyword allows you to
   override the default (found in the manifest).  Returns the WASM
   result (integer from main)."
  (let* ((entry (load-skill name))
         (entry-point (or entry-point (skill-entry-entry-point entry)))
         (manifest (skill-entry-manifest entry))
         (context (crichton/runner:make-skill-context-from-manifest
                   manifest
                   :secret-resolver #'crichton/credentials:resolve-credential-for-skill)))

    (log:info "Invoking skill ~A (entry point: ~A)" name entry-point)

    (crichton/runner:call-with-skill-context context
      (lambda ()
        (handler-case
            (let ((result (crichton/wasm:run-wasm-bytes-with-host-fns
                           (skill-entry-wasm-bytes entry)
                           entry-point
                           :args nil
                           :nresults 1)))
              (log:info "Skill ~A returned: ~A" name result)
              result)
          (error (c)
            (log:error "Skill ~A failed: ~A" name c)
            (error c)))))))

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
