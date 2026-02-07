;;;; skills/manifest.lisp
;;;;
;;;; Skill capability manifest parsing and validation.
;;;; Parses skill.toml files and validates declared capabilities against policy.

(in-package #:crichton/skills)

;;; --- TOML conversion (local reimplementation, same pattern as config/loader) ---

(defun toml-key-to-keyword (key)
  (intern (string-upcase (substitute #\- #\_ (string key)))
          :keyword))

(defun toml-value-to-lisp (value)
  (cond
    ((hash-table-p value) (toml-table-to-plist value))
    ((eq value 'cl-toml:true) t)
    ((eq value 'cl-toml:false) nil)
    ((stringp value) value)
    ((vectorp value) (map 'list #'toml-value-to-lisp value))
    (t value)))

(defun toml-table-to-plist (table)
  (when (hash-table-p table)
    (let (result)
      (maphash (lambda (k v)
                 (let ((key (toml-key-to-keyword k))
                       (val (toml-value-to-lisp v)))
                   (push val result)
                   (push key result)))
               table)
      result)))

;;; --- Manifest parsing ---

(define-condition manifest-error (simple-error)
  ((path :initarg :path :reader manifest-error-path)
   (problems :initarg :problems :reader manifest-error-problems))
  (:report (lambda (c stream)
             (format stream "Manifest error~@[ in ~A~]: ~{~A~^; ~}"
                     (manifest-error-path c)
                     (manifest-error-problems c)))))

(defun parse-skill-manifest (pathname)
  "Parse a skill.toml file at PATHNAME. Returns a plist with :SKILL and :CAPABILITIES.
   Signals MANIFEST-ERROR if required fields are missing."
  (let* ((path (pathname pathname))
         (parsed (handler-case (cl-toml:parse-file path)
                   (error (c)
                     (error 'manifest-error
                            :path (namestring path)
                            :problems (list (format nil "TOML parse failed: ~A" c))
                            :format-control "~A"
                            :format-arguments (list c)))))
         (manifest (toml-table-to-plist parsed)))
    (validate-required-fields manifest path)
    manifest))

(defun validate-required-fields (manifest path)
  (let ((problems nil)
        (skill (getf manifest :skill)))
    (unless skill
      (push "missing [skill] section" problems))
    (when skill
      (unless (getf skill :name)
        (push "missing skill.name" problems))
      (unless (getf skill :version)
        (push "missing skill.version" problems)))
    (when problems
      (error 'manifest-error
             :path (namestring path)
             :problems (nreverse problems)
             :format-control "~{~A~^; ~}"
             :format-arguments (list problems)))))

;;; --- Accessors ---

(defun manifest-skill-name (manifest)
  (getf (getf manifest :skill) :name))

(defun manifest-skill-version (manifest)
  (getf (getf manifest :skill) :version))

(defun manifest-capabilities (manifest)
  (getf manifest :capabilities))

;;; --- Capability validation against policy ---

(defun validate-capabilities (manifest policy)
  "Validate the capabilities declared in MANIFEST against POLICY.
   POLICY is a plist with optional keys:
     :ALLOWED-HTTP-DOMAINS — list of allowed domain strings (nil = none allowed)
     :MAX-MEMORY-MB — integer ceiling
     :MAX-CPU-SECONDS — integer ceiling
   Returns (VALUES T NIL) on success, or (VALUES NIL problems-list) on failure."
  (let ((caps (manifest-capabilities manifest))
        (problems nil))
    (let ((requested-domains (getf caps :http-domains))
          (allowed-domains (getf policy :allowed-http-domains)))
      (dolist (domain requested-domains)
        (unless (member domain allowed-domains :test #'string-equal)
          (push (format nil "HTTP domain ~S not in allowed list" domain) problems))))
    (let ((requested-mem (getf caps :max-memory-mb))
          (max-mem (getf policy :max-memory-mb)))
      (when (and requested-mem max-mem (> requested-mem max-mem))
        (push (format nil "requested ~D MB exceeds policy limit of ~D MB"
                       requested-mem max-mem)
              problems)))
    (let ((requested-cpu (getf caps :max-cpu-seconds))
          (max-cpu (getf policy :max-cpu-seconds)))
      (when (and requested-cpu max-cpu (> requested-cpu max-cpu))
        (push (format nil "requested ~D CPU seconds exceeds policy limit of ~D seconds"
                       requested-cpu max-cpu)
              problems)))
    (if problems
        (values nil (nreverse problems))
        (values t nil))))
