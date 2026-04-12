;;;; agent/tools.lisp
;;;;
;;;; Tool registry, define-tool macro, and shared helpers.
;;;; Tool definitions live in:
;;;;   tools-system.lisp   — weather, system-info, scheduler, time, ephemeris, RSS, battery, usage
;;;;   tools-ops.lisp      — daemon logs, Amp CLI orchestration
;;;;   tools-data.lisp     — memory, raindrop, skills, orgmode, books, pushover, github, git, hoobs
;;;;   tools-register.lisp — channel allowlist + register-all-tools (must load last)

(in-package #:crichton/agent)

;;; --- Tool registry ---

(defclass agent-tool ()
  ((name :initarg :name
         :accessor agent-tool-name
         :initform ""
         :type string)
   (description :initarg :description
                :accessor agent-tool-description
                :initform ""
                :type string)
   (input-schema :initarg :input-schema
                 :accessor agent-tool-input-schema
                 :initform nil)
   (handler :initarg :handler
            :accessor agent-tool-handler
            :initform nil
            :type (or null function))))

(defun make-agent-tool (&key (name "") (description "") input-schema handler)
  (make-instance 'agent-tool :name name :description description
                 :input-schema input-schema :handler handler))

(defvar *agent-tools* (make-hash-table :test #'equal)
  "Registry of available agent tools, keyed by name.")

(defun register-tool (name description input-schema handler)
  "Register a tool for agent use."
  (setf (gethash name *agent-tools*)
        (make-agent-tool :name name
                          :description description
                          :input-schema input-schema
                          :handler handler)))

(defun get-tool (name)
  "Look up a registered tool by name."
  (gethash name *agent-tools*))

(defun all-tool-defs ()
  "Return a list of tool definition plists for the Anthropic API."
  (let (tools)
    (maphash (lambda (name tool)
               (declare (ignore name))
               (push (list :name (agent-tool-name tool)
                           :description (agent-tool-description tool)
                           :input-schema (agent-tool-input-schema tool))
                     tools))
             *agent-tools*)
    (nreverse tools)))

(defun dispatch-tool (name input)
  "Call the handler for tool NAME with INPUT (a hash-table from the LLM).
   Returns a result string. On error, returns an error description string.
   Offers :USE-VALUE and :REPORT-ERROR restarts.
   Writes an audit event for every tool call."
  (let ((tool (get-tool name)))
    (unless tool
      (return-from dispatch-tool
        (format nil "Error: unknown tool ~S" name)))
    ;; Audit every tool invocation
    (let ((fields (make-hash-table :test #'equal)))
      (setf (gethash "tool" fields) name)
      (crichton/logging:write-audit-event "agent.tool.call" fields))
    (handler-bind
        ((error (lambda (c)
                  (let ((r (find-restart :report-error)))
                    (when r
                      (invoke-restart r
                        (format nil "Error executing ~A: ~A" name c)))))))
      (restart-case
          (funcall (agent-tool-handler tool) input)
        (:use-value (value)
          :report (lambda (s) (format s "Supply a result for tool ~A" name))
          :interactive (lambda ()
                         (format *query-io* "~&Tool result string: ")
                         (list (read-line *query-io*)))
          value)
        (:report-error (message)
          :report (lambda (s) (format s "Return error message to LLM for ~A" name))
          message)))))

;;; --- Helper: hash-table key access with default ---

(defun hget (ht key &optional default)
  "Get KEY from hash-table HT, returning DEFAULT if missing."
  (if (and ht (hash-table-p ht))
      (multiple-value-bind (val found) (gethash key ht)
        (if found val default))
      default))

;;; --- JSON Schema helpers ---

(defun make-json-schema (&rest pairs)
  "Build a JSON Schema hash-table from keyword pairs.
   Convenience for building input_schema objects."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash (substitute #\_ #\-
                              (string-downcase (symbol-name k)))
                            ht)
                   v))
    ht))

(defun make-properties (&rest prop-defs)
  "Build a JSON Schema properties hash-table.
   Each PROP-DEF is (name type description &key enum).
   Returns (values properties-ht required-list)."
  (let ((props (make-hash-table :test #'equal))
        (required nil))
    (loop for def in prop-defs
          do (destructuring-bind (name type desc &key enum required-p) def
               (let ((prop (make-hash-table :test #'equal)))
                 (setf (gethash "type" prop) type
                       (gethash "description" prop) desc)
                 (when enum
                   (setf (gethash "enum" prop) (coerce enum 'vector)))
                 (setf (gethash name props) prop)
                 (when required-p
                   (push name required)))))
    (values props (coerce (nreverse required) 'vector))))

;;; --- define-tool macro ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-to-underscored (sym)
    "Convert a Lisp symbol to an underscored lowercase string.
     E.g. SYSTEM-INFO → \"system_info\"."
    (substitute #\_ #\- (string-downcase (symbol-name sym))))

  (defun extract-declarations (body)
    "Separate leading DECLARE forms from BODY.
     Returns (values declarations remaining-body)."
    (loop for (form . rest) on body
          while (and (consp form) (eq (car form) 'declare))
          collect form into decls
          finally (return (values decls (cons form rest)))))

  (defun register-fn-name (name)
    "Derive the REGISTER-<NAME>-TOOL function name symbol, always interned
     in the CRICHTON/AGENT package."
    (intern (format nil "~A~A~A"
                    (string '#:register-)
                    (string-upcase (symbol-name name))
                    (string '#:-tool))
            (find-package '#:crichton/agent)))) ;; end eval-when

(defmacro define-tool (name (&key description (tool-name nil tool-name-p))
                       params &body body)
  "Define a tool registration function.  NAME is a symbol used to derive the
registration function name (REGISTER-<NAME>-TOOL) and, unless :TOOL-NAME is
supplied, the tool name string (hyphens become underscores).

PARAMS is a list of parameter specs:
  (var-name type-string description &key required-p enum default)
Each VAR-NAME is bound via %HGET in the handler body.

BODY is wrapped in (BLOCK HANDLER ...) so RETURN-FROM HANDLER is available.
Leading DECLARE forms in BODY are placed before the BLOCK."
  (let* ((tool-str (if tool-name-p
                       tool-name
                       (symbol-to-underscored name)))
         (register-fn (register-fn-name name)))
    ;; Build the make-properties argument forms and the let-bindings
    (let ((prop-forms
            (loop for spec in params
                  collect
                  (destructuring-bind (var type desc &key required-p enum
                                                          default)
                      spec
                    (declare (ignore default))
                    (let ((name-str (symbol-to-underscored var)))
                      `(list ,name-str ,type ,desc
                             ,@(when required-p '(:required-p t))
                             ,@(when enum `(:enum (list ,@enum))))))))
          (let-bindings
            (loop for spec in params
                  collect
                  (destructuring-bind (var type desc &key required-p enum
                                                          default)
                      spec
                    (declare (ignore type desc required-p enum))
                    (let ((name-str (symbol-to-underscored var)))
                      (if default
                          `(,var (hget input ,name-str ,default))
                          `(,var (hget input ,name-str))))))))
      (multiple-value-bind (declarations real-body)
          (extract-declarations body)
        (if params
            ;; --- With parameters ---
            `(defun ,register-fn ()
               (multiple-value-bind (props required)
                   (make-properties ,@prop-forms)
                 (register-tool
                  ,tool-str
                  ,description
                  (make-json-schema :type "object"
                                    :properties props
                                    :required required)
                  (lambda (input)
                    (let ,let-bindings
                      ,@declarations
                      (block handler
                        ,@real-body))))))
            ;; --- No parameters ---
            `(defun ,register-fn ()
               (register-tool
                ,tool-str
                ,description
                (make-json-schema :type "object"
                                  :properties (make-hash-table :test #'equal))
                (lambda (input)
                  (declare (ignore input))
                  (block handler
                    ,@real-body)))))))))
