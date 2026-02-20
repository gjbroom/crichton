;;;; skills/pipeline.lisp
;;;;
;;;; Skill pipeline executor: chains skill invocations so the output
;;;; of one step can feed into the next.  Supports both built-in Lisp
;;;; skills and external WASM skills.
;;;;
;;;; Pipelines are linear sequences of steps.  Each step produces a
;;;; result stored in a results table keyed by step id.  Subsequent
;;;; steps can reference earlier results via {"ref": "step-id.key.subkey"}.

(in-package #:crichton/skills)

;;; --- Pipeline builtin registry ---

(defvar *pipeline-builtins* (make-hash-table :test #'equal)
  "Registry of built-in functions available as pipeline steps.")

(defun register-pipeline-builtin (name fn)
  "Register FN as a built-in pipeline step callable by NAME."
  (setf (gethash name *pipeline-builtins*) fn))

(defun pipeline-builtin (name)
  "Look up a registered pipeline built-in by NAME."
  (gethash name *pipeline-builtins*))

;;; --- Default builtins ---

(defun register-default-pipeline-builtins ()
  "Register the standard built-in skills as pipeline steps."
  ;; rss-fetch: params {"url": "...", "max_items": 50}
  (register-pipeline-builtin "rss_fetch"
    (lambda (params)
      (rss-fetch (gethash "url" params)
                 :max-items (or (gethash "max_items" params) 50))))
  ;; rss-check: params {"url": "...", "max_items": 50}
  (register-pipeline-builtin "rss_check"
    (lambda (params)
      (rss-check (gethash "url" params)
                 :max-items (or (gethash "max_items" params) 50))))
  ;; weather: params {"city": "Victoria"}
  (register-pipeline-builtin "weather"
    (lambda (params)
      (weather-conditions :city (gethash "city" params)))))

;;; --- Reference resolution ---

(defun ref-object-p (obj)
  "Return the ref path string if OBJ is a reference object, or NIL."
  (when (and (hash-table-p obj)
             (= 1 (hash-table-count obj))
             (gethash "ref" obj))
    (gethash "ref" obj)))

(defun resolve-ref-path (path results)
  "Resolve a dotted reference path like 'fetch.items' against RESULTS.
   RESULTS is a hash-table of step-id → step-output.
   Path segments navigate into hash-tables (string keys), plists (keyword keys),
   and vectors/lists (numeric index)."
  (let* ((segments (cl-ppcre:split "\\." path))
         (step-id (first segments))
         (value (gethash step-id results)))
    (unless value
      (error "Pipeline ref ~S: step ~S not found in results" path step-id))
    (dolist (seg (rest segments) value)
      (setf value
            (cond
              ;; numeric index for vectors/lists
              ((every #'digit-char-p seg)
               (let ((idx (parse-integer seg)))
                 (etypecase value
                   (vector (aref value idx))
                   (list (nth idx value)))))
              ;; hash-table: try string key
              ((hash-table-p value)
               (multiple-value-bind (v found) (gethash seg value)
                 (if found v
                     (error "Pipeline ref ~S: key ~S not found in hash-table"
                            path seg))))
              ;; plist: try keyword
              ((and (listp value) (keywordp (first value)))
               (let ((key (intern (string-upcase
                                   (substitute #\- #\_ seg))
                                  :keyword)))
                 (or (getf value key)
                     (error "Pipeline ref ~S: key ~S not found in plist"
                            path seg))))
              (t
               (error "Pipeline ref ~S: cannot navigate into ~S with key ~S"
                      path (type-of value) seg)))))))

(defun resolve-refs (params results)
  "Walk PARAMS (hash-table or vector), replacing any ref objects with resolved values.
   Returns a new hash-table/vector with refs replaced (does not mutate PARAMS)."
  (cond
    ;; Check if this is a ref object itself
    ((ref-object-p params)
     (resolve-ref-path (ref-object-p params) results))
    ;; Hash-table: resolve values recursively
    ((hash-table-p params)
     (let ((new-ht (make-hash-table :test #'equal)))
       (maphash (lambda (k v)
                  (setf (gethash k new-ht) (resolve-refs v results)))
                params)
       new-ht))
    ;; Vector (but not strings): resolve elements recursively
    ((and (vectorp params) (not (stringp params)))
     (map 'vector (lambda (v) (resolve-refs v results)) params))
    ;; List: resolve elements recursively (but not plists — those come from builtins)
    ((listp params)
     (mapcar (lambda (v) (resolve-refs v results)) params))
    ;; Scalar: return as-is
    (t params)))

;;; --- Step normalization ---

(defun normalize-step (step-spec)
  "Normalize a step spec (hash-table from JSON) into a canonical plist.
   Keys: :ID :KIND :SKILL :ENTRY-POINT :PARAMS :BUILTIN"
  (let ((id (gethash "id" step-spec))
        (kind-str (or (gethash "kind" step-spec) "auto"))
        (skill (gethash "skill" step-spec))
        (entry-point (gethash "entry_point" step-spec))
        (params (or (gethash "params" step-spec)
                    (make-hash-table :test #'equal)))
        (builtin (gethash "builtin" step-spec)))
    (unless id
      (error "Pipeline step missing required 'id' field"))
    (let ((kind (cond
                  ((string-equal kind-str "wasm") :wasm)
                  ((string-equal kind-str "builtin") :builtin)
                  (t :auto))))
      ;; Auto-detect kind
      (when (eq kind :auto)
        (setf kind (cond
                     (builtin :builtin)
                     (skill :wasm)
                     (t (error "Pipeline step ~S: must specify 'skill' or 'builtin'"
                               id)))))
      (list :id id :kind kind :skill skill :entry-point entry-point
            :params params :builtin builtin))))

;;; --- Pipeline condition ---

(define-condition pipeline-error (simple-error)
  ((step-id :initarg :step-id :reader pipeline-error-step-id)
   (step-skill :initarg :step-skill :reader pipeline-error-step-skill))
  (:report (lambda (c stream)
             (format stream "Pipeline error at step ~S~@[ (skill: ~A)~]: ~?"
                     (pipeline-error-step-id c)
                     (pipeline-error-step-skill c)
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)))))

;;; --- Main executor ---

(defun execute-pipeline (steps)
  "Execute a pipeline: an ordered list of step specs.
   Each step is a hash-table (from JSON) or plist with:
     id          — unique step identifier (string)
     kind        — \"wasm\", \"builtin\", or \"auto\"
     skill       — skill name for WASM steps
     entry_point — WASM function or builtin name to call
     params      — input parameters (may contain {\"ref\": \"...\"} references)
     builtin     — built-in function name for builtin steps

   Returns a hash-table of step-id → result."
  (let ((results (make-hash-table :test #'equal))
        (seen-ids (make-hash-table :test #'equal)))
    ;; Normalize all steps first
    (let ((normalized (mapcar (lambda (s)
                                (if (hash-table-p s)
                                    (normalize-step s)
                                    s))  ; already a plist
                              steps)))
      ;; Validate unique IDs
      (dolist (step normalized)
        (let ((id (getf step :id)))
          (when (gethash id seen-ids)
            (error 'pipeline-error
                   :step-id id :step-skill nil
                   :format-control "duplicate step id ~S"
                   :format-arguments (list id)))
          (setf (gethash id seen-ids) t)))
      ;; Execute steps in order
      (dolist (step normalized results)
        (let* ((id (getf step :id))
               (kind (getf step :kind))
               (skill (getf step :skill))
               (entry-point (getf step :entry-point))
               (raw-params (getf step :params))
               (builtin-name (getf step :builtin))
               (resolved-params (resolve-refs raw-params results)))
          (log:info "Pipeline step ~A (~A): ~A"
                    id kind (or skill builtin-name))
          (restart-case
              (let ((result
                      (ecase kind
                        (:builtin
                         (let* ((name (or builtin-name entry-point))
                                (fn (pipeline-builtin name)))
                           (unless fn
                             (error "Unknown pipeline builtin: ~S" name))
                           (funcall fn resolved-params)))
                        (:wasm
                         (unless skill
                           (error "WASM step ~S missing 'skill' field" id))
                         (invoke-skill skill
                                       :entry-point entry-point
                                       :params resolved-params)))))
                (setf (gethash id results) result)
                (log:info "Pipeline step ~A completed" id))
            (:skip-step ()
              :report (lambda (s) (format s "Skip pipeline step ~A" id))
              (setf (gethash id results) nil)
              (log:warn "Pipeline step ~A skipped" id))
            (:use-value (value)
              :report (lambda (s) (format s "Supply a result for step ~A" id))
              :interactive (lambda ()
                             (format *query-io* "~&Step ~A result: " id)
                             (list (eval (read *query-io*))))
              (setf (gethash id results) value)
              (log:info "Pipeline step ~A: using supplied value" id))
            (:retry ()
              :report (lambda (s) (format s "Retry pipeline step ~A" id))
              ;; Re-resolve refs in case upstream results changed
              (let ((re-resolved (resolve-refs raw-params results)))
                (setf resolved-params re-resolved))
              ;; Re-run step dispatch inline
              (let ((result
                      (ecase kind
                        (:builtin
                         (let* ((name (or builtin-name entry-point))
                                (fn (pipeline-builtin name)))
                           (unless fn
                             (error "Unknown pipeline builtin: ~S" name))
                           (funcall fn resolved-params)))
                        (:wasm
                         (invoke-skill skill
                                       :entry-point entry-point
                                       :params resolved-params)))))
                (setf (gethash id results) result)
                (log:info "Pipeline step ~A completed (retry)" id)))))))))
