;;;; agent/tools-data/skills.lisp
;;;;
;;;; Tool definition for WASM skill and pipeline management.

(in-package #:crichton/agent)

;;; --- Skills tool helpers ---

(defun skills-run-pipeline (steps)
  "Execute a multi-step skill pipeline from the STEPS array.
   Returns a formatted result string."
  (unless steps
    (return-from skills-run-pipeline
      "Error: 'steps' array is required for pipeline action."))
  (handler-case
      (let* ((step-list (coerce steps 'list))
             (results (execute-pipeline step-list))
             (step-count (hash-table-count results)))
        (with-output-to-string (s)
          (format s "Pipeline completed (~D step~:P):~%" step-count)
          (maphash (lambda (id result)
                     (format s "  ~A: ~S~%" id result))
                   results)))
    (pipeline-error (c)
      (format nil "Pipeline failed at step '~A': ~A"
              (pipeline-error-step-id c) c))
    (error (c)
      (format nil "Pipeline error: ~A" c))))

;;; --- Skills tool ---

(define-tool skills
    (:description "Manage external WASM skills and pipelines.  Actions: 'list' (show all discovered skills), 'info' (get details for a specific skill), 'invoke' (run a skill with optional params), 'refresh' (re-scan skills directory), 'pipeline' (run a multi-step pipeline chaining skills together), 'save_pipeline' (save a named pipeline for scheduling), 'delete_pipeline' (remove a saved pipeline), 'list_pipelines' (show saved pipelines).  Saved pipelines are registered as schedulable actions (name: 'pipeline:<name>') and can be scheduled via the scheduler tool.  Use 'params' to pass structured input to skills that expect JSON data (e.g., rss-filter).  Use 'pipeline' to chain multiple steps where later steps reference earlier results via {\"ref\": \"step_id.key\"}.")
  ((action "string"
           "The skills action to perform."
           :enum ("list" "info" "invoke" "refresh" "pipeline"
                  "save_pipeline" "delete_pipeline" "list_pipelines")
           :required-p t)
   (name "string"
         "Skill name. Required for info, invoke, save_pipeline, delete_pipeline.")
   (entry-point "string"
                "Function entry point to call. Optional; defaults to the manifest's declared entry point.")
   (params "object"
           "JSON parameters to pass to the skill. When provided, the JSON ABI is used automatically. Required for pure-function skills like rss-filter.")
   (steps "array"
          "Pipeline steps array (for 'pipeline' and 'save_pipeline' actions). Each step is an object with: id (string, required), kind ('wasm'/'builtin'/'auto'), skill (string, for WASM steps), builtin (string, for builtin steps like 'rss_fetch', 'rss_check', 'weather'), entry_point (string), params (object, may contain {\"ref\": \"step_id.key\"} references to earlier step outputs)."))
  (cond
    ((string-equal action "list")
     (discover-skills)            ; refresh before listing
     (with-output-to-string (s)
       (skill-report :stream s)))
    ((string-equal action "info")
     (unless name
       (return-from handler "Error: 'name' is required for info action."))
     (let ((info (skill-info name)))
       (if info
           (format nil "~S" info)
           (format nil "Skill '~A' not found." name))))
    ((string-equal action "invoke")
     (unless name
       (return-from handler "Error: 'name' is required for invoke action."))
     (handler-case
         (let ((result (invoke-skill name
                         :entry-point entry-point
                         :params params)))
           (format nil "Skill '~A' returned: ~A" name result))
       (error (c)
         (format nil "Error invoking skill '~A': ~A" name c))))
    ((string-equal action "pipeline")
     (skills-run-pipeline steps))
    ((string-equal action "save_pipeline")
     (unless name
       (return-from handler "Error: 'name' is required for save_pipeline."))
     (unless steps
       (return-from handler "Error: 'steps' is required for save_pipeline."))
     (save-pipeline name steps)
     (format nil "Pipeline '~A' saved (~D step~:P). Schedulable as action 'pipeline:~A'."
             name (length steps) name))
    ((string-equal action "delete_pipeline")
     (unless name
       (return-from handler "Error: 'name' is required for delete_pipeline."))
     (if (delete-pipeline name)
         (format nil "Pipeline '~A' deleted." name)
         (format nil "Pipeline '~A' not found." name)))
    ((string-equal action "list_pipelines")
     (let ((pipelines (list-saved-pipelines)))
       (if pipelines
           (with-output-to-string (s)
             (format s "Saved pipelines:~%")
             (dolist (p pipelines)
               (format s "  ~A (~D step~:P) — schedulable as 'pipeline:~A'~%"
                       (getf p :name) (getf p :step-count) (getf p :name))))
           "No saved pipelines.")))
    ((string-equal action "refresh")
     (let ((count (discover-skills)))
       (format nil "Discovered ~D skill~:P." count)))
    (t
     (format nil "Unknown skills action: ~A" action))))
