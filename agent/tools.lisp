;;;; agent/tools.lisp
;;;;
;;;; Tool definitions wrapping built-in skills for the agent loop.
;;;; Each tool has:
;;;;   - Anthropic API schema (name, description, input_schema)
;;;;   - Handler function (input hash-table → result string)
;;;;
;;;; Tools are registered in *agent-tools* and looked up by name
;;;; during agent loop dispatch.

(in-package #:crichton/agent)

;;; --- Tool registry ---

(defclass agent-tool ()
  ((name :initarg :name :accessor agent-tool-name :initform "" :type string)
   (description :initarg :description :accessor agent-tool-description :initform "" :type string)
   (input-schema :initarg :input-schema :accessor agent-tool-input-schema :initform nil)
   (handler :initarg :handler :accessor agent-tool-handler :initform nil :type (or null function))))

(defun %make-agent-tool (&key (name "") (description "") input-schema handler)
  (make-instance 'agent-tool :name name :description description
                 :input-schema input-schema :handler handler))

(defvar *agent-tools* (make-hash-table :test #'equal)
  "Registry of available agent tools, keyed by name.")

(defun register-tool (name description input-schema handler)
  "Register a tool for agent use."
  (setf (gethash name *agent-tools*)
        (%make-agent-tool :name name
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
   Offers :USE-VALUE and :REPORT-ERROR restarts."
  (let ((tool (get-tool name)))
    (unless tool
      (return-from dispatch-tool
        (format nil "Error: unknown tool ~S" name)))
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

(defun %hget (ht key &optional default)
  "Get KEY from hash-table HT, returning DEFAULT if missing."
  (if (and ht (hash-table-p ht))
      (multiple-value-bind (val found) (gethash key ht)
        (if found val default))
      default))

;;; --- Tool definitions ---

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
  (defun %symbol-to-underscored (sym)
    "Convert a Lisp symbol to an underscored lowercase string.
     E.g. SYSTEM-INFO → \"system_info\"."
    (substitute #\_ #\- (string-downcase (symbol-name sym))))

  (defun %extract-declarations (body)
    "Separate leading DECLARE forms from BODY.
     Returns (values declarations remaining-body)."
    (loop for (form . rest) on body
          while (and (consp form) (eq (car form) 'declare))
          collect form into decls
          finally (return (values decls (cons form rest)))))

  (defun %register-fn-name (name)
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
                       (%symbol-to-underscored name)))
         (register-fn (%register-fn-name name)))
    ;; Build the make-properties argument forms and the let-bindings
    (let ((prop-forms
            (loop for spec in params
                  collect
                  (destructuring-bind (var type desc &key required-p enum
                                                          default)
                      spec
                    (declare (ignore default))
                    (let ((name-str (%symbol-to-underscored var)))
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
                    (let ((name-str (%symbol-to-underscored var)))
                      (if default
                          `(,var (%hget input ,name-str ,default))
                          `(,var (%hget input ,name-str))))))))
      (multiple-value-bind (declarations real-body)
          (%extract-declarations body)
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

;;; --- Weather tool ---

(define-tool weather
    (:description "Get current weather conditions and forecast for a Canadian city.  Uses Environment Canada data.  Provides temperature, humidity, wind, pressure, and multi-day forecast.")
  ((city "string"
         "Canadian city name for weather lookup. Default: configured city or Victoria."))
  (with-output-to-string (s)
    (crichton/skills:weather-report :city city :stream s)))

;;; --- System info tool ---

(define-tool system-info
    (:description "Get current system health metrics: CPU load average, memory usage, thermal zones, and disk usage.  Linux only (reads /proc and /sys).  Use this to check if the system is healthy or under stress.")
  ((include-load "boolean" "Include CPU load average. Default: true.")
   (include-memory "boolean" "Include memory statistics. Default: true.")
   (include-thermal "boolean" "Include thermal zone temperatures. Default: true.")
   (include-disk "boolean" "Include disk usage. Default: true."))
  (declare (ignore include-load include-memory include-thermal include-disk))
  (with-output-to-string (s)
    (crichton/skills:system-report
     :stream s
     :mounts '("/" "/home"))))

;;; --- Scheduler tool helpers ---

(defun %scheduler-schedule-every (action-name interval-seconds task-name)
  "Handle the schedule_every scheduler action."
  (unless action-name
    (return-from %scheduler-schedule-every
      "Error: 'action_name' is required. Use action 'actions' to list available."))
  (unless interval-seconds
    (return-from %scheduler-schedule-every "Error: 'interval_seconds' is required."))
  (let ((act (crichton/skills:get-schedulable-action action-name)))
    (unless act
      (return-from %scheduler-schedule-every
        (format nil "Error: unknown action '~A'. Use action 'actions' to list available." action-name)))
    (crichton/skills:schedule-every task-name interval-seconds (getf act :fn)
                                    :replace t :action-name action-name)
    (format nil "Scheduled '~A' every ~Ds as task '~A'." action-name interval-seconds task-name)))

(defun %scheduler-schedule-daily (action-name hour minute task-name)
  "Handle the schedule_daily scheduler action."
  (unless action-name
    (return-from %scheduler-schedule-daily "Error: 'action_name' is required."))
  (unless hour
    (return-from %scheduler-schedule-daily "Error: 'hour' is required for schedule_daily."))
  (let ((act (crichton/skills:get-schedulable-action action-name))
        (min (or minute 0)))
    (unless act
      (return-from %scheduler-schedule-daily
        (format nil "Error: unknown action '~A'." action-name)))
    (crichton/skills:schedule-daily task-name hour min (getf act :fn)
                                    :replace t :action-name action-name)
    (format nil "Scheduled '~A' daily at ~2,'0D:~2,'0D as task '~A'." action-name hour min task-name)))

(defun %scheduler-schedule-once (action-name delay-seconds task-name)
  "Handle the schedule_once scheduler action."
  (unless action-name
    (return-from %scheduler-schedule-once "Error: 'action_name' is required."))
  (unless delay-seconds
    (return-from %scheduler-schedule-once "Error: 'delay_seconds' is required."))
  (let ((act (crichton/skills:get-schedulable-action action-name)))
    (unless act
      (return-from %scheduler-schedule-once
        (format nil "Error: unknown action '~A'." action-name)))
    (crichton/skills:schedule-at task-name (+ (get-universal-time) delay-seconds) (getf act :fn)
                                 :replace t :action-name action-name)
    (format nil "Scheduled '~A' to run once in ~Ds as task '~A'." action-name delay-seconds task-name)))

;;; --- Scheduler tool ---

(define-tool scheduler
    (:description "Manage the daemon's task scheduler.  Actions: 'status' (overview), 'list' (all tasks), 'actions' (list schedulable actions), 'schedule_every' (recurring task), 'schedule_daily' (daily at specific time), 'schedule_once' (one-shot after delay), 'cancel' (remove a task by name).")
  ((action "string"
           "The scheduler action to perform."
           :enum ("status" "list" "actions" "schedule_every" "schedule_daily" "schedule_once" "cancel")
           :required-p t)
   (action-name "string"
                "Name of the schedulable action to run (use 'actions' to list available). Required for schedule_every, schedule_daily, schedule_once.")
   (interval-seconds "integer"
                     "Interval in seconds for schedule_every. Required for schedule_every.")
   (hour "integer"
         "Hour (0-23) for schedule_daily. Required for schedule_daily.")
   (minute "integer"
           "Minute (0-59) for schedule_daily. Required for schedule_daily.")
   (delay-seconds "integer"
                  "Delay in seconds from now for schedule_once. Required for schedule_once.")
   (name "string"
         "Task name. Auto-generated with 'user:' prefix if omitted."))
  (let ((task-name (or name (when action-name
                               (format nil "user:~A" action-name)))))
    (cond
      ((string-equal action "status")
       (format nil "~S" (crichton/skills:scheduler-status)))
      ((string-equal action "list")
       (format nil "~S" (crichton/skills:list-tasks)))
      ((string-equal action "actions")
       (let ((actions (crichton/skills:list-schedulable-actions)))
         (if actions
             (with-output-to-string (s)
               (format s "Available schedulable actions:~%")
               (dolist (a actions)
                 (format s "  ~A - ~A~%" (getf a :name) (getf a :description))))
             "No schedulable actions registered.")))
      ((string-equal action "schedule_every")
       (%scheduler-schedule-every action-name interval-seconds task-name))
      ((string-equal action "schedule_daily")
       (%scheduler-schedule-daily action-name hour minute task-name))
      ((string-equal action "schedule_once")
       (%scheduler-schedule-once action-name delay-seconds task-name))
      ((string-equal action "cancel")
       (unless name
         (return-from handler "Error: 'name' is required for cancel."))
       (if (crichton/skills:cancel-task name)
           (format nil "Cancelled task '~A'." name)
           (format nil "No task found with name '~A'." name)))
      (t
       (format nil "Unknown scheduler action: ~A" action)))))

;;; --- Current time tool ---

(define-tool time
    (:description "Get the current date and time.  Returns the current date and time in both human-readable and Unix timestamp formats.  Useful for understanding when events happen relative to the current moment.")
  ()
  (with-output-to-string (s)
    (crichton/skills:current-time-report :stream s)
    (let ((pl (crichton/skills:current-time-plist)))
      (format s "Unix timestamp: ~D~%" (getf pl :unix-seconds)))))

;;; --- Ephemeris tool (solar + lunar) ---

(define-tool ephemeris
    (:description "Get solar and lunar ephemeris data for today.  Includes sunrise, sunset, solar noon, day length, lunar phase, and illumination percentage.  Uses the configured location.  No input required.")
  ()
  (let ((lat (crichton/config:config-section-get :location :latitude 48.43))
        (lon (crichton/config:config-section-get :location :longitude -123.37)))
    (with-output-to-string (s)
      (crichton/skills:ephemeris-report lat lon :stream s))))

;;; --- RSS tool ---

(define-tool rss
    (:description "Fetch, check, and monitor RSS/Atom feeds.  Use 'fetch' to read all items from a feed, 'check' to see only new items since last check, 'monitor_start' to begin periodic monitoring, 'monitor_stop' to stop a monitor, or 'list_monitors' to see active monitors.  Monitors can optionally filter items by keywords using the rss-filter WASM skill — only matching items are reported and notified.  Filter settings survive daemon restarts.")
  ((action "string"
           "The RSS action: fetch (get all items), check (get new items only), monitor_start (begin periodic monitoring), monitor_stop (stop monitoring), list_monitors (show active monitors)."
           :enum ("fetch" "check" "monitor_start" "monitor_stop" "list_monitors")
           :required-p t)
   (url "string"
        "The RSS/Atom feed URL. Required for fetch, check, monitor_start.")
   (name "string"
         "Monitor name (for monitor_start/monitor_stop). Conventionally 'rss:something'.")
   (interval-seconds "integer"
                     "Polling interval in seconds for monitor_start. Default: 3600 (1 hour)."
                     :default 3600)
   (keywords "array"
             "Optional list of keyword strings to filter items through the rss-filter WASM skill.  Only items matching these keywords will be reported.  Requires the rss-filter skill to be installed.")
   (match-mode "string"
               "Keyword matching mode: 'any' (default, match if any keyword found) or 'all' (match only if all keywords found)."
               :enum ("any" "all"))
   (search-fields "array"
                  "Which item fields to search for keywords.  Default: [\"title\", \"description\"].  Valid values: title, description, content."))
  (cond
    ((string-equal action "fetch")
     (if (null url)
         "Error: 'url' is required for fetch"
         (with-output-to-string (s)
           (crichton/skills:rss-report url :stream s))))
    ((string-equal action "check")
     (if (null url)
         "Error: 'url' is required for check"
         (with-output-to-string (s)
           (crichton/skills:rss-check-report url :stream s))))
    ((string-equal action "monitor_start")
     (if (null url)
         "Error: 'url' is required for monitor_start"
         (let ((task-name (or name (format nil "rss:~A" url)))
               (kw-list (when keywords (coerce keywords 'list)))
               (sf-list (when search-fields (coerce search-fields 'list))))
           (crichton/skills:rss-monitor-start task-name url interval-seconds
                                              :keywords kw-list
                                              :match-mode match-mode
                                              :search-fields sf-list)
           (format nil "Monitor started: ~A (every ~Ds~@[, filtering for: ~{~A~^, ~}~])"
                   task-name interval-seconds kw-list))))
    ((string-equal action "monitor_stop")
     (let ((task-name (or name "?")))
       (if (crichton/skills:rss-monitor-stop task-name)
           (format nil "Monitor stopped: ~A" task-name)
           (format nil "Monitor not found: ~A" task-name))))
    ((string-equal action "list_monitors")
     (let ((monitors (crichton/skills:rss-list-monitors)))
       (if monitors
           (format nil "~{~S~^~%~}" monitors)
           "No active RSS monitors.")))
    (t (format nil "Unknown RSS action: ~A" action))))

;;; --- Battery tool ---

(define-tool battery
    (:description "Monitor battery level and charging status on laptop systems.  Get current battery percentage, charging state, and time remaining.  Can start/stop periodic monitoring with proactive alerts at configurable thresholds.  Linux only (reads /sys/class/power_supply/).")
  ((action "string"
           "The battery action: status (current battery info), start_monitoring (enable periodic checks), stop_monitoring (disable periodic checks)."
           :enum ("status" "start_monitoring" "stop_monitoring")
           :required-p t))
  (cond
    ((string-equal action "status")
     (with-output-to-string (s)
       (crichton/skills:battery-report s)))
    ((string-equal action "start_monitoring")
     (if (crichton/skills:start-battery-monitoring)
         "Battery monitoring started. Will check periodically and alert on low battery."
         "Battery monitoring not available (no batteries detected)."))
    ((string-equal action "stop_monitoring")
     (if (crichton/skills:stop-battery-monitoring)
         "Battery monitoring stopped."
         "Battery monitoring was not running."))
    (t
     (format nil "Unknown battery action: ~A" action))))

;;; --- Token/resource usage tool ---

(define-tool usage
    (:description "Query metered resource usage, burn rate, and cost estimates.  Tracks any service that consumes tokens or API calls (LLM, weather API, etc.).  Use 'summary' for aggregate view, 'meter' for one service, 'list' to see meter names, 'recent' for call history."
     :tool-name "resource_usage")
  ((action "string"
           "The action: summary (aggregate across all meters), meter (single meter detail), list (list meter names), recent (recent calls for a meter)."
           :enum ("summary" "meter" "list" "recent")
           :required-p t)
   (meter "string"
          "Meter name (for 'meter' and 'recent' actions). e.g. 'llm'.")
   (count "integer"
          "Number of recent records to return (for 'recent'). Default: 10."
          :default 10))
  (cond
    ((string-equal action "summary")
     (with-output-to-string (s)
       (crichton/skills:usage-report :stream s)))
    ((string-equal action "meter")
     (if (null meter)
         "Error: 'meter' parameter required for meter action"
         (with-output-to-string (s)
           (crichton/skills:meter-report meter :stream s))))
    ((string-equal action "list")
     (let ((names (crichton/skills:list-meters)))
       (if names
           (format nil "Active meters: ~{~A~^, ~}" names)
           "No active meters.")))
    ((string-equal action "recent")
     (if (null meter)
         "Error: 'meter' parameter required for recent action"
         (format nil "~{~S~^~%~}"
                 (crichton/skills:meter-recent meter count))))
    (t (format nil "Unknown action: ~A" action))))

;;; --- Daemon log inspector tool ---

(defun %format-log-entries (entries)
  "Format a list of log entry hash-tables as human-readable text."
  (with-output-to-string (s)
    (if (null entries)
        (format s "No log entries found.~%")
        (dolist (entry entries)
          (format s "[~A] [~A] (~A) ~A~%"
                  (gethash "timestamp" entry "?")
                  (gethash "level" entry "?")
                  (gethash "logger" entry "?")
                  (gethash "message" entry ""))))))

(define-tool log-inspector
    (:description "Inspect the daemon's own log files.  Use to check for errors, warnings, or verify that systems are functioning correctly.  Logs are sanitized — safe to share with users."
     :tool-name "daemon_logs")
  ((action "string"
           "The log inspection action to perform."
           :enum ("summary" "recent" "errors" "search")
           :required-p t)
   (count "integer"
          "Number of log entries to examine. Default: 50."
          :default 50)
   (pattern "string"
            "Search substring for the 'search' action. Case-insensitive match against message text.")
   (level "string"
          "Filter by log level (ERROR, WARN, INFO, DEBUG). Optional for 'search' action."
          :enum ("ERROR" "WARN" "INFO" "DEBUG")))
  (cond
    ((string-equal action "summary")
     (with-output-to-string (s)
       (crichton/skills:log-report :stream s :count count)))
    ((string-equal action "recent")
     (%format-log-entries
      (crichton/skills:read-log-tail :count count)))
     ((string-equal action "errors")
      (%format-log-entries
      (crichton/skills:search-log :level "ERROR" :count count)))
     ((string-equal action "search")
      (if (and (null pattern) (null level))
          "Error: 'pattern' and/or 'level' required for search action."
          (%format-log-entries
          (crichton/skills:search-log :pattern pattern
                                      :level level
                                      :count count))))
    (t
     (format nil "Unknown daemon_logs action: ~A" action))))

;;; --- Amp orchestration tools ---

(define-tool amp-check
    (:description "Check whether the Amp CLI coding agent is available and enabled on this system.  Returns availability and configuration status.  Use this before attempting amp_code or amp_test tasks.")
  ()
  (let ((status (crichton/skills:amp-status)))
    (cond
      ((not (getf status :enabled))
       "Amp orchestration is DISABLED in config. Set [amp] enable = true in config.toml.")
      ((not (getf status :binary-available))
       "Amp is enabled in config but the CLI binary is NOT found on PATH. Install it to enable code delegation.")
      (t
       (format nil "Amp CLI is available and enabled.~%Binary: ~A~%Allowed repo roots: ~A"
               (getf status :binary-path)
               (or (getf status :allowed-repo-roots) "unrestricted"))))))

(define-tool amp-code
    (:description "Delegate a coding task to the Amp CLI agent.  Amp is an AI coding assistant that can read, write, and modify code files.  Use this for implementation tasks: writing new functions, fixing bugs, refactoring code, adding features.  Requires Amp to be enabled in config and CLI installed (check with amp_check first).")
  ((description "string"
                "Description of the coding task to delegate to Amp. Be specific about what code to write or modify."
                :required-p t)
   (repo-path "string"
              "Absolute path to the repository to work in. Must be under an allowed_repo_roots path."
              :required-p t)
   (files "string"
          "Comma-separated list of file paths to focus on (relative to repo_path).")
   (context "string"
            "Additional context for the task (e.g., coding standards, constraints, existing patterns).")
   (timeout-seconds "integer"
                    "Maximum time in seconds to wait for Amp to complete. Default: 300 (5 minutes)."
                    :default 300))
  (let ((file-list (when files
                     (mapcar (lambda (f)
                               (string-trim '(#\Space) f))
                             (cl-ppcre:split "," files)))))
    (let ((result (crichton/skills:amp-code-task
                   description
                   :repo-path repo-path
                   :files file-list
                   :context context
                   :timeout-seconds timeout-seconds)))
      (with-output-to-string (s)
        (crichton/skills:amp-report result :stream s)))))

(define-tool amp-test
    (:description "Run a test suite and optionally use the Amp CLI agent to fix failures.  Executes the test runner directly (no shell).  If tests fail and fix_failures is true, Amp is invoked with the failure context to attempt repairs, iterating up to max_iterations.  Requires Amp to be enabled in config for fix mode.")
  ((test-runner "string"
                "The test runner executable (e.g., 'pytest', 'make', 'cargo', 'sbcl')."
                :required-p t)
   (test-args "string"
              "Comma-separated arguments for the test runner (e.g., 'test,--verbose'). Each comma-separated element is passed as a separate argument.")
   (repo-path "string"
              "Absolute path to the repository. Tests run with this as the working directory.")
   (fix-failures "boolean"
                 "If true, invoke Amp to fix test failures automatically. Default: false.")
   (max-iterations "integer"
                   "Maximum test/fix cycles when fix_failures is true. Default: 3."
                   :default 3)
   (timeout-seconds "integer"
                    "Maximum time in seconds per Amp fix invocation. Default: 300."
                    :default 300))
  (let ((args-list (when test-args
                     (mapcar (lambda (a) (string-trim '(#\Space) a))
                             (cl-ppcre:split "," test-args)))))
    (let ((result (crichton/skills:amp-test-task
                   test-runner
                   :test-args args-list
                   :repo-path repo-path
                   :fix-failures fix-failures
                   :max-iterations max-iterations
                   :timeout-seconds timeout-seconds)))
      (with-output-to-string (s)
        (crichton/skills:amp-report result :stream s)))))

;;; --- Amp JSON tools (cricht-olb) ---

(defun %plist-to-json-ht (plist)
  "Recursively convert a plist to a JSON-compatible hash-table.
   Keyword keys become lowercase underscore strings."
  (cond
    ((and (listp plist) (evenp (length plist))
          (plusp (length plist))
          (loop for (k) on plist by #'cddr always (keywordp k)))
     (let ((ht (make-hash-table :test #'equal)))
       (loop for (k v) on plist by #'cddr
             do (setf (gethash (substitute #\_ #\-
                                 (string-downcase (symbol-name k)))
                               ht)
                      (%plist-to-json-ht v)))
       ht))
    ((listp plist) (mapcar #'%plist-to-json-ht plist))
    ((vectorp plist) (map 'vector #'%plist-to-json-ht plist))
    (t plist)))

(defun %json-string (plist)
  "Convert a plist to a JSON string."
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (shasht:write-json (%plist-to-json-ht plist) s))))

(define-tool amp-check-json
    (:description "Check Amp CLI status and return structured JSON. Includes enabled state, binary availability, binary path, and allowed repo roots."
     :tool-name "amp_check_json")
  ()
  (%json-string (crichton/skills:amp-status)))

(define-tool amp-code-json
    (:description "Delegate a coding task to the Amp CLI agent and return structured JSON result. Includes success, output, error, exit code, elapsed time, changed files, and truncation flags."
     :tool-name "amp_code_json")
  ((description "string"
                "Description of the coding task to delegate to Amp."
                :required-p t)
   (repo-path "string"
              "Absolute path to the repository to work in."
              :required-p t)
   (files "string"
          "Comma-separated list of file paths to focus on (relative to repo_path).")
   (context "string"
            "Additional context for the task.")
   (timeout-seconds "integer"
                    "Maximum time in seconds. Default: 300."
                    :default 300))
  (let ((file-list (when files
                     (mapcar (lambda (f) (string-trim '(#\Space) f))
                             (cl-ppcre:split "," files)))))
    (%json-string (crichton/skills:amp-code-task
                   description
                   :repo-path repo-path
                   :files file-list
                   :context context
                   :timeout-seconds timeout-seconds))))

(define-tool amp-test-json
    (:description "Run a test suite with optional Amp fix mode and return structured JSON result. Includes success, iterations, final output, and elapsed time."
     :tool-name "amp_test_json")
  ((test-runner "string"
                "The test runner executable."
                :required-p t)
   (test-args "string"
              "Comma-separated arguments for the test runner.")
   (repo-path "string"
              "Absolute path to the repository.")
   (fix-failures "boolean"
                 "If true, invoke Amp to fix test failures. Default: false.")
   (max-iterations "integer"
                   "Maximum test/fix cycles. Default: 3."
                   :default 3)
   (timeout-seconds "integer"
                    "Maximum time per Amp fix invocation. Default: 300."
                    :default 300))
  (let ((args-list (when test-args
                     (mapcar (lambda (a) (string-trim '(#\Space) a))
                             (cl-ppcre:split "," test-args)))))
    (%json-string (crichton/skills:amp-test-task
                   test-runner
                   :test-args args-list
                   :repo-path repo-path
                   :fix-failures fix-failures
                   :max-iterations max-iterations
                   :timeout-seconds timeout-seconds))))

;;; --- Memory tools ---

(define-tool memory-write
    (:description "Write a durable fact to long-term memory (MEMORY.org) or today's journal.  Use 'memory' target for important, curated facts that should persist across sessions (user preferences, project context, lessons learned).  Use 'journal' target for session-specific notes and observations that don't rise to long-term memory level.")
  ((target "string"
           "Where to write: 'memory' for MEMORY.org (curated, long-term), 'journal' for today's daily log (raw, session-specific)."
           :enum ("memory" "journal")
           :required-p t)
   (content "string"
            "The content to write. Use org-mode formatting (- for list items, * for headings)."
            :required-p t))
  (cond
    ((string-equal target "memory")
     (let ((path (crichton/state:append-to-memory content)))
       (format nil "Written to long-term memory (~A)." (file-namestring path))))
    ((string-equal target "journal")
     (let ((path (crichton/state:journal-append content)))
       (format nil "Written to journal (~A)." (file-namestring path))))
    (t
     (format nil "Unknown target: ~A" target))))

(define-tool memory-search
    (:description "Search daily journal entries by keyword.  Returns matching excerpts with dates and context.  Use this to recall session-specific notes and observations from recent days.")
  ((query "string"
          "Search term to look for in journal entries. Case-insensitive substring match."
          :required-p t)
   (days-back "integer"
              "Number of days to search back from today. Default: 7."
              :default 7))
  (crichton/state:journal-search query :days-back days-back))

;;; --- Raindrop.io bookmarks tool ---

(define-tool raindrop
    (:description "Manage bookmarks in Raindrop.io.  Actions: 'save' (create a new bookmark), 'search' (find bookmarks by keyword), 'list' (list bookmarks in a collection), 'get' (get a bookmark by ID), 'update' (modify a bookmark), 'delete' (trash a bookmark), 'collections' (list all collections), 'create_collection' (create a new collection), 'tags' (list all tags).  Requires Raindrop.io API token stored as 'raindrop-api' in credentials.")
  ((action "string"
           "The bookmark action to perform."
           :enum ("save" "search" "list" "get" "update" "delete"
                  "collections" "create_collection" "tags")
           :required-p t)
   (url "string"
        "Bookmark URL. Required for 'save'.")
   (title "string"
          "Bookmark title. Optional for 'save' and 'update'.")
   (excerpt "string"
            "Bookmark description/excerpt. Optional for 'save' and 'update'.")
   (tags "string"
         "Comma-separated tags. Optional for 'save' and 'update'.")
   (collection-id "integer"
                   "Collection ID. For 'save'/'update': target collection. For 'list'/'tags': scope. Use 0 for all, -1 for Unsorted.")
   (query "string"
          "Search query text. Required for 'search'.")
   (id "integer"
       "Raindrop (bookmark) ID. Required for 'get', 'update', 'delete'.")
   (important "string"
              "Mark as favorite: 'true' or 'false'. For 'update'.")
   (page "integer"
         "Page number (0-based) for 'list' and 'search'. Default: 0."
         :default 0)
   (per-page "integer"
             "Results per page for 'list' and 'search'. Max 50. Default: 25."
             :default 25))
  (let ((tag-list (when tags
                    (mapcar (lambda (s) (string-trim '(#\Space) s))
                            (cl-ppcre:split "," tags)))))
    (cond
      ((string-equal action "save")
       (unless url
         (return-from handler "Error: 'url' is required for save."))
       (with-output-to-string (s)
         (crichton/skills:raindrop-save-report
          url :title title :excerpt excerpt :tags tag-list
              :collection-id collection-id :stream s)))
      ((string-equal action "search")
       (unless query
         (return-from handler "Error: 'query' is required for search."))
       (with-output-to-string (s)
         (crichton/skills:raindrop-find-report
          query :collection-id (or collection-id 0)
                :max-items per-page :stream s)))
      ((string-equal action "list")
       (with-output-to-string (s)
         (multiple-value-bind (items total)
             (crichton/skills:raindrop-list
              (or collection-id 0) :page page :per-page per-page)
           (crichton/skills:format-raindrop-list
            items :stream s :total total))))
      ((string-equal action "get")
       (unless id
         (return-from handler "Error: 'id' is required for get."))
       (let ((item (crichton/skills:raindrop-get-one id)))
         (if item
             (with-output-to-string (s)
               (crichton/skills:format-raindrop item s))
             (format nil "Bookmark ~D not found." id))))
      ((string-equal action "update")
       (unless id
         (return-from handler "Error: 'id' is required for update."))
       (let ((result (crichton/skills:raindrop-update
                      id :title title :excerpt excerpt
                         :tags tag-list :link url
                         :collection-id collection-id
                         :important important)))
         (with-output-to-string (s)
           (format s "Updated bookmark:~%")
           (crichton/skills:format-raindrop result s))))
      ((string-equal action "delete")
       (unless id
         (return-from handler "Error: 'id' is required for delete."))
       (if (crichton/skills:raindrop-remove id)
           (format nil "Bookmark ~D moved to Trash." id)
           (format nil "Failed to delete bookmark ~D." id)))
      ((string-equal action "collections")
       (with-output-to-string (s)
         (crichton/skills:raindrop-collections-report :stream s)))
      ((string-equal action "create_collection")
       (unless title
         (return-from handler "Error: 'title' is required for create_collection."))
       (let ((result (crichton/skills:raindrop-create-collection
                      title :parent-id collection-id)))
         (format nil "Created collection: ~A (id: ~D)"
                 (getf result :title) (getf result :id))))
      ((string-equal action "tags")
       (with-output-to-string (s)
         (crichton/skills:raindrop-tags-report
          :collection-id collection-id :stream s)))
      (t
       (format nil "Unknown raindrop action: ~A" action)))))

;;; --- Skills tool helpers ---

(defun %skills-run-pipeline (steps)
  "Execute a multi-step skill pipeline from the STEPS array.
   Returns a formatted result string."
  (unless steps
    (return-from %skills-run-pipeline
      "Error: 'steps' array is required for pipeline action."))
  (handler-case
      (let* ((step-list (coerce steps 'list))
             (results (crichton/skills:execute-pipeline step-list))
             (step-count (hash-table-count results)))
        (with-output-to-string (s)
          (format s "Pipeline completed (~D step~:P):~%" step-count)
          (maphash (lambda (id result)
                     (format s "  ~A: ~S~%" id result))
                   results)))
    (crichton/skills:pipeline-error (c)
      (format nil "Pipeline failed at step '~A': ~A"
              (crichton/skills:pipeline-error-step-id c) c))
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
     (crichton/skills:discover-skills)            ; refresh before listing
     (with-output-to-string (s)
       (crichton/skills:skill-report :stream s)))
    ((string-equal action "info")
     (unless name
       (return-from handler "Error: 'name' is required for info action."))
     (let ((info (crichton/skills:skill-info name)))
       (if info
           (format nil "~S" info)
           (format nil "Skill '~A' not found." name))))
    ((string-equal action "invoke")
     (unless name
       (return-from handler "Error: 'name' is required for invoke action."))
     (handler-case
         (let ((result (crichton/skills:invoke-skill name
                         :entry-point entry-point
                         :params params)))
           (format nil "Skill '~A' returned: ~A" name result))
       (error (c)
         (format nil "Error invoking skill '~A': ~A" name c))))
    ((string-equal action "pipeline")
     (%skills-run-pipeline steps))
    ((string-equal action "save_pipeline")
     (unless name
       (return-from handler "Error: 'name' is required for save_pipeline."))
     (unless steps
       (return-from handler "Error: 'steps' is required for save_pipeline."))
     (crichton/skills:save-pipeline name steps)
     (format nil "Pipeline '~A' saved (~D step~:P). Schedulable as action 'pipeline:~A'."
             name (length steps) name))
    ((string-equal action "delete_pipeline")
     (unless name
       (return-from handler "Error: 'name' is required for delete_pipeline."))
     (if (crichton/skills:delete-pipeline name)
         (format nil "Pipeline '~A' deleted." name)
         (format nil "Pipeline '~A' not found." name)))
    ((string-equal action "list_pipelines")
     (let ((pipelines (crichton/skills:list-saved-pipelines)))
       (if pipelines
           (with-output-to-string (s)
             (format s "Saved pipelines:~%")
             (dolist (p pipelines)
               (format s "  ~A (~D step~:P) — schedulable as 'pipeline:~A'~%"
                       (getf p :name) (getf p :step-count) (getf p :name))))
           "No saved pipelines.")))
    ((string-equal action "refresh")
     (let ((count (crichton/skills:discover-skills)))
       (format nil "Discovered ~D skill~:P." count)))
    (t
     (format nil "Unknown skills action: ~A" action))))

;;; --- Org-mode tool ---

(define-tool orgmode
    (:description "Read, search, create, and manage org-mode files and org-roam notes.  LOCAL-ONLY: requires [orgmode] config.  Actions: 'read' (parse a file), 'search' (find notes by title/tag), 'list_tags' (org-roam tags), 'backlinks' (graph links), 'create_note' (new org-roam note), 'append' (add text to a file), 'list_files' (enumerate org files), 'status' (skill config).")
  ((action "string"
           "The orgmode action to perform."
           :enum ("read" "search" "list_tags" "backlinks" "create_note" "append" "list_files" "status")
           :required-p t)
   (path "string"
         "File path or org-roam node ID (UUID).  Required for read, backlinks, append.")
   (query "string"
          "Search query string.  Required for search.")
   (tag "string"
        "Filter by org-roam tag.  Used with search.")
   (title "string"
          "Note title.  Required for create_note.")
   (body "string"
         "Note body content.  Used with create_note.")
   (filetags "array"
             "File-level tags for the new note.  Used with create_note.")
   (root "string"
         "Root directory for create_note or list_files.  Must be in allowed_paths.")
   (text "string"
         "Text to append.  Required for append.")
   (headline "string"
             "Headline title to append under.  Used with append.")
   (direction "string"
              "Link direction for backlinks: 'backlinks' (default), 'forward', 'both'."
              :enum ("backlinks" "forward" "both"))
   (include-raw "boolean"
                "Include raw file text in read results.")
   (limit "integer"
          "Maximum results to return."
          :default 50))
  (handler-case
      (cond
        ((string-equal action "status")
         (format nil "~S" (crichton/skills:orgmode-status)))
        ((string-equal action "read")
         (unless path
           (return-from handler "Error: 'path' is required for read."))
         (format nil "~S" (crichton/skills:orgmode-read path :include-raw include-raw)))
        ((string-equal action "search")
         (unless query
           (return-from handler "Error: 'query' is required for search."))
         (let ((results (crichton/skills:orgmode-search query :tag tag :limit limit)))
           (format nil "~D result~:P:~%~{~S~^~%~}" (length results) results)))
        ((string-equal action "list_tags")
         (let ((tags (crichton/skills:orgmode-list-tags)))
           (format nil "~D tag~:P: ~{~A~^, ~}" (length tags) tags)))
        ((string-equal action "backlinks")
         (unless path
           (return-from handler "Error: 'path' is required for backlinks."))
         (let* ((dir-kw (cond
                          ((or (null direction) (string-equal direction "backlinks")) :backlinks)
                          ((string-equal direction "forward") :forward)
                          ((string-equal direction "both") :both)
                          (t :backlinks)))
                (results (crichton/skills:orgmode-backlinks path :direction dir-kw :limit limit)))
           (format nil "~D link~:P:~%~{~S~^~%~}" (length results) results)))
        ((string-equal action "create_note")
         (unless title
           (return-from handler "Error: 'title' is required for create_note."))
         (let ((ft-list (when filetags (coerce filetags 'list))))
           (let ((created-path (crichton/skills:orgmode-create-note title
                                 :root root :body body :filetags ft-list)))
             (format nil "Created: ~A" created-path))))
        ((string-equal action "append")
         (unless path
           (return-from handler "Error: 'path' is required for append."))
         (unless text
           (return-from handler "Error: 'text' is required for append."))
         (let ((result (crichton/skills:orgmode-append path text :headline headline)))
           (format nil "Appended to: ~A" result)))
        ((string-equal action "list_files")
         (let ((results (crichton/skills:orgmode-list-files :root root :limit limit)))
           (format nil "~D file~:P:~%~{~S~^~%~}" (length results) results)))
        (t (format nil "Unknown orgmode action: ~A" action)))
    (error (c)
      (format nil "Orgmode error: ~A" c))))

;;; --- Registration ---

(defun register-all-tools ()
  "Register all built-in tools for agent use."
  (register-weather-tool)
  (register-system-info-tool)
  (register-scheduler-tool)
  (register-time-tool)
  (register-ephemeris-tool)
  (register-rss-tool)
  (register-usage-tool)
  (register-battery-tool)
  (register-log-inspector-tool)
  (register-amp-check-tool)
  (register-amp-code-tool)
  (register-amp-test-tool)
  (register-amp-check-json-tool)
  (register-amp-code-json-tool)
  (register-amp-test-json-tool)
  (register-skills-tool)
  (register-memory-write-tool)
  (register-memory-search-tool)
  (register-raindrop-tool)
  (register-orgmode-tool)
  ;; Register pipeline built-in functions
  (crichton/skills:register-default-pipeline-builtins)
  (log:info "Registered ~D agent tools" (hash-table-count *agent-tools*)))
