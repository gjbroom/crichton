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
   Returns a result string. On error, returns an error description string."
  (let ((tool (get-tool name)))
    (unless tool
      (return-from dispatch-tool
        (format nil "Error: unknown tool ~S" name)))
    (handler-case
        (funcall (agent-tool-handler tool) input)
      (error (c)
        (format nil "Error executing ~A: ~A" name c)))))

;;; --- Helper: hash-table key access with default ---

(defun hget (ht key &optional default)
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

;;; --- Weather tool ---

(defun register-weather-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("city" "string"
         "Canadian city name for weather lookup. Default: configured city or Victoria."))
    (register-tool
     "weather"
     "Get current weather conditions and forecast for a Canadian city. Uses Environment Canada data. Provides temperature, humidity, wind, pressure, and multi-day forecast."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((city (hget input "city")))
         (with-output-to-string (s)
           (crichton/skills:weather-report :city city :stream s)))))))

;;; --- System info tool ---

(defun register-system-info-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("include_load" "boolean" "Include CPU load average. Default: true.")
       '("include_memory" "boolean" "Include memory statistics. Default: true.")
       '("include_thermal" "boolean" "Include thermal zone temperatures. Default: true.")
       '("include_disk" "boolean" "Include disk usage. Default: true."))
    (declare (ignore required))
    (register-tool
     "system_info"
     "Get current system health metrics: CPU load average, memory usage, thermal zones, and disk usage. Linux only (reads /proc and /sys). Use this to check if the system is healthy or under stress."
     (make-json-schema :type "object" :properties props)
     (lambda (input)
       (declare (ignore input))
       (with-output-to-string (s)
         (crichton/skills:system-report
          :stream s
          :mounts '("/" "/home")))))))

;;; --- Scheduler tool ---

(defun register-scheduler-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("action" "string"
         "The scheduler action to perform."
         :enum ("status" "list" "actions" "schedule_every" "schedule_daily" "schedule_once" "cancel")
         :required-p t)
       '("action_name" "string"
         "Name of the schedulable action to run (use 'actions' to list available). Required for schedule_every, schedule_daily, schedule_once.")
       '("interval_seconds" "integer"
         "Interval in seconds for schedule_every. Required for schedule_every.")
       '("hour" "integer"
         "Hour (0-23) for schedule_daily. Required for schedule_daily.")
       '("minute" "integer"
         "Minute (0-59) for schedule_daily. Required for schedule_daily.")
       '("delay_seconds" "integer"
         "Delay in seconds from now for schedule_once. Required for schedule_once.")
       '("name" "string"
         "Task name. Auto-generated with 'user:' prefix if omitted."))
    (register-tool
     "scheduler"
     "Manage the daemon's task scheduler. Actions: 'status' (overview), 'list' (all tasks), 'actions' (list schedulable actions), 'schedule_every' (recurring task), 'schedule_daily' (daily at specific time), 'schedule_once' (one-shot after delay), 'cancel' (remove a task by name)."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (block handler
         (let ((action (hget input "action" "status")))
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
              (let* ((action-name (hget input "action_name"))
                     (interval (hget input "interval_seconds"))
                     (task-name (or (hget input "name")
                                    (format nil "user:~A" action-name))))
                (unless action-name
                  (return-from handler "Error: 'action_name' is required. Use action 'actions' to list available."))
                (unless interval
                  (return-from handler "Error: 'interval_seconds' is required."))
                (let ((act (crichton/skills:get-schedulable-action action-name)))
                  (unless act
                    (return-from handler (format nil "Error: unknown action '~A'. Use action 'actions' to list available." action-name)))
                  (crichton/skills:schedule-every task-name interval (getf act :fn)
                                                   :replace t :action-name action-name)
                  (format nil "Scheduled '~A' every ~Ds as task '~A'." action-name interval task-name))))
             ((string-equal action "schedule_daily")
              (let* ((action-name (hget input "action_name"))
                     (hour (hget input "hour"))
                     (minute (hget input "minute" 0))
                     (task-name (or (hget input "name")
                                    (format nil "user:~A" action-name))))
                (unless action-name
                  (return-from handler "Error: 'action_name' is required."))
                (unless hour
                  (return-from handler "Error: 'hour' is required for schedule_daily."))
                (let ((act (crichton/skills:get-schedulable-action action-name)))
                  (unless act
                    (return-from handler (format nil "Error: unknown action '~A'." action-name)))
                  (crichton/skills:schedule-daily task-name hour minute (getf act :fn)
                                                   :replace t :action-name action-name)
                  (format nil "Scheduled '~A' daily at ~2,'0D:~2,'0D as task '~A'." action-name hour minute task-name))))
             ((string-equal action "schedule_once")
              (let* ((action-name (hget input "action_name"))
                     (delay (hget input "delay_seconds"))
                     (task-name (or (hget input "name")
                                    (format nil "user:~A" action-name))))
                (unless action-name
                  (return-from handler "Error: 'action_name' is required."))
                (unless delay
                  (return-from handler "Error: 'delay_seconds' is required."))
                (let ((act (crichton/skills:get-schedulable-action action-name)))
                  (unless act
                    (return-from handler (format nil "Error: unknown action '~A'." action-name)))
                  (crichton/skills:schedule-at task-name (+ (get-universal-time) delay) (getf act :fn)
                                               :replace t :action-name action-name)
                  (format nil "Scheduled '~A' to run once in ~Ds as task '~A'." action-name delay task-name))))
             ((string-equal action "cancel")
              (let ((task-name (hget input "name")))
                (unless task-name
                  (return-from handler "Error: 'name' is required for cancel."))
                (if (crichton/skills:cancel-task task-name)
                    (format nil "Cancelled task '~A'." task-name)
                    (format nil "No task found with name '~A'." task-name))))
             (t
              (format nil "Unknown scheduler action: ~A" action)))))))))

;;; --- Current time tool ---

(defun register-time-tool ()
  (register-tool
   "time"
   "Get the current date and time. Returns the current date and time in both human-readable and Unix timestamp formats. Useful for understanding when events happen relative to the current moment."
   (make-json-schema :type "object" :properties (make-hash-table :test #'equal))
   (lambda (input)
     (declare (ignore input))
     (with-output-to-string (s)
       (crichton/skills:current-time-report :stream s)
       (let ((pl (crichton/skills:current-time-plist)))
         (format s "Unix timestamp: ~D~%" (getf pl :unix-seconds)))))))

;;; --- Ephemeris tool (solar + lunar) ---

(defun register-ephemeris-tool ()
  (register-tool
   "ephemeris"
   "Get solar and lunar ephemeris data for today. Includes sunrise, sunset, solar noon, day length, lunar phase, and illumination percentage. Uses the configured location. No input required."
   (make-json-schema :type "object" :properties (make-hash-table :test #'equal))
   (lambda (input)
     (declare (ignore input))
     (let ((lat (crichton/config:config-section-get :location :latitude 48.43))
           (lon (crichton/config:config-section-get :location :longitude -123.37)))
       (with-output-to-string (s)
         (crichton/skills:ephemeris-report lat lon :stream s))))))

;;; --- RSS tool ---

(defun register-rss-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("action" "string"
         "The RSS action: fetch (get all items), check (get new items only), monitor_start (begin periodic monitoring), monitor_stop (stop monitoring), list_monitors (show active monitors)."
         :enum ("fetch" "check" "monitor_start" "monitor_stop" "list_monitors")
         :required-p t)
       '("url" "string"
         "The RSS/Atom feed URL. Required for fetch, check, monitor_start.")
       '("name" "string"
         "Monitor name (for monitor_start/monitor_stop). Conventionally 'rss:something'.")
       '("interval_seconds" "integer"
         "Polling interval in seconds for monitor_start. Default: 3600 (1 hour)."))
    (register-tool
     "rss"
     "Fetch, check, and monitor RSS/Atom feeds. Use 'fetch' to read all items from a feed, 'check' to see only new items since last check, 'monitor_start' to begin periodic monitoring, 'monitor_stop' to stop a monitor, or 'list_monitors' to see active monitors."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((action (hget input "action"))
             (url (hget input "url"))
             (name (hget input "name"))
             (interval (hget input "interval_seconds" 3600)))
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
                (let ((task-name (or name (format nil "rss:~A" url))))
                  (crichton/skills:rss-monitor-start task-name url interval)
                  (format nil "Monitor started: ~A (every ~Ds)" task-name interval))))
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
           (t (format nil "Unknown RSS action: ~A" action))))))))

;;; --- Battery tool ---

(defun register-battery-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("action" "string"
         "The battery action: status (current battery info), start_monitoring (enable periodic checks), stop_monitoring (disable periodic checks)."
         :enum ("status" "start_monitoring" "stop_monitoring")
         :required-p t))
    (register-tool
     "battery"
     "Monitor battery level and charging status on laptop systems. Get current battery percentage, charging state, and time remaining. Can start/stop periodic monitoring with proactive alerts at configurable thresholds. Linux only (reads /sys/class/power_supply/)."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((action (hget input "action" "status")))
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
            (format nil "Unknown battery action: ~A" action))))))))

;;; --- Token/resource usage tool ---

(defun register-usage-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("action" "string"
         "The action: summary (aggregate across all meters), meter (single meter detail), list (list meter names), recent (recent calls for a meter)."
         :enum ("summary" "meter" "list" "recent")
         :required-p t)
       '("meter" "string"
         "Meter name (for 'meter' and 'recent' actions). e.g. 'llm'.")
       '("count" "integer"
         "Number of recent records to return (for 'recent'). Default: 10."))
    (register-tool
     "resource_usage"
     "Query metered resource usage, burn rate, and cost estimates. Tracks any service that consumes tokens or API calls (LLM, weather API, etc.). Use 'summary' for aggregate view, 'meter' for one service, 'list' to see meter names, 'recent' for call history."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((action (hget input "action" "summary"))
             (meter-name (hget input "meter"))
             (count (hget input "count" 10)))
         (cond
           ((string-equal action "summary")
            (with-output-to-string (s)
              (crichton/skills:usage-report :stream s)))
           ((string-equal action "meter")
            (if (null meter-name)
                "Error: 'meter' parameter required for meter action"
                (with-output-to-string (s)
                  (crichton/skills:meter-report meter-name :stream s))))
           ((string-equal action "list")
            (let ((names (crichton/skills:list-meters)))
              (if names
                  (format nil "Active meters: ~{~A~^, ~}" names)
                  "No active meters.")))
           ((string-equal action "recent")
            (if (null meter-name)
                "Error: 'meter' parameter required for recent action"
                (format nil "~{~S~^~%~}"
                        (crichton/skills:meter-recent meter-name count))))
           (t (format nil "Unknown action: ~A" action))))))))


;;; --- Daemon log inspector tool ---

(defun format-log-entries (entries)
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

(defun register-log-inspector-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("action" "string"
         "The log inspection action to perform."
         :enum ("summary" "recent" "errors" "search")
         :required-p t)
       '("count" "integer"
         "Number of log entries to examine. Default: 50.")
       '("pattern" "string"
         "Search substring for the 'search' action. Case-insensitive match against message text.")
       '("level" "string"
         "Filter by log level (ERROR, WARN, INFO, DEBUG). Optional for 'search' action."
         :enum ("ERROR" "WARN" "INFO" "DEBUG")))
    (register-tool
     "daemon_logs"
     "Inspect the daemon's own log files. Use to check for errors, warnings, or verify that systems are functioning correctly. Logs are sanitized — safe to share with users."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((action (hget input "action" "summary"))
             (count (hget input "count" 50))
             (pattern (hget input "pattern"))
             (level (hget input "level")))
         (cond
           ((string-equal action "summary")
            (with-output-to-string (s)
              (crichton/skills:log-report :stream s :count count)))
           ((string-equal action "recent")
            (format-log-entries
             (crichton/skills:read-log-tail :count count)))
           ((string-equal action "errors")
            (format-log-entries
             (crichton/skills:search-log :level "ERROR" :count count)))
           ((string-equal action "search")
            (if (and (null pattern) (null level))
                "Error: 'pattern' and/or 'level' required for search action."
                (format-log-entries
                 (crichton/skills:search-log :pattern pattern
                                             :level level
                                             :count count))))
           (t
            (format nil "Unknown daemon_logs action: ~A" action))))))))

;;; --- Amp orchestration tools ---

(defun register-amp-check-tool ()
  (register-tool
   "amp_check"
   "Check whether the Amp CLI coding agent is available on this system. Returns availability status. Use this before attempting amp_code or amp_test tasks."
   (make-json-schema :type "object" :properties (make-hash-table :test #'equal))
   (lambda (input)
     (declare (ignore input))
     (if (crichton/skills:amp-available-p)
         "Amp CLI is available and ready for coding/testing tasks."
         "Amp CLI is NOT available. Install it to enable code delegation."))))

(defun register-amp-code-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("description" "string"
         "Description of the coding task to delegate to Amp. Be specific about what code to write or modify."
         :required-p t)
       '("repo_path" "string"
         "Absolute path to the repository to work in. Amp will run with this as the working directory.")
       '("files" "string"
         "Comma-separated list of file paths to focus on (relative to repo_path).")
       '("context" "string"
         "Additional context for the task (e.g., coding standards, constraints, existing patterns).")
       '("timeout_seconds" "integer"
         "Maximum time in seconds to wait for Amp to complete. Default: 300 (5 minutes)."))
    (register-tool
     "amp_code"
     "Delegate a coding task to the Amp CLI agent. Amp is an AI coding assistant that can read, write, and modify code files. Use this for implementation tasks: writing new functions, fixing bugs, refactoring code, adding features. Requires Amp CLI to be installed (check with amp_check first)."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((description (hget input "description"))
             (repo-path (hget input "repo_path"))
             (files-str (hget input "files"))
             (context (hget input "context"))
             (timeout (hget input "timeout_seconds" 300)))
         (let ((files (when files-str
                        (mapcar (lambda (f)
                                  (string-trim '(#\Space) f))
                                (cl-ppcre:split "," files-str)))))
           (let ((result (crichton/skills:amp-code-task
                          description
                          :repo-path repo-path
                          :files files
                          :context context
                          :timeout-seconds timeout)))
             (with-output-to-string (s)
               (crichton/skills:amp-report result :stream s)))))))))

(defun register-amp-test-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("test_command" "string"
         "The shell command to run tests (e.g., 'make test', 'pytest', 'cargo test')."
         :required-p t)
       '("repo_path" "string"
         "Absolute path to the repository. Tests run with this as the working directory.")
       '("fix_failures" "boolean"
         "If true, invoke Amp to fix test failures automatically. Default: false.")
       '("max_iterations" "integer"
         "Maximum test/fix cycles when fix_failures is true. Default: 3.")
       '("timeout_seconds" "integer"
         "Maximum time in seconds per Amp fix invocation. Default: 300."))
    (register-tool
     "amp_test"
     "Run a test suite and optionally use the Amp CLI agent to fix failures. First runs the test command directly. If tests fail and fix_failures is true, Amp is invoked with the failure context to attempt repairs, iterating up to max_iterations. Requires Amp CLI for fix mode (check with amp_check)."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((test-command (hget input "test_command"))
             (repo-path (hget input "repo_path"))
             (fix-failures (hget input "fix_failures"))
             (max-iterations (hget input "max_iterations" 3))
             (timeout (hget input "timeout_seconds" 300)))
         (let ((result (crichton/skills:amp-test-task
                        test-command
                        :repo-path repo-path
                        :fix-failures fix-failures
                        :max-iterations max-iterations
                        :timeout-seconds timeout)))
           (with-output-to-string (s)
             (crichton/skills:amp-report result :stream s))))))))

;;; --- Skills tool ---

(defun register-skills-tool ()
  (multiple-value-bind (props required)
      (make-properties
       '("action" "string"
         "The skills action to perform."
         :enum ("list" "info" "invoke" "refresh")
         :required-p t)
       '("name" "string"
         "Skill name. Required for info and invoke."))
    (register-tool
     "skills"
     "Manage external WASM skills. Actions: 'list' (show all discovered skills), 'info' (get details for a specific skill), 'invoke' (run a skill), 'refresh' (re-scan skills directory). Skills are discovered from ~/.crichton/skills/ and can be scheduled using the scheduler tool."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (block handler
         (let ((action (hget input "action" "list")))
           (cond
             ((string-equal action "list")
              (crichton/skills:discover-skills)  ; refresh before listing
              (with-output-to-string (s)
                (crichton/skills:skill-report :stream s)))
             ((string-equal action "info")
              (let* ((name (hget input "name"))
                     (info (crichton/skills:skill-info name)))
                (unless name
                  (return-from handler "Error: 'name' is required for info action."))
                (if info
                    (format nil "~S" info)
                    (format nil "Skill '~A' not found." name))))
             ((string-equal action "invoke")
              (let ((name (hget input "name")))
                (unless name
                  (return-from handler "Error: 'name' is required for invoke action."))
                (handler-case
                    (let ((result (crichton/skills:invoke-skill name)))
                      (format nil "Skill '~A' returned: ~A" name result))
                  (error (c)
                    (format nil "Error invoking skill '~A': ~A" name c)))))
             ((string-equal action "refresh")
              (let ((count (crichton/skills:discover-skills)))
                (format nil "Discovered ~D skill~:P." count)))
             (t
              (format nil "Unknown skills action: ~A" action)))))))))

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
  (register-skills-tool)
  (log:info "Registered ~D agent tools" (hash-table-count *agent-tools*)))
