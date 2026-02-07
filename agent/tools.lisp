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

(defstruct (agent-tool (:constructor %make-agent-tool))
  (name "" :type string)
  (description "" :type string)
  (input-schema nil)
  (handler nil :type (or null function)))

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
         :enum ("status" "list")
         :required-p t))
    (register-tool
     "scheduler"
     "View the status of the daemon's task scheduler and list scheduled tasks. Use action 'status' for overview or 'list' for all tasks."
     (make-json-schema :type "object" :properties props :required required)
     (lambda (input)
       (let ((action (hget input "action" "status")))
         (cond
           ((string-equal action "status")
            (format nil "~S" (crichton/skills:scheduler-status)))
           ((string-equal action "list")
            (format nil "~S" (crichton/skills:list-tasks)))
           (t
            (format nil "Unknown scheduler action: ~A" action))))))))

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

;;; --- Registration ---

(defun register-all-tools ()
  "Register all built-in tools for agent use."
  (register-weather-tool)
  (register-system-info-tool)
  (register-scheduler-tool)
  (register-rss-tool)
  (register-usage-tool)
  (log:info "Registered ~D agent tools" (hash-table-count *agent-tools*)))
