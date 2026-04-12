;;;; agent/tools-system.lisp
;;;;
;;;; System-facing tool definitions: weather, system-info, scheduler,
;;;; time, ephemeris, RSS feeds, battery, and resource usage.

(in-package #:crichton/agent)

;;; --- Weather tool ---

(define-tool weather
    (:description "Get current weather conditions and forecast for a Canadian city.  Uses Environment Canada data.  Provides temperature, humidity, wind, pressure, and multi-day forecast.")
  ((city "string"
         "Canadian city name for weather lookup. Default: configured city or Victoria."))
  (with-output-to-string (s)
    (crichton/skills:weather-report :city city :stream s)))

;;; --- System info tool ---

(define-tool system-info
    (:description "Get current system health metrics and manage continuous monitoring.  Metrics: CPU load average, memory usage, disk usage, and thermal zone temperatures.  Linux only (reads /proc and /sys).  Can start/stop background monitoring that sends push notifications when thresholds are crossed (configurable via [system] in config.toml).")
  ((action "string"
           "The action: 'status' (get current metrics, default), 'start_monitoring' (enable periodic threshold alerts), 'stop_monitoring' (disable monitoring)."
           :enum ("status" "start_monitoring" "stop_monitoring")
           :default "status"))
  (let ((a (or action "status")))
    (cond
      ((string-equal a "status")
       (with-output-to-string (s)
         (format s "Crichton ~A~%" crichton/config:*crichton-version*)
         (crichton/skills:system-report :stream s :mounts '("/" "/home"))))
      ((string-equal a "start_monitoring")
       (let* ((config (crichton/skills:system-monitor-config))
              (interval (getf config :interval)))
         (if (crichton/skills:start-system-monitoring :interval interval)
             (format nil "System monitoring started (every ~Ds). Will alert when: memory >=~A%, load/CPU >=~,1F, any disk >=~A% full, any thermal zone >=~,1F°C."
                     interval
                     (getf config :mem-alert-percent)
                     (getf config :cpu-alert-load)
                     (getf config :disk-alert-percent)
                     (getf config :temp-alert-celsius))
             "System monitoring is already running.")))
      ((string-equal a "stop_monitoring")
       (if (crichton/skills:stop-system-monitoring)
           "System monitoring stopped."
           "System monitoring was not running."))
      (t (format nil "Unknown system_info action: ~A" a)))))

;;; --- Scheduler tool helpers ---

(defun scheduler-schedule-every (action-name interval-seconds task-name)
  "Handle the schedule_every scheduler action."
  (unless action-name
    (return-from scheduler-schedule-every
      "Error: 'action_name' is required. Use action 'actions' to list available."))
  (unless interval-seconds
    (return-from scheduler-schedule-every "Error: 'interval_seconds' is required."))
  (let ((act (crichton/skills:get-schedulable-action action-name)))
    (unless act
      (return-from scheduler-schedule-every
        (format nil "Error: unknown action '~A'. Use action 'actions' to list available." action-name)))
    (crichton/skills:schedule-every task-name interval-seconds (getf act :fn)
                                    :replace t :action-name action-name)
    (crichton/skills:persist-user-tasks)
    (format nil "Scheduled '~A' every ~Ds as task '~A'." action-name interval-seconds task-name)))

(defun scheduler-schedule-daily (action-name hour minute task-name)
  "Handle the schedule_daily scheduler action."
  (unless action-name
    (return-from scheduler-schedule-daily "Error: 'action_name' is required."))
  (unless hour
    (return-from scheduler-schedule-daily "Error: 'hour' is required for schedule_daily."))
  (let ((act (crichton/skills:get-schedulable-action action-name))
        (min (or minute 0)))
    (unless act
      (return-from scheduler-schedule-daily
        (format nil "Error: unknown action '~A'." action-name)))
    (crichton/skills:schedule-daily task-name hour min (getf act :fn)
                                    :replace t :action-name action-name)
    (crichton/skills:persist-user-tasks)
    (format nil "Scheduled '~A' daily at ~2,'0D:~2,'0D as task '~A'." action-name hour min task-name)))

(defun scheduler-schedule-once (action-name delay-seconds task-name)
  "Handle the schedule_once scheduler action."
  (unless action-name
    (return-from scheduler-schedule-once "Error: 'action_name' is required."))
  (unless delay-seconds
    (return-from scheduler-schedule-once "Error: 'delay_seconds' is required."))
  (let ((act (crichton/skills:get-schedulable-action action-name)))
    (unless act
      (return-from scheduler-schedule-once
        (format nil "Error: unknown action '~A'." action-name)))
    (crichton/skills:schedule-at task-name (+ (get-universal-time) delay-seconds) (getf act :fn)
                                 :replace t :action-name action-name)
    (crichton/skills:persist-user-tasks)
    (format nil "Scheduled '~A' to run once in ~Ds as task '~A'." action-name delay-seconds task-name)))

;;; --- Scheduler tool ---

(define-tool scheduler
    (:description "Manage the daemon's task scheduler.  Actions: 'status' (overview), 'list' (all tasks), 'actions' (list schedulable actions), 'schedule_every' (recurring built-in action), 'schedule_daily' (daily built-in action), 'schedule_once' (one-shot built-in action), 'schedule_prompt_every' (run an agent prompt on a recurring interval), 'schedule_prompt_daily' (run an agent prompt daily at a set time), 'schedule_prompt_once' (run an agent prompt once after a delay), 'cancel' (remove a task by name), 'persist' (manually save user tasks to disk), 'list_unrestorable' (show tasks that would be lost on restart).")
  ((action "string"
           "The scheduler action to perform."
           :enum ("status" "list" "actions" "schedule_every" "schedule_daily" "schedule_once" "schedule_prompt_every" "schedule_prompt_daily" "schedule_prompt_once" "cancel" "persist" "list_unrestorable")
           :required-p t)
   (action-name "string"
                "Name of the schedulable action to run (use 'actions' to list available). Required for schedule_every, schedule_daily, schedule_once.")
   (prompt "string"
           "Agent prompt to run on schedule. Required for schedule_prompt_every, schedule_prompt_daily, schedule_prompt_once.")
   (interval-seconds "integer"
                     "Interval in seconds for schedule_every / schedule_prompt_every. Required for those actions.")
   (hour "integer"
         "Hour (0-23) for schedule_daily / schedule_prompt_daily. Required for those actions.")
   (minute "integer"
           "Minute (0-59) for schedule_daily / schedule_prompt_daily. Required for those actions.")
   (delay-seconds "integer"
                  "Delay in seconds from now for schedule_once / schedule_prompt_once. Required for those actions.")
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
       (scheduler-schedule-every action-name interval-seconds task-name))
      ((string-equal action "schedule_daily")
       (scheduler-schedule-daily action-name hour minute task-name))
      ((string-equal action "schedule_once")
       (scheduler-schedule-once action-name delay-seconds task-name))
      ((string-equal action "schedule_prompt_every")
       (unless prompt
         (return-from handler "Error: 'prompt' is required for schedule_prompt_every."))
       (unless interval-seconds
         (return-from handler "Error: 'interval_seconds' is required for schedule_prompt_every."))
       (let ((task-name (or name (format nil "user:prompt-~A" (crichton/rpc:next-id)))))
         (crichton/skills:schedule-prompt-every task-name interval-seconds prompt :replace t)
         (crichton/skills:persist-user-tasks)
         (format nil "Scheduled prompt task '~A' every ~Ds." task-name interval-seconds)))
      ((string-equal action "schedule_prompt_daily")
       (unless prompt
         (return-from handler "Error: 'prompt' is required for schedule_prompt_daily."))
       (unless hour
         (return-from handler "Error: 'hour' is required for schedule_prompt_daily."))
       (let ((task-name (or name (format nil "user:prompt-~A" (crichton/rpc:next-id))))
             (min (or minute 0)))
         (crichton/skills:schedule-prompt-daily task-name hour min prompt :replace t)
         (crichton/skills:persist-user-tasks)
         (format nil "Scheduled prompt task '~A' daily at ~2,'0D:~2,'0D." task-name hour min)))
      ((string-equal action "schedule_prompt_once")
       (unless prompt
         (return-from handler "Error: 'prompt' is required for schedule_prompt_once."))
       (unless delay-seconds
         (return-from handler "Error: 'delay_seconds' is required for schedule_prompt_once."))
       (let ((task-name (or name (format nil "user:prompt-~A" (crichton/rpc:next-id)))))
         (crichton/skills:schedule-prompt-at task-name (+ (get-universal-time) delay-seconds) prompt :replace t)
         (crichton/skills:persist-user-tasks)
         (format nil "Scheduled prompt task '~A' to run once in ~Ds." task-name delay-seconds)))
      ((string-equal action "cancel")
       (unless name
         (return-from handler "Error: 'name' is required for cancel."))
       (if (crichton/skills:cancel-task name)
           (progn
             (crichton/skills:persist-user-tasks)
             (format nil "Cancelled task '~A'." name))
           (format nil "No task found with name '~A'." name)))
      ((string-equal action "persist")
       (let ((count (crichton/skills:persist-user-tasks)))
         (format nil "Persisted ~D user task~:P to encrypted storage." count)))
      ((string-equal action "list_unrestorable")
       (let ((tasks (crichton/skills:list-unrestorable-tasks)))
         (if tasks
             (with-output-to-string (s)
               (format s "~D task~:P would be lost on restart (action not registered):~%"
                       (length tasks))
               (dolist (task tasks)
                 (format s "  ~A (action: ~A, kind: ~A)~%"
                         (getf task :name)
                         (getf task :action-name)
                         (getf task :kind))))
             "All persisted user tasks have registered actions — none would be lost on restart.")))
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
    (:description "Fetch, check, and monitor RSS/Atom feeds; or write/publish your own RSS feeds.
Reading: 'fetch' reads all items from a URL, 'check' returns only new items since last check, 'monitor_start' starts periodic polling, 'monitor_stop' stops a monitor, 'list_monitors' shows active monitors with backoff status.
Muting: 'mute_monitor' silences a feed indefinitely (skips polls, keeps registration); 'unmute_monitor' resumes it and resets the failure counter.
Bulk import/export: 'opml_import' parses an OPML file and registers all feeds as monitors in a single call — use this instead of looping monitor_start.  'opml_export' generates an OPML 2.0 file from all registered monitors.
Writing: 'publish_item' adds an item to a named feed (creating it implicitly), 'configure_feed' sets feed metadata (title/description/link/max-items), 'get_feed_xml' returns the RSS 2.0 XML for serving, 'list_feed_items' shows the current item list, 'list_feeds' shows all feeds, 'clear_feed' removes all items (keeps config), 'delete_feed' removes feed entirely.
Monitors automatically back off on failure (exponential, capped at 7 days) and post a notification after 10 consecutive failures.  Monitors can filter items by keywords via the rss-filter WASM skill.  Published feeds persist across restarts via encrypted storage.")
  ((action "string"
           "The RSS action: fetch, check, monitor_start, monitor_stop, list_monitors, mute_monitor, unmute_monitor, opml_import, opml_export, publish_item, configure_feed, get_feed_xml, list_feed_items, list_feeds, clear_feed, delete_feed."
           :enum ("fetch" "check" "monitor_start" "monitor_stop" "list_monitors"
                  "mute_monitor" "unmute_monitor"
                  "opml_import" "opml_export"
                  "publish_item" "configure_feed" "get_feed_xml"
                  "list_feed_items" "list_feeds" "clear_feed" "delete_feed")
           :required-p t)
   (url "string"
        "The RSS/Atom feed URL. Required for fetch, check, monitor_start.")
   (file-path "string"
              "Absolute path to an OPML file on disk.  Required for opml_import; optional for opml_export (omit to get XML as a string).")
   (name "string"
         "Feed name for writing actions (publish_item, configure_feed, get_feed_xml, etc.), or monitor name for monitor_start/monitor_stop, or OPML title for opml_export.  For monitors, conventionally 'rss:something'.")
   (interval-seconds "integer"
                     "Polling interval in seconds for monitor_start. Default: 3600 (1 hour)."
                     :default 3600)
   (keywords "array"
             "Optional list of keyword strings to filter items through the rss-filter WASM skill.  Only items matching these keywords will be reported.  Requires the rss-filter skill to be installed.")
   (match-mode "string"
               "Keyword matching mode: 'any' (default, match if any keyword found) or 'all' (match only if all keywords found)."
               :enum ("any" "all"))
   (search-fields "array"
                  "Which item fields to search for keywords.  Default: [\"title\", \"description\"].  Valid values: title, description, content.")
   (title "string"
          "Item title (for publish_item) or feed title (for configure_feed).")
   (description "string"
                "Item description/body (for publish_item) or feed description (for configure_feed).")
   (link "string"
         "Item URL (for publish_item) or feed homepage URL (for configure_feed).")
   (guid "string"
         "Explicit GUID for publish_item.  Auto-generated if omitted.")
   (pub-date "string"
             "Publication date in RFC 822 format for publish_item.  Defaults to current time.")
   (max-items "integer"
              "Maximum number of items to retain for configure_feed.  Oldest items are dropped.  Default: 100."))
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
    ((string-equal action "opml_import")
     (if (null file-path)
         "Error: 'file_path' is required for opml_import"
         (handler-case
             (crichton/skills:opml-import-monitors
              file-path
              :interval-seconds interval-seconds)
           (error (c)
             (format nil "OPML import failed: ~A" c)))))
    ((string-equal action "opml_export")
     (handler-case
         (crichton/skills:opml-export-monitors
          :file-path file-path
          :title (or name "RSS Feeds"))
       (error (c)
         (format nil "OPML export failed: ~A" c))))
    ((string-equal action "monitor_stop")
     (let ((task-name (or name "?")))
       (if (crichton/skills:rss-monitor-stop task-name)
           (format nil "Monitor stopped: ~A" task-name)
           (format nil "Monitor not found: ~A" task-name))))
    ((string-equal action "list_monitors")
     (let ((configs (crichton/skills:rss-monitor-configs)))
       (if configs
           (with-output-to-string (s)
             (format s "~D active RSS monitor~:P:~%" (length configs))
             (let ((now (get-universal-time)))
               (dolist (cfg configs)
                 (let* ((name        (getf cfg :name))
                        (url         (getf cfg :url))
                        (interval    (getf cfg :interval-seconds))
                        (failures    (or (getf cfg :consecutive-failures) 0))
                        (muted-until  (getf cfg :muted-until))
                        (user-muted   (getf cfg :user-muted))
                        (last-failure (getf cfg :last-failure)))
                   (format s "  ~A~%" name)
                   (format s "    ~A  (every ~Ds)~%" url interval)
                   (cond
                     (user-muted
                      (format s "    Status: user-muted~%"))
                     ((and muted-until (> muted-until now))
                      (format s "    Status: backoff ~Ds remaining (~D failure~:P)~%"
                              (- muted-until now) failures))
                     ((plusp failures)
                      (format s "    Status: ok (~D prior failure~:P, recovered)~%"
                              failures))
                     (t
                      (format s "    Status: ok~%")))
                   (when last-failure
                     (format s "    Last error: ~A~%" last-failure))))))
           "No active RSS monitors.")))
    ((string-equal action "mute_monitor")
     (let ((task-name (or name "?")))
       (handler-case
           (crichton/skills:rss-monitor-mute task-name)
         (error (c) (format nil "Error: ~A" c)))))
    ((string-equal action "unmute_monitor")
     (let ((task-name (or name "?")))
       (handler-case
           (crichton/skills:rss-monitor-unmute task-name)
         (error (c) (format nil "Error: ~A" c)))))
    ;; --- Feed writing actions ---
    ((string-equal action "publish_item")
     (if (null name)
         "Error: 'name' is required for publish_item"
         (let ((guid (crichton/skills:rss-feed-publish
                      name
                      :title       (or title "")
                      :description (or description "")
                      :link        (or link "")
                      :guid        guid
                      :pub-date    pub-date)))
           (format nil "Published item to feed '~A'. GUID: ~A" name guid))))
    ((string-equal action "configure_feed")
     (if (null name)
         "Error: 'name' is required for configure_feed"
         (progn
           (crichton/skills:rss-feed-configure
            name
            :title       title
            :description description
            :link        link
            :max-items   max-items)
           (format nil "Feed '~A' configured." name))))
    ((string-equal action "get_feed_xml")
     (if (null name)
         "Error: 'name' is required for get_feed_xml"
         (crichton/skills:rss-feed-xml name)))
    ((string-equal action "list_feed_items")
     (if (null name)
         "Error: 'name' is required for list_feed_items"
         (let ((items (crichton/skills:rss-feed-items name)))
           (if items
               (with-output-to-string (s)
                 (format s "Feed '~A': ~D item~:P~%" name (length items))
                 (dolist (item items)
                   (format s "  ~A~%" (getf item :title))
                   (when (plusp (length (getf item :link)))
                     (format s "    ~A~%" (getf item :link)))
                   (format s "    ~A~%" (getf item :pub-date))))
               (format nil "Feed '~A' has no items." name)))))
    ((string-equal action "list_feeds")
     (let ((feeds (crichton/skills:rss-feed-list)))
       (if feeds
           (format nil "Published feeds:~%~{  ~A~%~}" feeds)
           "No published feeds configured.")))
    ((string-equal action "clear_feed")
     (if (null name)
         "Error: 'name' is required for clear_feed"
         (progn
           (crichton/skills:rss-feed-clear name)
           (format nil "Feed '~A' items cleared." name))))
    ((string-equal action "delete_feed")
     (if (null name)
         "Error: 'name' is required for delete_feed"
         (progn
           (crichton/skills:rss-feed-delete name)
           (format nil "Feed '~A' deleted." name))))
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
