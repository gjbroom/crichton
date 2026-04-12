;;;; skills/builtins/rss-monitor.lisp
;;;;
;;;; RSS monitor registry: periodic polling with adaptive backoff,
;;;; WASM filter integration, mute/unmute, and OPML import.
;;;; Depends on rss.lisp for feed-item class, fetching, and seen-item state.

(in-package #:crichton/skills)

;;; --- Monitor registry (survives restarts) ---

(defvar *rss-monitors-lock* (bt:make-lock "rss-monitors"))
(defvar *rss-monitors* (make-hash-table :test #'equal)
  "Registry of active RSS monitors: name → plist
(:url :interval-seconds :keywords :match-mode :search-fields
 :consecutive-failures :muted-until :user-muted :last-failure).")

(defparameter *rss-dead-feed-threshold* 10
  "Consecutive failures before a feed is treated as dead and a notification is posted.")

(defparameter *rss-max-backoff-seconds* (* 7 24 3600)
  "Maximum per-failure backoff duration for a failing RSS feed (7 days).")

(defun persist-rss-monitors ()
  "Save the monitor registry to storage so monitors survive restarts."
  (bt:with-lock-held (*rss-monitors-lock*)
    (let ((ht (make-hash-table :test #'equal)))
      (maphash (lambda (name config)
                 (let ((entry (make-hash-table :test #'equal)))
                   (setf (gethash "url" entry) (getf config :url)
                         (gethash "interval_seconds" entry) (getf config :interval-seconds))
                   (when (getf config :keywords)
                     (setf (gethash "keywords" entry) (coerce (getf config :keywords) 'vector)))
                   (when (getf config :match-mode)
                     (setf (gethash "match_mode" entry) (getf config :match-mode)))
                   (when (getf config :search-fields)
                     (setf (gethash "search_fields" entry) (coerce (getf config :search-fields) 'vector)))
                   (let ((failures (getf config :consecutive-failures)))
                     (when (and failures (plusp failures))
                       (setf (gethash "consecutive_failures" entry) failures)))
                   (let ((muted-until (getf config :muted-until)))
                     (when muted-until
                       (setf (gethash "muted_until" entry) muted-until)))
                   (when (getf config :user-muted)
                     (setf (gethash "user_muted" entry) t))
                   (setf (gethash name ht) entry)))
               *rss-monitors*)
      (crichton/storage:store-set "rss" "monitors" ht)
      (crichton/storage:flush-all-storage)))
  (log:debug "Persisted ~D RSS monitor~:P" (hash-table-count *rss-monitors*)))

(defun restore-rss-monitors ()
  "Restore RSS monitors from storage and re-register them with the scheduler.
   Call after start-scheduler during daemon init."
  (let ((data (crichton/storage:store-get "rss" "monitors")))
    (when (and data (hash-table-p data))
      (let ((restored 0) (failed 0))
        (maphash (lambda (name entry)
                   (handler-case
                       (let ((url (gethash "url" entry))
                             (interval (gethash "interval_seconds" entry))
                             (keywords (gethash "keywords" entry))
                             (match-mode (gethash "match_mode" entry))
                             (search-fields (gethash "search_fields" entry))
                             (failures (gethash "consecutive_failures" entry))
                             (muted-until (gethash "muted_until" entry))
                             (user-muted (gethash "user_muted" entry)))
                         (rss-monitor-start name url interval
                                            :keywords (when keywords (coerce keywords 'list))
                                            :match-mode match-mode
                                            :search-fields (when search-fields (coerce search-fields 'list))
                                            :consecutive-failures (or failures 0)
                                            :muted-until muted-until
                                            :user-muted user-muted
                                            :persist nil)
                         (incf restored))
                     (error (c)
                       (log:warn "Failed to restore RSS monitor ~A: ~A" name c)
                       (incf failed))))
                 data)
        (log:info "Restored ~D RSS monitor~:P (~D failed)" restored failed)))))

;;; --- Monitoring integration with scheduler ---

(defun run-rss-filter (items keywords &key (match-mode "any") (search-fields '("title" "description")))
  "Run ITEMS through the rss-filter WASM skill.
   ITEMS is a list of plists (canonical RSS schema).
   Returns the filter result hash-table (with \"matches\" and \"statistics\" keys),
   or NIL if the rss-filter skill is not available."
  (let ((entry (gethash "rss-filter" *skill-registry*)))
    (unless entry
      (log:warn "rss-filter WASM skill not found in registry; skipping filter")
      (return-from run-rss-filter nil))
    (let ((params (make-hash-table :test #'equal)))
      ;; Convert plist items to hash-tables for JSON serialization
      (setf (gethash "items" params)
            (coerce
             (mapcar (lambda (item)
                       (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "id" ht) (or (getf item :id) "")
                               (gethash "title" ht) (or (getf item :title) "")
                               (gethash "description" ht) (or (getf item :description) "")
                               (gethash "link" ht) (or (getf item :link) "")
                               (gethash "published" ht) (or (getf item :published) "")
                               (gethash "feed_name" ht) (or (getf item :feed-name) ""))
                         ht))
                     items)
             'vector))
      (setf (gethash "keywords" params) (coerce keywords 'vector))
      (setf (gethash "match_mode" params) match-mode)
      (setf (gethash "search_fields" params) (coerce search-fields 'vector))
      (invoke-skill "rss-filter" :entry-point "filter_items" :params params))))

(defun rss-monitor-poll (name url &key keywords match-mode search-fields)
  "Poll a single RSS feed, log new items, and post notifications.
Implements adaptive backoff: consecutive failures trigger exponential
back-off up to *rss-max-backoff-seconds*.  After *rss-dead-feed-threshold*
consecutive failures a dead-feed notification is posted.  Polls are
silently skipped while a backoff or user-mute is active.
When KEYWORDS is non-nil, new items are filtered through the rss-filter
WASM skill before reporting."
  ;; Check mute state before doing any network I/O
  (let ((snapshot (bt:with-lock-held (*rss-monitors-lock*)
                    (copy-list (gethash name *rss-monitors*)))))
    (when snapshot
      (when (getf snapshot :user-muted)
        (log:debug "RSS monitor ~A: skipped (user-muted)" name)
        (return-from rss-monitor-poll nil))
      (let ((muted-until (getf snapshot :muted-until)))
        (when (and muted-until (> muted-until (get-universal-time)))
          (log:debug "RSS monitor ~A: backoff active (~Ds remaining)"
                     name (- muted-until (get-universal-time)))
          (return-from rss-monitor-poll nil)))))
  ;; Perform the poll
  (handler-case
      (let ((result (rss-check url)))
        ;; On success: reset failure counter (logged if recovering)
        (bt:with-lock-held (*rss-monitors-lock*)
          (let ((config (gethash name *rss-monitors*)))
            (when config
              (let ((prev (or (getf config :consecutive-failures) 0)))
                (when (plusp prev)
                  (log:info "RSS monitor ~A: recovered after ~D failure~:P" name prev)))
              (setf (getf config :consecutive-failures) 0
                    (getf config :muted-until) nil
                    (getf config :last-failure) nil)
              (setf (gethash name *rss-monitors*) config))))
        ;; Report new items
        (when (plusp (getf result :new-count))
          (let ((report-items (getf result :new-items))
                (report-count (getf result :new-count)))
            ;; Apply WASM filter if keywords configured
            (when keywords
              (let ((filter-result (run-rss-filter report-items keywords
                                                   :match-mode (or match-mode "any")
                                                   :search-fields (or search-fields '("title" "description")))))
                (when filter-result
                  (let ((matches (gethash "matches" filter-result)))
                    (setf report-count (length matches))
                    ;; Convert matched hash-tables back to plists for logging
                    (setf report-items
                          (mapcar (lambda (m)
                                    (list :id (gethash "id" m)
                                          :title (gethash "title" m)
                                          :link (gethash "link" m)
                                          :matched-keywords (gethash "matched_keywords" m)))
                                  (coerce matches 'list)))))))
            (when (plusp report-count)
              (log:info "RSS ~A: ~D ~Aitem~:P" name report-count
                        (if keywords "matching " ""))
              (dolist (item report-items)
                (log:info "  ~A: ~A" name (getf item :title)))
              (crichton/daemon:notification-post
               "rss"
               (format nil "~D ~Aitem~:P in ~A" report-count
                       (if keywords "matching " "") name)
               name)))))
    (error (c)
      ;; On failure: exponential backoff, dead-feed alert at threshold.
      ;; notification-post is called OUTSIDE the lock to avoid lock ordering issues.
      (let (dead-feed-backoff)
        (bt:with-lock-held (*rss-monitors-lock*)
          (let* ((config (gethash name *rss-monitors*))
                 (interval (or (and config (getf config :interval-seconds)) 3600))
                 (failures (1+ (or (and config (getf config :consecutive-failures)) 0)))
                 (backoff (min *rss-max-backoff-seconds*
                               (* interval (expt 2 (min failures 20)))))
                 (muted-until (+ (get-universal-time) backoff)))
            (when config
              (setf (getf config :consecutive-failures) failures
                    (getf config :muted-until) muted-until
                    (getf config :last-failure) (format nil "~A" c))
              (setf (gethash name *rss-monitors*) config))
            (log:warn "RSS monitor ~A failed (~D): ~A — backoff ~Ds"
                      name failures c backoff)
            (when (= failures *rss-dead-feed-threshold*)
              (log:warn "RSS monitor ~A: ~D consecutive failures — feed may be dead"
                        name failures)
              (setf dead-feed-backoff backoff))))
        (when dead-feed-backoff
          (crichton/daemon:notification-post
           "rss"
           (format nil "Feed ~A may be dead (~D consecutive failures); paused ~Ads"
                   name *rss-dead-feed-threshold* dead-feed-backoff)
           name))))))

(defun rss-monitor-start (name url interval-seconds
                          &key (replace t) (persist t)
                               keywords match-mode search-fields
                               (consecutive-failures 0) muted-until user-muted)
  "Start monitoring a feed by scheduling periodic checks.
   NAME is the task name. Results logged via log4cl.
   When KEYWORDS is non-nil, new items are filtered through the rss-filter
   WASM skill before reporting.  MATCH-MODE (\"any\"/\"all\") and SEARCH-FIELDS
   (list of field names) control filter behaviour.
   CONSECUTIVE-FAILURES, MUTED-UNTIL, USER-MUTED restore persisted backoff
   state after a daemon restart (passed internally by RESTORE-RSS-MONITORS).
   When PERSIST is T (default), saves the monitor config to storage."
  (schedule-every name interval-seconds
                  (lambda () (rss-monitor-poll name url
                                               :keywords keywords
                                               :match-mode match-mode
                                               :search-fields search-fields))
                  :replace replace)
  (bt:with-lock-held (*rss-monitors-lock*)
    (setf (gethash name *rss-monitors*)
          (list :url url :interval-seconds interval-seconds
                :keywords keywords :match-mode match-mode
                :search-fields search-fields
                :consecutive-failures consecutive-failures
                :muted-until muted-until
                :user-muted user-muted)))
  (when persist
    (persist-rss-monitors))
  (log:info "RSS monitor started: ~A → ~A (every ~Ds~@[, keywords: ~{~A~^,~}~])"
            name url interval-seconds keywords)
  name)

(defun rss-monitor-stop (name)
  "Stop monitoring a feed by cancelling its scheduled task.
   Removes the monitor from persistent storage."
  (let ((found (cancel-task name)))
    (bt:with-lock-held (*rss-monitors-lock*)
      (remhash name *rss-monitors*))
    (persist-rss-monitors)
    found))

(defun rss-list-monitors ()
  "List all RSS monitor tasks (tasks whose names start with 'rss:')."
  (remove-if-not (lambda (task)
                   (let ((name (getf task :name)))
                     (and (stringp name)
                          (>= (length name) 4)
                          (string-equal "rss:" name :end2 4))))
                 (list-tasks)))

(defun rss-monitor-configs ()
  "Return a list of all monitor config plists with :name added, sorted by name.
Includes backoff state (:consecutive-failures, :muted-until, :user-muted)."
  (sort
   (bt:with-lock-held (*rss-monitors-lock*)
     (let (acc)
       (maphash (lambda (name config)
                  (push (list* :name name config) acc))
                *rss-monitors*)
       acc))
   #'string< :key (lambda (e) (getf e :name))))

(defun rss-monitor-mute (name)
  "Silence monitor NAME indefinitely — polls are skipped until explicitly unmuted.
The scheduler task remains registered; use RSS-MONITOR-UNMUTE to resume.
Returns a status string."
  (bt:with-lock-held (*rss-monitors-lock*)
    (let ((config (gethash name *rss-monitors*)))
      (unless config
        (error "RSS monitor not found: ~A" name))
      (setf (getf config :user-muted) t)
      (setf (gethash name *rss-monitors*) config)))
  (persist-rss-monitors)
  (log:info "RSS monitor muted: ~A" name)
  (format nil "Monitor ~A muted." name))

(defun rss-monitor-unmute (name)
  "Resume a muted or backed-off monitor.
Clears user-mute, active backoff, and the consecutive failure counter so
the next scheduled poll fires normally.
Returns a status string."
  (bt:with-lock-held (*rss-monitors-lock*)
    (let ((config (gethash name *rss-monitors*)))
      (unless config
        (error "RSS monitor not found: ~A" name))
      (setf (getf config :user-muted) nil
            (getf config :muted-until) nil
            (getf config :consecutive-failures) 0
            (getf config :last-failure) nil)
      (setf (gethash name *rss-monitors*) config)))
  (persist-rss-monitors)
  (log:info "RSS monitor unmuted: ~A" name)
  (format nil "Monitor ~A unmuted; failure count reset." name))

;;; --- OPML import ---

(defun split-commas (s)
  "Split string S on commas, trimming whitespace, dropping empty parts."
  (loop for start = 0 then (1+ end)
        for end   = (position #\, s :start start)
        for part  = (string-trim " " (subseq s start (or end (length s))))
        when (plusp (length part)) collect part
        while end))

(defun opml-collect-feeds (node &optional category)
  "Recursively collect feed entries from OPML outline NODE.
Returns a list of plists for all outlines that carry an xmlUrl attribute.
Keys: :name :url :category :interval :keywords :match-mode :search-fields.
Crichton-specific attributes (pollIntervalSeconds, crichtonKeywords, etc.)
are read back transparently to support round-trip import."
  (let ((children (xml-find-children "outline" node)))
    (loop for child in children
          for xml-url = (xml-attr "xmlUrl" child)
          for text    = (or (xml-attr "text"  child)
                            (xml-attr "title" child)
                            "")
          nconc
          (if xml-url
              (let* ((interval-str  (xml-attr "pollIntervalSeconds" child))
                     (keywords-str  (xml-attr "crichtonKeywords"    child))
                     (match-mode    (xml-attr "crichtonMatchMode"    child))
                     (fields-str    (xml-attr "crichtonSearchFields" child)))
                (list (list :name         text
                            :url          xml-url
                            :category     category
                            :interval     (when interval-str
                                            (ignore-errors (parse-integer interval-str)))
                            :keywords     (when (and keywords-str
                                                     (plusp (length keywords-str)))
                                            (split-commas keywords-str))
                            :match-mode   match-mode
                            :search-fields (when (and fields-str
                                                       (plusp (length fields-str)))
                                             (split-commas fields-str)))))
              ;; No xmlUrl — treat as a category folder; recurse
              (opml-collect-feeds child (if (plusp (length text)) text category))))))

(defun opml-import-monitors (file-path &key (interval-seconds 3600)
                                             (name-prefix "rss:"))
  "Parse the OPML file at FILE-PATH and register every feed as an RSS monitor.
INTERVAL-SECONDS is the fallback polling interval when not encoded in the file.
NAME-PREFIX is prepended to each feed's text attribute when no monitor name is
stored in the file (i.e. feeds imported from a third-party OPML).
Crichton-exported OPML files carry pollIntervalSeconds, crichtonKeywords, etc.
as custom attributes; these are restored transparently for full round-tripping.
Returns a human-readable summary string."
  (unless (probe-file file-path)
    (error "OPML file not found: ~A" file-path))
  (let* ((content (uiop:read-file-string file-path))
         (root    (xmls:parse content)))
    (unless (string-equal (xml-tag-name root) "opml")
      (error "Not an OPML file (root element: ~A)" (xml-tag-name root)))
    (let* ((body   (xml-find-child "body" root))
           (feeds  (when body (opml-collect-feeds body)))
           (ok     0)
           (errs   nil))
      (dolist (feed feeds)
        (let* ((url       (getf feed :url))
               (text      (getf feed :name))
               (interval  (or (getf feed :interval) interval-seconds))
               (keywords  (getf feed :keywords))
               (match-mode   (getf feed :match-mode))
               (search-fields (getf feed :search-fields))
               (task-name (format nil "~A~A" name-prefix
                                  (if (plusp (length text)) text url))))
          (handler-case
              (progn
                (rss-monitor-start task-name url interval
                                   :keywords keywords
                                   :match-mode match-mode
                                   :search-fields search-fields
                                   :persist nil)
                (incf ok))
            (error (c)
              (push (format nil "~A: ~A" text c) errs)))))
      ;; Single persist after all monitors registered, not one per feed
      (when (plusp ok)
        (persist-rss-monitors))
      (with-output-to-string (s)
        (format s "OPML import: ~D of ~D feed~:P registered~%"
                ok (length feeds))
        (when errs
          (format s "~D error~:P:~%" (length errs))
          (dolist (e (nreverse errs))
            (format s "  ~A~%" e)))))))
