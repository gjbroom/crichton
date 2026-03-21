;;;; skills/builtins/rss.lisp
;;;;
;;;; Built-in skill: RSS/Atom feed fetching and monitoring.
;;;; Fetches feeds via dexador, parses via xmls.
;;;; Maintains seen-item state for monitoring (new item detection).
;;;; Integrates with scheduler for periodic polling.
;;;;
;;;; Handles RSS 2.0 (items inside channel), RSS 1.0/RDF (items outside
;;;; channel), and Atom (entry elements).
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Feed item representation ---

(defclass feed-item ()
  ((title :initarg :title
          :initform ""
          :type string
          :accessor feed-item-title)
   (link :initarg :link
         :initform ""
         :type string
         :accessor feed-item-link)
   (guid :initarg :guid
         :initform ""
         :type string
         :accessor feed-item-guid)
   (description :initarg :description
                :initform ""
                :type string
                :accessor feed-item-description)
   (pub-date :initarg :pub-date
             :initform ""
             :type string
             :accessor feed-item-pub-date)))

(defun %make-feed-item (&key (title "") (link "") (guid "") (description "") (pub-date ""))
  (make-instance 'feed-item
                 :title title
                 :link link
                 :guid guid
                 :description description
                 :pub-date pub-date))

;;; --- XML helpers (xmls returns node structs, not lists) ---

(defun xml-node-p (thing)
  "Return T if THING is an xmls node struct."
  (typep thing 'xmls:node))

(defun xml-tag-name (node)
  "Get the local tag name from an xmls node, stripping any namespace."
  (when (xml-node-p node)
    (let ((name (xmls:node-name node)))
      (cond
        ((stringp name)
         (let ((colon (position #\: name)))
           (if colon (subseq name (1+ colon)) name)))
        ((and (listp name) (second name))
         (let ((local (second name)))
           (when (stringp local)
             (let ((colon (position #\: local)))
               (if colon (subseq local (1+ colon)) local)))))
        (t nil)))))

(defun xml-find-child (tag-name node)
  "Find the first child element with TAG-NAME (case-insensitive) in NODE."
  (when (xml-node-p node)
    (find-if (lambda (child)
               (and (xml-node-p child)
                    (let ((n (xml-tag-name child)))
                      (and n (string-equal tag-name n)))))
             (xmls:node-children node))))

(defun xml-child-text (tag-name node)
  "Get the text content of the first child element named TAG-NAME."
  (let ((child (xml-find-child tag-name node)))
    (when child
      (let ((text-parts
              (remove-if-not #'stringp (xmls:node-children child))))
        (format nil "~{~A~}" text-parts)))))

(defun xml-find-children (tag-name node)
  "Find all child elements with TAG-NAME (case-insensitive) in NODE."
  (when (xml-node-p node)
    (remove-if-not (lambda (child)
                     (and (xml-node-p child)
                          (let ((n (xml-tag-name child)))
                            (and n (string-equal tag-name n)))))
                   (xmls:node-children node))))

(defun xml-attr (attr-name node)
  "Get an attribute value from an xmls node."
  (when (xml-node-p node)
    (let ((attrs (xmls:node-attrs node)))
      (when attrs
        (let ((pair (assoc attr-name attrs :test #'string-equal)))
          (when pair (cdr pair)))))))

;;; --- Feed parsing ---

(defun parse-rss-item (item-node)
  "Parse an RSS <item> element into a feed-item struct."
  (%make-feed-item
   :title (or (xml-child-text "title" item-node) "")
   :link (or (xml-child-text "link" item-node) "")
   :guid (or (xml-child-text "guid" item-node)
             (xml-child-text "link" item-node) "")
   :description (or (xml-child-text "description" item-node) "")
   :pub-date (or (xml-child-text "pubDate" item-node)
                 (xml-child-text "date" item-node) "")))

(defun parse-atom-entry (entry-node)
  "Parse an Atom <entry> element into a feed-item struct."
  (let ((link-node (xml-find-child "link" entry-node)))
    (%make-feed-item
     :title (or (xml-child-text "title" entry-node) "")
     :link (or (when link-node (xml-attr "href" link-node))
               (xml-child-text "link" entry-node) "")
     :guid (or (xml-child-text "id" entry-node) "")
     :description (or (xml-child-text "summary" entry-node)
                      (xml-child-text "content" entry-node) "")
     :pub-date (or (xml-child-text "updated" entry-node)
                   (xml-child-text "published" entry-node) ""))))

(defun detect-feed-format (root)
  "Detect whether ROOT is RSS or Atom. Returns :RSS, :ATOM, or NIL."
  (let ((name (xml-tag-name root)))
    (cond
      ((string-equal name "rss") :rss)
      ((string-equal name "feed") :atom)
      ((string-equal name "RDF") :rdf)
      (t nil))))

(defun parse-feed-xml (xml-string)
  "Parse an RSS or Atom feed from XML-STRING.
   Returns (values items feed-title feed-format).
   Handles RSS 2.0 (items in channel), RSS 1.0/RDF (items at root level),
   and Atom (entry elements)."
  (let* ((root (xmls:parse xml-string))
         (format (detect-feed-format root)))
    (unless format
      (error "Unrecognized feed format (root element: ~A)" (xml-tag-name root)))
    (case format
      (:rss
       (let* ((channel (xml-find-child "channel" root))
              (items (xml-find-children "item" (or channel root)))
              (title (or (when channel (xml-child-text "title" channel)) "")))
         (values (mapcar #'parse-rss-item items) title :rss)))
      (:rdf
       (let* ((channel (xml-find-child "channel" root))
              (items (xml-find-children "item" root))
              (title (or (when channel (xml-child-text "title" channel)) "")))
         (values (mapcar #'parse-rss-item items) title :rdf)))
      (:atom
       (let* ((entries (xml-find-children "entry" root))
              (title (or (xml-child-text "title" root) "")))
         (values (mapcar #'parse-atom-entry entries) title :atom))))))

;;; --- HTTP fetching ---

(defun fetch-feed (url)
  "Fetch and parse an RSS/Atom feed from URL.
   Returns (values items feed-title feed-format).
   Offers :RETRY and :USE-VALUE restarts on failure."
  (log:info "Fetching RSS feed: ~A" url)
  (restart-case
      (multiple-value-bind (body status)
          (dex:get url :headers '(("Accept" . "application/rss+xml, application/atom+xml, application/xml, text/xml")
                                  ("User-Agent" . "Crichton/0.1")))
        (unless (= status 200)
          (error "Feed fetch returned HTTP ~D for ~A" status url))
        (parse-feed-xml body))
    (:retry ()
      :report (lambda (s) (format s "Retry fetching ~A" url))
      (fetch-feed url))
    (:use-value (items &optional (title "") (fmt nil))
      :report (lambda (s) (format s "Supply feed data for ~A" url))
      :interactive (lambda ()
                     (format *query-io* "~&Items (list): ")
                     (list (eval (read *query-io*))))
      (values items title fmt))))

;;; --- Seen-item state for monitoring ---

(defvar *rss-state-lock* (bt:make-lock "rss-state"))
(defvar *rss-seen* (make-hash-table :test #'equal)
  "Hash of URL → hash-table of seen GUIDs → timestamp.")
(defvar *rss-state-loaded* nil
  "Flag indicating whether persisted state has been loaded.")
(defvar *auto-persist-rss* t
  "If true, automatically persist seen items after each mark-seen operation.")

(defun ensure-rss-state-loaded ()
  "Load persisted RSS state from storage on first access.
   Thread-safe. Caller must hold *rss-state-lock*."
  (unless *rss-state-loaded*
    (let ((seen-data (crichton/storage:store-get "rss" "seen-feeds")))
      (when (and seen-data (hash-table-p seen-data))
        (setf *rss-seen* seen-data)
        (log:info "Loaded RSS seen-items for ~D feed~:P from storage"
                  (hash-table-count seen-data))))
    (setf *rss-state-loaded* t)))

(defun seen-guids (url)
  "Get or create the set of seen GUIDs for URL."
  (bt:with-lock-held (*rss-state-lock*)
    (ensure-rss-state-loaded)
    (or (gethash url *rss-seen*)
        (setf (gethash url *rss-seen*)
              (make-hash-table :test #'equal)))))

(defun mark-seen (url items)
  "Mark all ITEMS as seen for URL. Persists to storage if auto-persist is enabled."
  (let ((seen (seen-guids url))
        (now (get-universal-time)))
    (bt:with-lock-held (*rss-state-lock*)
      (dolist (item items)
        (setf (gethash (feed-item-guid item) seen) now))))
  ;; Auto-persist if enabled
  (when *auto-persist-rss*
    (persist-rss-state)))

(defun filter-new-items (url items)
  "Return only items not yet seen for URL."
  (let ((seen (seen-guids url)))
    (bt:with-lock-held (*rss-state-lock*)
      (remove-if (lambda (item)
                   (gethash (feed-item-guid item) seen))
                 items))))

(defun clear-seen (url)
  "Clear the seen state for URL. Persists the change."
  (bt:with-lock-held (*rss-state-lock*)
    (ensure-rss-state-loaded)
    (remhash url *rss-seen*))
  (when *auto-persist-rss*
    (persist-rss-state)))

;;; --- Formatted output ---

(defun format-feed-item (item &optional (stream *standard-output*))
  "Format a single feed item for display."
  (format stream "  ~A~%" (feed-item-title item))
  (when (plusp (length (feed-item-link item)))
    (format stream "    ~A~%" (feed-item-link item)))
  (when (plusp (length (feed-item-pub-date item)))
    (format stream "    ~A~%" (feed-item-pub-date item))))

(defun format-feed-items (items &key (title "") (stream *standard-output*) (max 20))
  "Format a list of feed items for display."
  (when (plusp (length title))
    (format stream "~&~A~%" title))
  (format stream "~D item~:P~%" (length items))
  (dolist (item (subseq items 0 (min max (length items))))
    (format-feed-item item stream))
  (when (> (length items) max)
    (format stream "  ... and ~D more~%" (- (length items) max))))

;;; --- Public interface (programmatic) ---

(defun feed-item-to-plist (item &optional feed-name)
  "Convert a feed-item to the canonical plist representation.
   Field names match the rss-filter WASM skill's RssItem JSON schema:
     :id, :title, :description, :link, :published, :feed-name"
  (list :id (feed-item-guid item)
        :title (feed-item-title item)
        :description (feed-item-description item)
        :link (feed-item-link item)
        :published (feed-item-pub-date item)
        :feed-name (or feed-name "")))

(defun rss-fetch (url &key (max-items 50))
  "Fetch a feed and return items as a list of plists.
   Suitable for programmatic use by other daemon components.
   Item plists use the canonical RSS item schema: :id, :title,
   :description, :link, :published, :feed-name."
  (multiple-value-bind (items title format) (fetch-feed url)
    (let ((items (subseq items 0 (min max-items (length items)))))
      (list :url url
            :title title
            :format format
            :count (length items)
            :items (mapcar (lambda (item)
                             (feed-item-to-plist item title))
                           items)))))

(defun rss-check (url &key (max-items 50))
  "Check a feed for new items (not previously seen).
   On first check, all items are 'new'. Returns a plist.
   Item plists use the canonical RSS item schema."
  (multiple-value-bind (items title) (fetch-feed url)
    (let* ((limited (subseq items 0 (min max-items (length items))))
           (new-items (filter-new-items url limited)))
      (mark-seen url limited)
      (list :url url
            :title title
            :new-count (length new-items)
            :total-count (length limited)
            :new-items (mapcar (lambda (item)
                                 (feed-item-to-plist item title))
                               new-items)))))

;;; --- Public interface (formatted) ---

(defun rss-report (url &key (max-items 20) (stream *standard-output*))
  "Fetch and display a feed. Returns the raw item list."
  (multiple-value-bind (items title) (fetch-feed url)
    (format-feed-items items :title title :stream stream :max max-items)
    items))

(defun rss-check-report (url &key (max-items 20) (stream *standard-output*))
  "Check for new items and display them. Returns the check result plist."
  (let ((result (rss-check url :max-items max-items)))
    (format stream "~&Feed: ~A~%" (getf result :title))
    (format stream "~D new item~:P (of ~D total)~%"
            (getf result :new-count) (getf result :total-count))
    (dolist (item (getf result :new-items))
      (format stream "  ~A~%" (getf item :title))
      (when (getf item :link)
        (format stream "    ~A~%" (getf item :link))))
    result))

;;; --- Persistence ---

(defun persist-rss-state ()
  "Save RSS seen-items state to storage. Thread-safe."
  (bt:with-lock-held (*rss-state-lock*)
    (crichton/storage:store-set "rss" "seen-feeds" *rss-seen*)
    (log:debug "Persisted RSS seen-items for ~D feed~:P"
               (hash-table-count *rss-seen*))
    t))

(defun load-rss-state ()
  "Explicitly load RSS state from storage. Thread-safe.
   Returns number of feeds loaded."
  (bt:with-lock-held (*rss-state-lock*)
    (let ((seen-data (crichton/storage:store-get "rss" "seen-feeds")))
      (if (null seen-data)
          (progn
            (log:info "No persisted RSS state found in storage")
            0)
          (progn
            (setf *rss-seen* seen-data
                  *rss-state-loaded* t)
            (log:info "Loaded RSS seen-items for ~D feed~:P from storage"
                      (hash-table-count seen-data))
            (hash-table-count seen-data))))))

(defun clear-all-rss-state ()
  "Clear all RSS seen-items state (memory and storage). Thread-safe."
  (bt:with-lock-held (*rss-state-lock*)
    (clrhash *rss-seen*)
    (setf *rss-state-loaded* t))
  (crichton/storage:store-delete "rss" "seen-feeds")
  (log:info "Cleared all RSS seen-items state")
  t)

(defun rss-state-stats ()
  "Return statistics about RSS state. Returns plist."
  (bt:with-lock-held (*rss-state-lock*)
    (ensure-rss-state-loaded)
    (let ((total-guids 0))
      (maphash (lambda (url seen-table)
                 (declare (ignore url))
                 (incf total-guids (hash-table-count seen-table)))
               *rss-seen*)
      (list :feed-count (hash-table-count *rss-seen*)
            :total-seen-items total-guids
            :auto-persist-p *auto-persist-rss*))))

(defun enable-rss-persistence ()
  "Enable automatic RSS state persistence."
  (setf *auto-persist-rss* t))

(defun disable-rss-persistence ()
  "Disable automatic RSS state persistence."
  (setf *auto-persist-rss* nil))

;;; --- Monitor registry (survives restarts) ---

(defvar *rss-monitors-lock* (bt:make-lock "rss-monitors"))
(defvar *rss-monitors* (make-hash-table :test #'equal)
  "Registry of active RSS monitors: name → plist (:url :interval-seconds :keywords :match-mode :search-fields).")

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
                   (setf (gethash name ht) entry)))
               *rss-monitors*)
      (crichton/storage:store-set "rss" "monitors" ht)))
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
                             (search-fields (gethash "search_fields" entry)))
                         (rss-monitor-start name url interval
                                            :keywords (when keywords (coerce keywords 'list))
                                            :match-mode match-mode
                                            :search-fields (when search-fields (coerce search-fields 'list))
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
   When KEYWORDS is non-nil, runs new items through the rss-filter WASM
   skill and only reports/notifies on matching items.
   Called by the scheduler callback registered via RSS-MONITOR-START.
   Retries transient failures up to 2 times via fetch-feed's :RETRY restart."
  (let ((attempts 0)
        (max-retries 2))
    (handler-bind
        ((error (lambda (c)
                  (let ((r (find-restart :retry c)))
                    (when (and r (< attempts max-retries))
                      (incf attempts)
                      (log:warn "RSS monitor ~A: retrying (~D/~D) after: ~A"
                                name attempts max-retries c)
                      (invoke-restart r))))))
      (handler-case
          (let ((result (rss-check url)))
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
          (log:warn "RSS monitor ~A failed after ~D attempt~:P: ~A"
                    name (1+ attempts) c))))))

(defun rss-monitor-start (name url interval-seconds &key (replace t) (persist t)
                                                          keywords match-mode search-fields)
  "Start monitoring a feed by scheduling periodic checks.
   NAME is the task name. Results logged via log4cl.
   When KEYWORDS is non-nil, new items are filtered through the rss-filter
   WASM skill before reporting.  MATCH-MODE (\"any\"/\"all\") and SEARCH-FIELDS
   (list of field names) control filter behaviour.
   When PERSIST is T (default), saves the monitor config (including filter
   settings) to storage so it survives daemon restarts."
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
                :search-fields search-fields)))
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

;;; --- Feed writing/generation ---

(defconstant +rss-pub-namespace+ "rss-pub"
  "Storage namespace for published (generated) RSS feeds.")

(defconstant +rss-default-max-items+ 100
  "Default maximum number of items retained per published feed.")

(defun %xml-escape (text)
  "Escape special XML characters in TEXT for use in element content."
  (with-output-to-string (s)
    (loop for ch across (or text "") do
      (case ch
        (#\& (write-string "&amp;" s))
        (#\< (write-string "&lt;" s))
        (#\> (write-string "&gt;" s))
        (otherwise (write-char ch s))))))

(defun %format-rfc822-date (universal-time)
  "Format UNIVERSAL-TIME (seconds since CL epoch) as an RFC 822 date string (UTC)."
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time universal-time 0)
    (let ((days   #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
          (months #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
      (format nil "~A, ~2,'0D ~A ~4D ~2,'0D:~2,'0D:~2,'0D +0000"
              (aref days dow) day (aref months month) year hour min sec))))

(defun %feed-config-key (name)
  (format nil "feed-config:~A" name))

(defun %feed-items-key (name)
  (format nil "feed-items:~A" name))

(defun %feed-config (name)
  "Return the configuration hash-table for feed NAME, or a default config."
  (or (crichton/storage:store-get +rss-pub-namespace+ (%feed-config-key name))
      (let ((config (make-hash-table :test #'equal)))
        (setf (gethash "title" config) name
              (gethash "description" config) ""
              (gethash "link" config) ""
              (gethash "max_items" config) +rss-default-max-items+)
        config)))

(defun rss-feed-configure (name &key (title nil) (description nil) (link nil) (max-items nil))
  "Create or update configuration for a named published feed.
   NAME is the feed identifier used in publish/xml/clear/delete calls.
   TITLE, DESCRIPTION, LINK set RSS <channel> metadata.
   MAX-ITEMS caps retained items (oldest dropped when exceeded; default 100)."
  (let* ((existing (%feed-config name))
         (config (make-hash-table :test #'equal)))
    (setf (gethash "title" config)       (or title       (gethash "title" existing)       name)
          (gethash "description" config) (or description (gethash "description" existing) "")
          (gethash "link" config)        (or link        (gethash "link" existing)        "")
          (gethash "max_items" config)   (or max-items   (gethash "max_items" existing)   +rss-default-max-items+))
    (crichton/storage:store-set +rss-pub-namespace+ (%feed-config-key name) config)
    (log:info "RSS feed configured: ~A (max ~D items)"
              name (gethash "max_items" config))
    name))

(defun rss-feed-publish (name &key (title "") (description "") (link "") guid pub-date)
  "Publish a new item to the named RSS feed, persisting to storage.
   GUID defaults to a link+timestamp URN. PUB-DATE defaults to current time
   (RFC 822 format).  Oldest items are dropped when max-items is exceeded.
   Returns the GUID of the published item."
  (let* ((now (get-universal-time))
         (effective-guid (or (and guid (plusp (length guid)) guid)
                             (if (plusp (length link))
                                 (format nil "~A#~D" link now)
                                 (format nil "urn:crichton:~A:~D" name now))))
         (pub-date-str (or (and pub-date (plusp (length pub-date)) pub-date)
                           (%format-rfc822-date now)))
         (item (make-hash-table :test #'equal))
         (config (%feed-config name))
         (max-items (or (gethash "max_items" config) +rss-default-max-items+))
         (existing (or (crichton/storage:store-get +rss-pub-namespace+ (%feed-items-key name))
                       #()))
         (existing-list (coerce existing 'list)))
    (setf (gethash "title"       item) title
          (gethash "description" item) description
          (gethash "link"        item) link
          (gethash "guid"        item) effective-guid
          (gethash "pub_date"    item) pub-date-str)
    (let* ((new-list (cons item existing-list))
           (capped   (if (> (length new-list) max-items)
                         (subseq new-list 0 max-items)
                         new-list)))
      (crichton/storage:store-set +rss-pub-namespace+ (%feed-items-key name)
                                  (coerce capped 'vector)))
    (log:info "RSS feed ~A: published item ~S" name title)
    effective-guid))

(defun rss-feed-items (name)
  "Return the items of feed NAME as a list of plists, newest first.
   Each plist has :title :description :link :guid :pub-date."
  (let ((raw (crichton/storage:store-get +rss-pub-namespace+ (%feed-items-key name))))
    (when raw
      (remove nil
              (mapcar (lambda (ht)
                        (when (hash-table-p ht)
                          (list :title       (or (gethash "title"       ht) "")
                                :description (or (gethash "description" ht) "")
                                :link        (or (gethash "link"        ht) "")
                                :guid        (or (gethash "guid"        ht) "")
                                :pub-date    (or (gethash "pub_date"    ht) ""))))
                      (coerce raw 'list))))))

(defun rss-feed-xml (name)
  "Generate a valid RSS 2.0 XML document for feed NAME.
   Returns the XML as a string. Feed config (title/description/link) is read
   from storage; items are the last MAX-ITEMS published via RSS-FEED-PUBLISH."
  (let* ((config (%feed-config name))
         (title       (or (gethash "title"       config) name))
         (description (or (gethash "description" config) ""))
         (link        (or (gethash "link"        config) ""))
         (items       (rss-feed-items name)))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
      (format s "<rss version=\"2.0\">~%")
      (format s "  <channel>~%")
      (format s "    <title>~A</title>~%" (%xml-escape title))
      (format s "    <link>~A</link>~%" (%xml-escape link))
      (format s "    <description>~A</description>~%" (%xml-escape description))
      (format s "    <generator>Crichton</generator>~%")
      (dolist (item items)
        (format s "    <item>~%")
        (format s "      <title>~A</title>~%"
                (%xml-escape (getf item :title)))
        (when (plusp (length (getf item :link)))
          (format s "      <link>~A</link>~%"
                  (%xml-escape (getf item :link))))
        (when (plusp (length (getf item :description)))
          (format s "      <description>~A</description>~%"
                  (%xml-escape (getf item :description))))
        (format s "      <guid isPermaLink=\"false\">~A</guid>~%"
                (%xml-escape (getf item :guid)))
        (when (plusp (length (getf item :pub-date)))
          (format s "      <pubDate>~A</pubDate>~%" (getf item :pub-date)))
        (format s "    </item>~%"))
      (format s "  </channel>~%")
      (format s "</rss>~%"))))

(defun rss-feed-clear (name)
  "Remove all items from feed NAME. Configuration is preserved.
   Returns T."
  (crichton/storage:store-delete +rss-pub-namespace+ (%feed-items-key name))
  (log:info "RSS feed ~A: cleared all items" name)
  t)

(defun rss-feed-delete (name)
  "Delete feed NAME entirely, removing both configuration and items.
   Returns T."
  (crichton/storage:store-delete +rss-pub-namespace+ (%feed-config-key name))
  (crichton/storage:store-delete +rss-pub-namespace+ (%feed-items-key name))
  (log:info "RSS feed ~A: deleted" name)
  t)

(defun rss-feed-list ()
  "Return a list of all configured feed names (sorted alphabetically)."
  (let* ((keys   (crichton/storage:store-list-keys +rss-pub-namespace+ "feed-config:"))
         (prefix "feed-config:"))
    (sort (mapcar (lambda (k) (subseq k (length prefix))) keys)
          #'string<)))
