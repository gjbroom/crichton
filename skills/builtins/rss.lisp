;;;; skills/builtins/rss.lisp
;;;;
;;;; Built-in skill: RSS/Atom feed item class, XML helpers, feed parsing,
;;;; HTTP fetching, seen-item state, and persistence.
;;;; Split into three files:
;;;;   rss.lisp         — this file (feed-item, XML, parsing, fetch, state)
;;;;   rss-monitor.lisp — monitor registry, polling, backoff, OPML import
;;;;   rss-publish.lisp — feed generation (write/publish), OPML export
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

(defun make-feed-item (&key (title "") (link "") (guid "") (description "") (pub-date ""))
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
  (make-feed-item
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
    (make-feed-item
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
   Uses unified retry infrastructure for transient network failures."
  (log:info "Fetching RSS feed: ~A" url)
  (let ((config (get-retry-config :rss)))
    (multiple-value-bind (body status)
        (http-get-with-retry url
                             :headers '(("Accept" . "application/rss+xml, application/atom+xml, application/xml, text/xml")
                                        ("User-Agent" . "Crichton/0.1"))
                             :context :rss
                             :max-retries (getf config :max-retries)
                             :connect-timeout 10
                             :read-timeout 30)
      (unless (= status 200)
        (error "Feed fetch returned HTTP ~D for ~A" status url))
      (with-retry (:context :rss :max-retries 2 :backoff-base 0.1)
        (handler-case
            (parse-feed-xml body)
          (error (c)
            (error "RSS XML parsing failed for ~A: ~A" url c)))))))

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
