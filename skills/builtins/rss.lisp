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

(defstruct (feed-item (:constructor %make-feed-item))
  (title "" :type string)
  (link "" :type string)
  (guid "" :type string)
  (description "" :type string)
  (pub-date "" :type string))

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
   Returns (values items feed-title feed-format)."
  (log:info "Fetching RSS feed: ~A" url)
  (handler-case
      (multiple-value-bind (body status)
          (dex:get url :headers '(("Accept" . "application/rss+xml, application/atom+xml, application/xml, text/xml")
                                  ("User-Agent" . "Crichton/0.1")))
        (unless (= status 200)
          (error "Feed fetch returned HTTP ~D for ~A" status url))
        (parse-feed-xml body))
    (error (c)
      (error "Feed fetch failed for ~A: ~A" url c))))

;;; --- Seen-item state for monitoring ---

(defvar *rss-state-lock* (bt:make-lock "rss-state"))
(defvar *rss-seen* (make-hash-table :test #'equal)
  "Hash of URL → hash-table of seen GUIDs.")

(defun seen-guids (url)
  "Get or create the set of seen GUIDs for URL."
  (bt:with-lock-held (*rss-state-lock*)
    (or (gethash url *rss-seen*)
        (setf (gethash url *rss-seen*)
              (make-hash-table :test #'equal)))))

(defun mark-seen (url items)
  "Mark all ITEMS as seen for URL."
  (let ((seen (seen-guids url)))
    (bt:with-lock-held (*rss-state-lock*)
      (dolist (item items)
        (setf (gethash (feed-item-guid item) seen) t)))))

(defun filter-new-items (url items)
  "Return only items not yet seen for URL."
  (let ((seen (seen-guids url)))
    (bt:with-lock-held (*rss-state-lock*)
      (remove-if (lambda (item)
                   (gethash (feed-item-guid item) seen))
                 items))))

(defun clear-seen (url)
  "Clear the seen state for URL."
  (bt:with-lock-held (*rss-state-lock*)
    (remhash url *rss-seen*)))

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

(defun rss-fetch (url &key (max-items 50))
  "Fetch a feed and return items as a list of plists.
   Suitable for programmatic use by other daemon components."
  (multiple-value-bind (items title format) (fetch-feed url)
    (let ((items (subseq items 0 (min max-items (length items)))))
      (list :url url
            :title title
            :format format
            :count (length items)
            :items (mapcar (lambda (item)
                             (list :title (feed-item-title item)
                                   :link (feed-item-link item)
                                   :guid (feed-item-guid item)
                                   :pub-date (feed-item-pub-date item)
                                   :description (feed-item-description item)))
                           items)))))

(defun rss-check (url &key (max-items 50))
  "Check a feed for new items (not previously seen).
   On first check, all items are 'new'. Returns a plist."
  (multiple-value-bind (items title) (fetch-feed url)
    (let* ((limited (subseq items 0 (min max-items (length items))))
           (new-items (filter-new-items url limited)))
      (mark-seen url limited)
      (list :url url
            :title title
            :new-count (length new-items)
            :total-count (length limited)
            :new-items (mapcar (lambda (item)
                                 (list :title (feed-item-title item)
                                       :link (feed-item-link item)
                                       :guid (feed-item-guid item)
                                       :pub-date (feed-item-pub-date item)))
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

;;; --- Monitoring integration with scheduler ---

(defun rss-monitor-start (name url interval-seconds &key (replace t))
  "Start monitoring a feed by scheduling periodic checks.
   NAME is the task name. Results logged via log4cl."
  (schedule-every name interval-seconds
                  (lambda ()
                    (handler-case
                        (let ((result (rss-check url)))
                          (when (plusp (getf result :new-count))
                            (log:info "RSS ~A: ~D new item~:P" name (getf result :new-count))
                            (dolist (item (getf result :new-items))
                              (log:info "  ~A: ~A" name (getf item :title)))))
                      (error (c)
                        (log:warn "RSS monitor ~A failed: ~A" name c))))
                  :replace replace)
  (log:info "RSS monitor started: ~A → ~A (every ~Ds)" name url interval-seconds)
  name)

(defun rss-monitor-stop (name)
  "Stop monitoring a feed by cancelling its scheduled task."
  (cancel-task name))

(defun rss-list-monitors ()
  "List all RSS monitor tasks (tasks whose names start with 'rss:')."
  (remove-if-not (lambda (task)
                   (let ((name (getf task :name)))
                     (and (stringp name)
                          (>= (length name) 4)
                          (string-equal "rss:" name :end2 4))))
                 (list-tasks)))
