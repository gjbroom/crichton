;;;; skills/builtins/rss-publish.lisp
;;;;
;;;; RSS feed generation: publish items to named feeds, serve as RSS 2.0 XML,
;;;; and export monitor registries to OPML.
;;;; Depends on rss.lisp for xml-find-children, xml-attr, and xml-tag-name.
;;;; Depends on rss-monitor.lisp for *rss-monitors* and *rss-monitors-lock*.

(in-package #:crichton/skills)

;;; --- Feed writing/generation ---

(defconstant +rss-pub-namespace+
  (if (boundp '+rss-pub-namespace+) +rss-pub-namespace+ "rss-pub")
  "Storage namespace for published (generated) RSS feeds.")

(defconstant +rss-default-max-items+ 100
  "Default maximum number of items retained per published feed.")

(defun xml-escape (text)
  "Escape special XML characters in TEXT for use in element content."
  (with-output-to-string (s)
    (loop for ch across (or text "") do
      (case ch
        (#\& (write-string "&amp;" s))
        (#\< (write-string "&lt;" s))
        (#\> (write-string "&gt;" s))
        (otherwise (write-char ch s))))))

(defun format-rfc822-date (universal-time)
  "Format UNIVERSAL-TIME (seconds since CL epoch) as an RFC 822 date string (UTC)."
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time universal-time 0)
    (let ((days   #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
          (months #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
      (format nil "~A, ~2,'0D ~A ~4D ~2,'0D:~2,'0D:~2,'0D +0000"
              (aref days dow) day (aref months month) year hour min sec))))

(defun feed-config-key (name)
  (format nil "feed-config:~A" name))

(defun feed-items-key (name)
  (format nil "feed-items:~A" name))

(defun feed-config (name)
  "Return the configuration hash-table for feed NAME, or a default config."
  (or (crichton/storage:store-get +rss-pub-namespace+ (feed-config-key name))
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
  (let* ((existing (feed-config name))
         (config (make-hash-table :test #'equal)))
    (setf (gethash "title" config)       (or title       (gethash "title" existing)       name)
          (gethash "description" config) (or description (gethash "description" existing) "")
          (gethash "link" config)        (or link        (gethash "link" existing)        "")
          (gethash "max_items" config)   (or max-items   (gethash "max_items" existing)   +rss-default-max-items+))
    (crichton/storage:store-set +rss-pub-namespace+ (feed-config-key name) config)
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
                           (format-rfc822-date now)))
         (item (make-hash-table :test #'equal))
         (config (feed-config name))
         (max-items (or (gethash "max_items" config) +rss-default-max-items+))
         (existing (or (crichton/storage:store-get +rss-pub-namespace+ (feed-items-key name))
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
      (crichton/storage:store-set +rss-pub-namespace+ (feed-items-key name)
                                  (coerce capped 'vector)))
    (log:info "RSS feed ~A: published item ~S" name title)
    effective-guid))

(defun rss-feed-items (name)
  "Return the items of feed NAME as a list of plists, newest first.
   Each plist has :title :description :link :guid :pub-date."
  (let ((raw (crichton/storage:store-get +rss-pub-namespace+ (feed-items-key name))))
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
  (let* ((config (feed-config name))
         (title       (or (gethash "title"       config) name))
         (description (or (gethash "description" config) ""))
         (link        (or (gethash "link"        config) ""))
         (items       (rss-feed-items name)))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
      (format s "<rss version=\"2.0\">~%")
      (format s "  <channel>~%")
      (format s "    <title>~A</title>~%" (xml-escape title))
      (format s "    <link>~A</link>~%" (xml-escape link))
      (format s "    <description>~A</description>~%" (xml-escape description))
      (format s "    <generator>Crichton</generator>~%")
      (dolist (item items)
        (format s "    <item>~%")
        (format s "      <title>~A</title>~%"
                (xml-escape (getf item :title)))
        (when (plusp (length (getf item :link)))
          (format s "      <link>~A</link>~%"
                  (xml-escape (getf item :link))))
        (when (plusp (length (getf item :description)))
          (format s "      <description>~A</description>~%"
                  (xml-escape (getf item :description))))
        (format s "      <guid isPermaLink=\"false\">~A</guid>~%"
                (xml-escape (getf item :guid)))
        (when (plusp (length (getf item :pub-date)))
          (format s "      <pubDate>~A</pubDate>~%" (getf item :pub-date)))
        (format s "    </item>~%"))
      (format s "  </channel>~%")
      (format s "</rss>~%"))))

(defun rss-feed-clear (name)
  "Remove all items from feed NAME. Configuration is preserved.
   Returns T."
  (crichton/storage:store-delete +rss-pub-namespace+ (feed-items-key name))
  (log:info "RSS feed ~A: cleared all items" name)
  t)

(defun rss-feed-delete (name)
  "Delete feed NAME entirely, removing both configuration and items.
   Returns T."
  (crichton/storage:store-delete +rss-pub-namespace+ (feed-config-key name))
  (crichton/storage:store-delete +rss-pub-namespace+ (feed-items-key name))
  (log:info "RSS feed ~A: deleted" name)
  t)

(defun rss-feed-list ()
  "Return a list of all configured feed names (sorted alphabetically)."
  (let* ((keys   (crichton/storage:store-list-keys +rss-pub-namespace+ "feed-config:"))
         (prefix "feed-config:"))
    (sort (mapcar (lambda (k) (subseq k (length prefix))) keys)
          #'string<)))

;;; --- OPML export ---

(defun opml-export-monitors (&key file-path (title "RSS Feeds"))
  "Generate an OPML 2.0 document from all registered RSS monitors.
If FILE-PATH is given, write to that file and return a summary string.
Otherwise return the OPML XML as a string."
  (flet ((attr-escape (text)
           ;; xml-escape handles &/</>; also escape \" for attribute values
           (with-output-to-string (s)
             (loop for ch across (or text "") do
               (case ch
                 (#\& (write-string "&amp;" s))
                 (#\< (write-string "&lt;" s))
                 (#\> (write-string "&gt;" s))
                 (#\" (write-string "&quot;" s))
                 (otherwise (write-char ch s)))))))
    (let* ((entries
             (sort (bt:with-lock-held (*rss-monitors-lock*)
                     (let (acc)
                       (maphash (lambda (name config) (push (cons name config) acc))
                                *rss-monitors*)
                       acc))
                   #'string< :key #'car))
           (xml
             (with-output-to-string (s)
               (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
               (format s "<opml version=\"2.0\">~%")
               (format s "  <head>~%")
               (format s "    <title>~A</title>~%" (xml-escape title))
               (format s "    <dateCreated>~A</dateCreated>~%"
                       (format-rfc822-date (get-universal-time)))
               (format s "  </head>~%")
               (format s "  <body>~%")
               (dolist (entry entries)
                 (let* ((name     (car entry))
                        (config   (cdr entry))
                        (url      (getf config :url))
                        (interval (getf config :interval-seconds))
                        (keywords (getf config :keywords))
                        (match-mode    (getf config :match-mode))
                        (search-fields (getf config :search-fields))
                        (text    (if (and (>= (length name) 4)
                                          (string-equal "rss:" name :end2 4))
                                     (subseq name 4)
                                     name)))
                   (format s "    <outline type=\"rss\" text=\"~A\" xmlUrl=\"~A\"~
                                  ~@[ pollIntervalSeconds=\"~D\"~]~
                                  ~@[ crichtonKeywords=\"~A\"~]~
                                  ~@[ crichtonMatchMode=\"~A\"~]~
                                  ~@[ crichtonSearchFields=\"~A\"~]~
                                  />~%"
                           (attr-escape text)
                           (attr-escape url)
                           interval
                           (when keywords
                             (attr-escape (format nil "~{~A~^,~}" keywords)))
                           (when match-mode (attr-escape match-mode))
                           (when search-fields
                             (attr-escape (format nil "~{~A~^,~}" search-fields))))))
               (format s "  </body>~%")
               (format s "</opml>~%"))))
      (if file-path
          (progn
            (with-open-file (f file-path :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (write-string xml f))
            (format nil "OPML written to ~A (~D monitor~:P)"
                    file-path (length entries)))
          xml))))
