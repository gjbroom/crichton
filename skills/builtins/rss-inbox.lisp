;;;; skills/builtins/rss-inbox.lisp
;;;;
;;;; Persistent SQLite article inbox for RSS monitoring.
;;;; Articles found by rss-monitor-poll accumulate here until the
;;;; interest scorer has reviewed and dispositioned them.
;;;;
;;;; Schema
;;;;   guid            TEXT  PK — unique article identifier
;;;;   feed_name       TEXT  — monitor name that found the article
;;;;   title           TEXT
;;;;   link            TEXT
;;;;   description     TEXT  — used by the LLM scorer, not displayed
;;;;   pub_date        TEXT
;;;;   first_seen      INT   — get-universal-time when inserted
;;;;   interest_score  REAL  NULL until scored
;;;;   scored_at       INT   NULL until scored
;;;;   tags            TEXT  comma-separated, written by scorer
;;;;   score_reason    TEXT  one sentence from scorer
;;;;   saved_to_raindrop INT 0/1
;;;;   reviewed        INT   0/1 — set when user queries for the article

(in-package #:crichton/skills)

;;; --- Database path ---

(defun inbox-db-path ()
  "Return the pathname of the RSS article inbox SQLite database."
  (merge-pathnames "rss-inbox.db" (crichton/config:agent-path "data")))

;;; --- Schema ---

(defun inbox-ensure-schema (db)
  "Create the articles table and indexes if they do not yet exist."
  (sqlite:execute-non-query db
    "CREATE TABLE IF NOT EXISTS articles (
       guid              TEXT    PRIMARY KEY,
       feed_name         TEXT    NOT NULL DEFAULT '',
       title             TEXT    NOT NULL DEFAULT '',
       link              TEXT    NOT NULL DEFAULT '',
       description       TEXT    NOT NULL DEFAULT '',
       pub_date          TEXT    NOT NULL DEFAULT '',
       first_seen        INTEGER NOT NULL,
       interest_score    REAL,
       scored_at         INTEGER,
       tags              TEXT,
       score_reason      TEXT,
       saved_to_raindrop INTEGER NOT NULL DEFAULT 0,
       reviewed          INTEGER NOT NULL DEFAULT 0
     )")
  (sqlite:execute-non-query db
    "CREATE INDEX IF NOT EXISTS idx_inbox_feed     ON articles(feed_name)")
  (sqlite:execute-non-query db
    "CREATE INDEX IF NOT EXISTS idx_inbox_unsaved  ON articles(saved_to_raindrop, interest_score)")
  (sqlite:execute-non-query db
    "CREATE INDEX IF NOT EXISTS idx_inbox_reviewed ON articles(reviewed, first_seen)"))

;;; --- DB open macro ---

(defmacro with-inbox-db ((db) &body body)
  "Open the RSS inbox database and ensure the schema exists.
   Creates ~/.crichton/data/ and the database file on first use."
  `(let* ((path    (inbox-db-path))
          (pathstr (namestring path)))
     (ensure-directories-exist path)
     (sqlite:with-open-database (,db pathstr)
       (inbox-ensure-schema ,db)
       ,@body)))

;;; --- Row conversion helpers ---

(defun inbox-summary-row->plist (row)
  "Convert a summary-query row to a plist.
   Expected columns: guid feed_name title link pub_date
                     interest_score tags score_reason"
  (destructuring-bind (guid feed-name title link pub-date score tags reason) row
    (list :guid          guid
          :feed-name     (or feed-name "")
          :title         (or title "")
          :link          (or link "")
          :pub-date      (or pub-date "")
          :interest-score score
          :tags          (when (and tags (plusp (length tags)))
                           (split-commas tags))
          :reason        reason)))

(defun inbox-score-row->plist (row)
  "Convert a scoring-query row (includes description) to a plist.
   Expected columns: guid feed_name title description link pub_date"
  (destructuring-bind (guid feed-name title description link pub-date) row
    (list :guid        guid
          :feed-name   (or feed-name "")
          :title       (or title "")
          :description (or description "")
          :link        (or link "")
          :pub-date    (or pub-date ""))))

;;; --- Write operations ---

(defun inbox-insert-articles (items)
  "Insert ITEMS into the inbox.  Each item is a plist with keys
   :id :title :description :link :published :feed-name (canonical RSS schema).
   Duplicate GUIDs are silently ignored (INSERT OR IGNORE)."
  (when items
    (with-inbox-db (db)
      (sqlite:with-transaction db
        (dolist (item items)
          (sqlite:execute-non-query db
            "INSERT OR IGNORE INTO articles
             (guid, feed_name, title, link, description, pub_date, first_seen)
             VALUES (?, ?, ?, ?, ?, ?, ?)"
            (or (getf item :id) "")
            (or (getf item :feed-name) "")
            (or (getf item :title) "")
            (or (getf item :link) "")
            (or (getf item :description) "")
            (or (getf item :published) "")
            (get-universal-time)))))))

(defun inbox-save-scores (score-results)
  "Write LLM scoring results back to the inbox.
   SCORE-RESULTS is a list of plists:
     :guid string  :score float  :tags list-of-strings  :reason string"
  (let ((now (get-universal-time)))
    (with-inbox-db (db)
      (sqlite:with-transaction db
        (dolist (result score-results)
          (sqlite:execute-non-query db
            "UPDATE articles
             SET interest_score = ?, scored_at = ?, tags = ?, score_reason = ?
             WHERE guid = ?"
            (getf result :score 0.0)
            now
            (format nil "~{~A~^,~}" (or (getf result :tags) '()))
            (or (getf result :reason) "")
            (getf result :guid)))))))

(defun inbox-mark-saved (guid)
  "Record that the article with GUID has been saved to Raindrop."
  (with-inbox-db (db)
    (sqlite:execute-non-query db
      "UPDATE articles SET saved_to_raindrop = 1 WHERE guid = ?"
      guid)))

(defun inbox-mark-reviewed (guids)
  "Mark a list of article GUIDs as reviewed by the user."
  (when guids
    (with-inbox-db (db)
      (sqlite:with-transaction db
        (dolist (guid guids)
          (sqlite:execute-non-query db
            "UPDATE articles SET reviewed = 1 WHERE guid = ?"
            guid))))))

;;; --- Read operations ---

(defun inbox-get-unscored (&optional (limit 20))
  "Return up to LIMIT unscored articles (oldest first) for LLM scoring.
   Returns plists with :guid :feed-name :title :description :link :pub-date"
  (with-inbox-db (db)
    (mapcar #'inbox-score-row->plist
            (sqlite:execute-to-list db
              "SELECT guid, feed_name, title, description, link, pub_date
               FROM articles
               WHERE interest_score IS NULL
               ORDER BY first_seen ASC
               LIMIT ?"
              limit))))

(defun inbox-get-unsaved (&optional (threshold 0.7))
  "Return articles scored at or above THRESHOLD that have not been saved to Raindrop yet.
   Returns summary plists suitable for display and Raindrop save."
  (with-inbox-db (db)
    (mapcar #'inbox-summary-row->plist
            (sqlite:execute-to-list db
              "SELECT guid, feed_name, title, link, pub_date,
                      interest_score, tags, score_reason
               FROM articles
               WHERE interest_score >= ? AND saved_to_raindrop = 0
               ORDER BY interest_score DESC"
              threshold))))

(defun inbox-by-feed (feed-name)
  "Return unreviewed articles for FEED-NAME, or all feeds when FEED-NAME is NIL.
   Sorted by interest-score descending (unscored items last)."
  (with-inbox-db (db)
    (mapcar #'inbox-summary-row->plist
            (if feed-name
                (sqlite:execute-to-list db
                  "SELECT guid, feed_name, title, link, pub_date,
                          interest_score, tags, score_reason
                   FROM articles
                   WHERE feed_name = ? AND reviewed = 0
                   ORDER BY COALESCE(interest_score, -1) DESC, first_seen DESC
                   LIMIT 100"
                  feed-name)
                (sqlite:execute-to-list db
                  "SELECT guid, feed_name, title, link, pub_date,
                          interest_score, tags, score_reason
                   FROM articles
                   WHERE reviewed = 0
                   ORDER BY COALESCE(interest_score, -1) DESC, first_seen DESC
                   LIMIT 100")))))

(defun inbox-saved-since (timestamp)
  "Return articles saved to Raindrop at or after TIMESTAMP (universal-time).
   Used by the morning briefing to report what was curated overnight."
  (with-inbox-db (db)
    (mapcar #'inbox-summary-row->plist
            (sqlite:execute-to-list db
              "SELECT guid, feed_name, title, link, pub_date,
                      interest_score, tags, score_reason
               FROM articles
               WHERE saved_to_raindrop = 1 AND scored_at >= ?
               ORDER BY interest_score DESC"
              timestamp))))

(defun inbox-stats ()
  "Return statistics about the inbox as a plist."
  (with-inbox-db (db)
    (list :total      (sqlite:execute-single db "SELECT COUNT(*) FROM articles")
          :unscored   (sqlite:execute-single db
                        "SELECT COUNT(*) FROM articles WHERE interest_score IS NULL")
          :high-score (sqlite:execute-single db
                        "SELECT COUNT(*) FROM articles WHERE interest_score >= 0.7")
          :saved      (sqlite:execute-single db
                        "SELECT COUNT(*) FROM articles WHERE saved_to_raindrop = 1")
          :unreviewed (sqlite:execute-single db
                        "SELECT COUNT(*) FROM articles WHERE reviewed = 0"))))
