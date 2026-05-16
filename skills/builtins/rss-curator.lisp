;;;; skills/builtins/rss-curator.lisp
;;;;
;;;; RSS curation pipeline: interest-based scoring via LLM,
;;;; Raindrop bookmark saving, morning briefing, and on-demand queries.
;;;;
;;;; Flow:
;;;;   run-interest-scorer  (scheduled every N hours)
;;;;     → pull unscored articles from inbox
;;;;     → score via LLM against user's interests profile
;;;;     → write scores back to inbox
;;;;     → save high-scorers to Raindrop "crichton" collection
;;;;
;;;;   rss-morning-briefing (scheduled daily)
;;;;     → query inbox for articles saved to Raindrop since last briefing
;;;;     → post a single substantive notification
;;;;
;;;;   rss-inbox-query      (on-demand, via RPC)
;;;;     → return unreviewed articles for a named feed (or all feeds)
;;;;     → mark returned articles as reviewed
;;;;
;;;; Depends on rss-inbox.lisp for persistence.

(in-package #:crichton/skills)

;;; --- Interests profile ---

(defun get-interests-profile ()
  "Return the user's interests profile text, or NIL if not yet set."
  (crichton/storage:store-get "rss" "interests-profile"))

(defun set-interests-profile (text)
  "Set the user's interests profile to TEXT.  Persists immediately."
  (crichton/storage:store-set "rss" "interests-profile" text)
  (crichton/storage:flush-all-storage)
  (log:info "RSS interests profile updated (~D chars)" (length text))
  text)

;;; --- Raindrop collection management ---

(defvar *crichton-collection-id* nil
  "Cached Raindrop collection ID for the 'crichton' collection.
   NIL forces a fresh lookup on next access.")

(defun raindrop-find-or-create-crichton-collection ()
  "Return the integer ID of the 'crichton' Raindrop collection.
   Searches existing collections; creates one if absent.
   Caches the result — subsequent calls are free."
  (or *crichton-collection-id*
      (let* ((all      (raindrop-all-collections))
             (existing (find "crichton" all
                             :key  (lambda (c) (getf c :title))
                             :test #'string-equal)))
        (setf *crichton-collection-id*
              (if existing
                  (getf existing :id)
                  (progn
                    (log:info "Creating 'crichton' Raindrop collection")
                    (getf (raindrop-create-collection "crichton") :id)))))))

;;; --- LLM scoring ---

(defparameter *curator-score-threshold* 0.7
  "Minimum interest score for an article to be saved to Raindrop.")

(defparameter *curator-batch-size* 20
  "Maximum articles per LLM scoring call.")

(defparameter *curator-system-prompt*
  "You are a relevance scorer for a personal RSS reader.
Given a user's interests profile and a list of articles, score each article.

Respond ONLY with a JSON array — no explanation, no markdown fences, nothing else.
Each element must contain exactly these fields:
  \"guid\":   the article's guid string, copied unchanged from input
  \"score\":  float 0.0-1.0
  \"tags\":   array of 1-4 short topic strings
  \"reason\": one sentence explaining the score

Scoring guide:
  0.9-1.0  Must-read — directly addresses core interests
  0.7-0.8  Worth reading — clearly relevant
  0.5-0.6  Tangential — loosely related
  0.0-0.4  Not relevant — skip")

(defun extract-json-array (text)
  "Pull the JSON array substring from TEXT, tolerating LLM preamble and markdown fences."
  (let* ((start (position #\[ text))
         (end   (when start (position #\] text :from-end t))))
    (when (and start end (< start end))
      (subseq text start (1+ end)))))

(defun parse-score-response (response-text)
  "Parse an LLM scoring response into a list of result plists.
   Returns NIL (with a warning) on any parse or format failure."
  (let ((json-str (extract-json-array response-text)))
    (unless json-str
      (log:warn "RSS curator: no JSON array found in scoring response")
      (return-from parse-score-response nil))
    (handler-case
        (let ((parsed (shasht:read-json json-str)))
          (unless (vectorp parsed)
            (log:warn "RSS curator: scoring response root is not a JSON array")
            (return-from parse-score-response nil))
          (loop for item across parsed
                when (hash-table-p item)
                collect (list :guid   (gethash "guid"   item "")
                              :score  (float (gethash "score"  item 0.0))
                              :tags   (coerce (gethash "tags"   item #()) 'list)
                              :reason (gethash "reason" item ""))))
      (error (c)
        (log:warn "RSS curator: failed to parse scoring response: ~A" c)
        (log:debug "RSS curator: first 300 chars of response: ~A"
                   (subseq response-text 0 (min 300 (length response-text))))
        nil))))

(defun score-article-batch (articles interests-profile)
  "Score ARTICLES against INTERESTS-PROFILE.
   ARTICLES is a list of plists (:guid :feed-name :title :description).
   Uses the local keyword scorer when the active provider is offline;
   otherwise calls the LLM.
   Returns a list of (:guid :score :tags :reason) plists, or NIL on failure."
  (unless articles
    (return-from score-article-batch nil))
  (let ((provider (crichton/llm:ensure-llm-provider)))
    (when (typep provider 'crichton/llm:offline-provider)
      (log:info "RSS curator: using local keyword scorer (offline mode)")
      (return-from score-article-batch
        (local-score-article-batch articles interests-profile)))
    (let* ((items-json
             (shasht:write-json
              (coerce
               (mapcar (lambda (a)
                         (let ((ht (make-hash-table :test #'equal)))
                           (setf (gethash "guid"        ht) (or (getf a :guid) "")
                                 (gethash "title"       ht) (or (getf a :title) "")
                                 (gethash "description" ht) (or (getf a :description) "")
                                 (gethash "feed_name"   ht) (or (getf a :feed-name) ""))
                           ht))
                       articles)
               'vector)
              nil))
           (prompt (format nil "Interests profile:~%~A~%~%Articles:~%~A"
                           interests-profile items-json)))
      (handler-case
          (let* ((response (crichton/llm:send-message
                            provider
                            (list (list :role :user :content prompt))
                            :system *curator-system-prompt*
                            :max-tokens 2048))
                 (text (crichton/llm:response-text response)))
            (parse-score-response text))
        (error (c)
          (log:warn "RSS curator: LLM scoring failed: ~A" c)
          nil)))))

;;; --- Local (offline) keyword scorer ---

(defparameter *stop-words*
  '("a" "an" "the" "and" "or" "but" "in" "on" "at" "to" "for" "of" "with"
    "by" "from" "is" "are" "was" "were" "be" "been" "being" "have" "has"
    "had" "do" "does" "did" "will" "would" "could" "should" "may" "might"
    "that" "this" "these" "those" "it" "its" "as" "not" "no" "so" "if"
    "about" "into" "than" "then" "also" "how" "what" "when" "where" "who"
    "which" "their" "they" "them" "we" "our" "you" "your" "he" "she" "his"
    "her" "i" "my" "me" "us" "new" "can" "up" "out" "all" "more" "just")
  "Common English words to exclude from keyword matching.")

(defun tokenize-text (text)
  "Split TEXT into lowercase alpha tokens, dropping stop-words and short tokens."
  (let ((tokens '()))
    (cl-ppcre:do-matches-as-strings (tok "[a-zA-Z]{3,}" text)
      (let ((word (string-downcase tok)))
        (unless (member word *stop-words* :test #'equal)
          (push word tokens))))
    (remove-duplicates tokens :test #'equal)))

(defun local-score-article-batch (articles interests-profile)
  "Score ARTICLES against INTERESTS-PROFILE using keyword overlap.
   No LLM required. Returns the same (:guid :score :tags :reason) plist
   format as score-article-batch so the rest of the pipeline is unchanged."
  (let* ((profile-terms (tokenize-text interests-profile))
         (total (max 1 (length profile-terms))))
    (mapcar (lambda (article)
              (let* ((text (format nil "~A ~A"
                                   (or (getf article :title) "")
                                   (or (getf article :description) "")))
                     (article-terms (tokenize-text text))
                     (matched (remove-if-not
                               (lambda (t1) (member t1 article-terms :test #'equal))
                               profile-terms))
                     (score (min 1.0 (/ (float (length matched)) total)))
                     (tags (mapcar #'string-capitalize
                                   (subseq matched 0 (min 4 (length matched)))))
                     (reason (if matched
                                 (format nil "~D term~:P matched: ~{~A~^, ~}"
                                         (length matched)
                                         (subseq matched 0 (min 5 (length matched))))
                                 "No profile terms matched")))
                (list :guid   (getf article :guid)
                      :score  score
                      :tags   tags
                      :reason reason)))
            articles)))

;;; --- Scoring loop ---

(defun save-article-to-raindrop (article collection-id)
  "Save a scored article to the Raindrop 'crichton' collection.
   Marks the inbox row saved on success; logs a warning on failure."
  (handler-case
      (progn
        (raindrop-create (or (getf article :link) "")
                         :title         (getf article :title)
                         :excerpt       (getf article :reason)
                         :tags          (getf article :tags)
                         :collection-id collection-id)
        (inbox-mark-saved (getf article :guid))
        (log:info "RSS curator: saved → ~A" (getf article :title)))
    (error (c)
      (log:warn "RSS curator: Raindrop save failed for ~S: ~A"
                (getf article :guid) c))))

(defun run-interest-scorer ()
  "Score a batch of unscored inbox articles and save high-scorers to Raindrop.
   Silently no-ops when no interests profile is set or no unscored articles exist."
  (let ((profile (get-interests-profile)))
    (unless (and profile (plusp (length profile)))
      (log:debug "RSS curator: no interests profile set — skipping")
      (return-from run-interest-scorer nil))
    (let ((batch (inbox-get-unscored *curator-batch-size*)))
      (unless batch
        (log:debug "RSS curator: no unscored articles")
        (return-from run-interest-scorer nil))
      (log:info "RSS curator: scoring ~D article~:P" (length batch))
      (let ((scores (score-article-batch batch profile)))
        (when scores
          (inbox-save-scores scores)
          (log:info "RSS curator: scored ~D article~:P" (length scores))
          (let ((to-save (inbox-get-unsaved *curator-score-threshold*)))
            (when to-save
              (handler-case
                  (let ((col-id (raindrop-find-or-create-crichton-collection)))
                    (dolist (a to-save)
                      (save-article-to-raindrop a col-id))
                    (log:info "RSS curator: ~D article~:P queued to Raindrop"
                              (length to-save)))
                (error (c)
                  (log:warn "RSS curator: Raindrop collection lookup failed: ~A" c))))))))))

;;; --- Morning briefing ---

(defun rss-morning-briefing ()
  "Post a daily briefing notification of articles curated since last time.
   Stays silent when nothing new was curated.
   Persists the last-briefed timestamp so restarts don't re-send."
  (let* ((last-briefed (or (crichton/storage:store-get "rss" "last-briefed-at") 0))
         (now          (get-universal-time))
         (articles     (inbox-saved-since last-briefed)))
    (crichton/storage:store-set "rss" "last-briefed-at" now)
    (crichton/storage:flush-all-storage)
    (if (null articles)
        (log:info "RSS morning briefing: nothing curated since last briefing")
        (let* ((count (length articles))
               (text  (with-output-to-string (s)
                        (format s "Morning briefing: ~D curated article~:P~%" count)
                        (dolist (a articles)
                          (format s "• ~A [~A]~%"
                                  (getf a :title)
                                  (getf a :feed-name))))))
          (log:info "RSS morning briefing: ~D article~:P" count)
          (crichton/daemon:notification-post "rss-briefing" text "rss-curator")
          (handler-case
              (pushover-send text :title "Morning Briefing")
            (error (c)
              (log:warn "RSS morning briefing: Pushover delivery failed: ~A" c)))
          count))))

;;; --- On-demand query ---

(defun rss-inbox-query (&key feed-name (mark-reviewed t))
  "Return unreviewed inbox articles for FEED-NAME (or all feeds when NIL).
   Results are sorted by interest-score descending; unscored articles last.
   Marks returned articles as reviewed when MARK-REVIEWED is true (default)."
  (let ((articles (inbox-by-feed feed-name)))
    (when (and mark-reviewed articles)
      (inbox-mark-reviewed (mapcar (lambda (a) (getf a :guid)) articles)))
    articles))

(defun format-inbox-query-result (articles &key (stream *standard-output*) feed-name)
  "Format ARTICLES for display.  FEED-NAME is used in the header when supplied."
  (if (null articles)
      (format stream "No new articles~@[ in ~A~].~%" feed-name)
      (progn
        (format stream "~D unread article~:P~@[ in ~A~]:~%"
                (length articles) feed-name)
        (dolist (a articles)
          (format stream "~%  ~A~%" (or (getf a :title) "(no title)"))
          (when (getf a :link)
            (format stream "  ~A~%" (getf a :link)))
          (when (getf a :interest-score)
            (format stream "  Score: ~,2F~@[ — ~A~]~%"
                    (getf a :interest-score)
                    (getf a :reason)))
          (format stream "  [~A]~%" (getf a :feed-name))))))

;;; --- Scheduler integration ---

(defun start-rss-curator (&key (replace t))
  "Register the interest-scoring and morning-briefing scheduled tasks."
  (let ((score-interval
          (or (crichton/config:config-section-get :rss-curator :score-interval-seconds)
              (* 2 3600)))
        (briefing-hour
          (or (crichton/config:config-section-get :rss-curator :briefing-hour) 8))
        (briefing-minute
          (or (crichton/config:config-section-get :rss-curator :briefing-minute) 0)))
    (schedule-every "rss:curator" score-interval
                    #'run-interest-scorer
                    :replace replace)
    (schedule-daily "rss:morning-briefing" briefing-hour briefing-minute
                    #'rss-morning-briefing
                    :replace replace)
    (log:info "RSS curator started (scoring every ~Ds, briefing at ~D:~2,'0D)"
              score-interval briefing-hour briefing-minute)))

(defun restore-rss-curator ()
  "Start RSS curator scheduled tasks.  Called from daemon init-skills."
  (start-rss-curator :replace t))
