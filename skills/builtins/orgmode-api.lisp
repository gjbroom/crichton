;;;; skills/builtins/orgmode-api.lisp
;;;;
;;;; Public API for the org-mode skill: read, search, write, TODO management.
;;;; Depends on orgmode.lisp (config/parser) and orgmode-roam.lisp (DB layer).

(in-package #:crichton/skills)

;;; ====================================================================
;;; Public API
;;; ====================================================================

(defun orgmode-read (path-or-id &key include-raw)
  "Read and parse an org file.
   PATH-OR-ID can be a file path or an org-roam node ID (UUID).
   Returns parsed document as plist."
  (om-validate-enabled)
  (let ((path (if (and (= (length path-or-id) 36)
                       (cl-ppcre:scan "^[0-9a-f-]+$" path-or-id))
                  ;; Looks like a UUID — resolve via org-roam DB
                  (let ((node (org-roam-node-by-id path-or-id)))
                    (unless node
                      (error "Node ~A not found in org-roam database" path-or-id))
                    (getf node :file))
                  path-or-id)))
    (let ((canonical (om-validate-path path)))
      (document-to-plist (parse-org-file canonical) :include-raw include-raw))))

(defun orgmode-search (query &key (mode :auto) tag (limit 50))
  "Search for notes matching QUERY.
   MODE: :AUTO (default, uses DB then files), :DB (org-roam only), :FILES (scan only).
   TAG: filter by org-roam tag (DB modes only).
   Returns list of match plists."
  (om-validate-enabled)
  (let ((db-results nil)
        (file-results nil))
    ;; DB search
    (when (and (member mode '(:auto :db))
               (org-roam-db-available-p))
      (handler-case
          (setf db-results (org-roam-search-nodes query :tag tag :limit limit))
        (error (c)
          (log:warn "Org-roam DB search failed: ~A" c))))
    ;; File scan (for :auto when DB gave few results, or :files mode)
    (when (or (eq mode :files)
              (and (eq mode :auto)
                   (< (length db-results) limit)
                   (null tag)))
      (let ((remaining (- limit (length db-results)))
            (seen-files (make-hash-table :test #'equal)))
        ;; Mark DB results as seen
        (dolist (r db-results)
          (setf (gethash (getf r :file) seen-files) t))
        ;; Scan allowed paths
        (dolist (root (orgmode-allowed-paths))
          (when (<= remaining 0) (return))
          (dolist (file (om-collect-org-files root))
            (when (<= remaining 0) (return))
            (let ((fpath (namestring file)))
              (unless (gethash fpath seen-files)
                (let ((preamble (parse-org-preamble file)))
                  (when (or (search query (getf preamble :title)
                                    :test #'char-equal)
                            (search query (namestring file)
                                    :test #'char-equal))
                    (push preamble file-results)
                    (decf remaining)))))))))
    (append db-results (nreverse file-results))))

(defun orgmode-list-tags ()
  "List all tags from the org-roam database."
  (om-validate-enabled)
  (if (org-roam-db-available-p)
      (org-roam-list-tags)
      (error "Org-roam database not available")))

(defun orgmode-backlinks (id-or-path &key (direction :backlinks) (limit 50))
  "Get links for a node.
   DIRECTION: :BACKLINKS (default), :FORWARD, :BOTH.
   ID-OR-PATH: org-roam node ID or file path."
  (om-validate-enabled)
  (let ((node-id (if (and (= (length id-or-path) 36)
                          (cl-ppcre:scan "^[0-9a-f-]+$" id-or-path))
                     id-or-path
                     ;; Resolve file path to node ID via preamble parse
                     (let ((preamble (parse-org-preamble
                                      (om-validate-path id-or-path))))
                       (or (getf preamble :file-id)
                           (error "File ~A has no :ID: property" id-or-path))))))
    (let ((back (when (member direction '(:backlinks :both))
                  (org-roam-backlinks node-id :limit limit)))
          (fwd (when (member direction '(:forward :both))
                 (org-roam-forward-links node-id :limit limit))))
      (let ((results (append back fwd)))
        (subseq results 0 (min limit (length results)))))))

(defun orgmode-create-note (title &key root body filetags id)
  "Create a new org-roam note file.
   ROOT defaults to the first allowed path.
   Generates a UUID for :ID: if not provided.
   Returns the path of the created file."
  (om-validate-enabled)
  (let* ((root (or root (first (orgmode-allowed-paths))))
         (id (or id (format nil "~(~A~)" (ironclad:byte-array-to-hex-string
                                           (ironclad:random-data 16)))))
         ;; Format as proper UUID: 8-4-4-4-12
         (id (if (= (length id) 32)
                 (format nil "~A-~A-~A-~A-~A"
                         (subseq id 0 8) (subseq id 8 12)
                         (subseq id 12 16) (subseq id 16 20)
                         (subseq id 20))
                 id))
         (path (generate-roam-filename title :root root))
         (setupfile (crichton/config:config-section-get :orgmode :setupfile)))
    (om-validate-new-path path)
    (let ((preamble (render-org-preamble title :id id :filetags filetags
                                              :setupfile setupfile)))
      (bt:with-lock-held (*orgmode-write-lock*)
        (with-open-file (s path :direction :output :if-does-not-exist :create)
          (write-string preamble s)
          (when body
            (write-string body s)
            (unless (and (plusp (length body))
                         (char= (char body (1- (length body))) #\Newline))
              (terpri s)))))
      (log:info "Created org note: ~A" (namestring path))
      (namestring path))))

(defun orgmode-append (path-or-id text &key headline)
  "Append TEXT to an org file.
   If HEADLINE is given, append under the first matching headline.
   PATH-OR-ID can be a file path or org-roam node ID."
  (om-validate-enabled)
  (let ((path (if (and (= (length path-or-id) 36)
                       (cl-ppcre:scan "^[0-9a-f-]+$" path-or-id))
                  (let ((node (org-roam-node-by-id path-or-id)))
                    (unless node
                      (error "Node ~A not found in org-roam database" path-or-id))
                    (getf node :file))
                  path-or-id)))
    (let ((canonical (om-validate-path path)))
      (bt:with-lock-held (*orgmode-write-lock*)
        (if headline
            ;; Insert under specific headline
            (let* ((doc (parse-org-file canonical))
                   (target (find headline (doc-headlines doc)
                                :key #'headline-title
                                :test #'string-equal)))
              (unless target
                (error "Headline ~S not found in ~A" headline canonical))
              ;; Read file, insert after headline's last line
              (let* ((lines (cl-ppcre:split "\\n" (doc-raw-text doc)))
                     (insert-at (1+ (headline-end-line target)))
                     (new-lines (append (subseq lines 0 insert-at)
                                        (list text)
                                        (subseq lines insert-at))))
                (with-open-file (s canonical :direction :output
                                             :if-exists :supersede)
                  (format s "~{~A~^~%~}" new-lines))))
            ;; Append to end of file
            (with-open-file (s canonical :direction :output
                                         :if-exists :append)
              (terpri s)
              (write-string text s)
              (terpri s))))
      (log:info "Appended to org file: ~A" canonical)
      canonical)))

(defun orgmode-list-files (&key root (limit 200))
  "List org files under allowed paths, returning preamble data.
   ROOT: specific root to list (must be in allowed_paths).
   Returns list of preamble plists."
  (om-validate-enabled)
  (let ((roots (if root
                   (if (orgmode-path-allowed-p
                        (namestring (truename root)))
                       (list root)
                       (error "Root ~A is not in allowed_paths" root))
                   (orgmode-allowed-paths)))
        (results nil)
        (count 0))
    (dolist (r roots)
      (when (>= count limit) (return))
      (dolist (file (om-collect-org-files r))
        (when (>= count limit) (return))
        (push (parse-org-preamble file) results)
        (incf count)))
    (nreverse results)))

(defun orgmode-read-file (path &key (max-chars 50000))
  "Read a non-org file as raw text, subject to the same path allowlist.
   MAX-CHARS: return only the first MAX-CHARS characters (default: 50000).
   Returns the file contents as a string."
  (let* ((canonical (om-validate-path path))
         (content (uiop:read-file-string canonical)))
    (if (> (length content) max-chars)
        (subseq content 0 max-chars)
        content)))

(defun orgmode-status ()
  "Return orgmode skill status as a plist."
  (list :enabled (orgmode-enabled-p)
        :allowed-paths (orgmode-allowed-paths)
        :db-path (orgmode-db-path)
        :db-available (org-roam-db-available-p)))

;;; ====================================================================
;;; TODO management
;;; ====================================================================

(defun om-active-todo-keywords ()
  "Return active (non-done) TODO keywords from config or defaults."
  (or (let ((val (crichton/config:config-section-get :orgmode :todo-keywords)))
        (when (listp val) val))
      '("TODO" "NEXT" "WAITING" "SOMEDAY")))

(defun om-done-keywords ()
  "Return done TODO keywords from config or defaults."
  (or (let ((val (crichton/config:config-section-get :orgmode :done-keywords)))
        (when (listp val) val))
      '("DONE" "CANCELLED")))

(defun om-todo-from-db (state priority tag limit include-done)
  "Query org-roam DB for TODO headlines, filtered by allowed paths."
  (when (org-roam-db-available-p)
    (handler-case
        (with-org-roam-db (db)
          (let* ((conditions (list "n.todo IS NOT NULL"))
                 (params nil)
                 (param-idx 0))
            (unless (or include-done state)
              ;; Skip the exclusion when state is explicit — the equality
              ;; filter below is already restrictive, and combining both
              ;; would produce a contradiction for done-state queries.
              (let ((done (om-done-keywords)))
                (push (format nil "n.todo NOT IN (~{?~^, ~})"
                              (make-list (length done) :initial-element nil))
                      conditions)
                (dolist (d done) (push (om-db-string d) params))))
            (when state
              (push (format nil "n.todo = ?") conditions)
              (push (om-db-string state) params))
            (when priority
              (push (format nil "n.priority = ?") conditions)
              (push (om-db-string priority) params))
            (let* ((where (format nil "~{~A~^ AND ~}" (nreverse conditions)))
                   (sql (if tag
                            (format nil "SELECT DISTINCT n.id, n.file, n.title, n.level, n.todo, n.priority ~
                                         FROM nodes n JOIN tags t ON t.node_id = n.id ~
                                         WHERE ~A AND t.tag = ? LIMIT ?" where)
                            (format nil "SELECT n.id, n.file, n.title, n.level, n.todo, n.priority ~
                                         FROM nodes n WHERE ~A LIMIT ?" where)))
                   (stmt (sqlite:prepare-statement db sql))
                   (bind-params (nreverse params)))
              (unwind-protect
                   (progn
                     (loop for p in bind-params
                           do (sqlite:bind-parameter stmt (incf param-idx) p))
                     (when tag
                       (sqlite:bind-parameter stmt (incf param-idx) (om-db-string tag)))
                     (sqlite:bind-parameter stmt (incf param-idx) limit)
                     (om-filter-by-allowed-paths
                      (om-sql-rows-to-plists
                       stmt '("id" "file" "title" "level" "todo" "priority"))))
                (sqlite:finalize-statement stmt)))))
      (error (c)
        (log:warn "Org-roam TODO query failed: ~A" c)
        nil))))

(defun om-scan-file-todos (path active-kws done-kws state priority include-done)
  "Lightweight headline-only scan of PATH for TODO items.
   Reads line-by-line without loading the full file into memory.
   Returns a list of TODO plists."
  (let ((results nil)
        (file-title nil)
        (line-num 0))
    (handler-case
        (with-open-file (s path :direction :input :if-does-not-exist nil)
          (when s
            (loop for line = (read-line s nil nil)
                  while line
                  do (incf line-num)
                  do (cond
                       ;; Grab title from preamble
                       ((and (null file-title)
                             (multiple-value-bind (match regs)
                                 (cl-ppcre:scan-to-strings *file-keyword-re* line)
                               (when (and match
                                          (string-equal "TITLE" (aref regs 0)))
                                 (setf file-title (aref regs 1))
                                 t))))
                       ;; Match headlines with TODO keywords
                       ((multiple-value-bind (match regs)
                            (cl-ppcre:scan-to-strings *headline-re* line)
                          (when (and match (aref regs 1))
                            (let ((todo-kw (aref regs 1))
                                  (h-priority (aref regs 2))
                                  (h-title (aref regs 3))
                                  (h-tags (parse-headline-tags (aref regs 4))))
                              (when (and (or include-done
                                             (member todo-kw active-kws
                                                     :test #'string-equal))
                                         (not (and (not include-done)
                                                   (member todo-kw done-kws
                                                           :test #'string-equal)))
                                         (or (null state)
                                             (string-equal state todo-kw))
                                         (or (null priority)
                                             (string-equal priority h-priority)))
                                (push (list :file (namestring path)
                                            :title (or file-title "")
                                            :headline h-title
                                            :level (length (aref regs 0))
                                            :todo todo-kw
                                            :priority h-priority
                                            :tags h-tags
                                            :line line-num)
                                      results)))
                            t)))))))
      (error () nil))
    (nreverse results)))

(defun om-todo-from-files (file state priority include-done limit existing-count)
  "Scan org files for TODO headlines. Returns list of plists."
  (let ((results nil)
        (remaining (- limit existing-count))
        (active-kws (om-active-todo-keywords))
        (done-kws (om-done-keywords)))
    (flet ((scan-file (path)
             (when (<= remaining 0) (return-from scan-file))
             (let ((todos (om-scan-file-todos
                           path active-kws done-kws
                           state priority include-done)))
               (dolist (todo todos)
                 (when (<= remaining 0) (return))
                 (push todo results)
                 (decf remaining)))))
      (if file
          ;; Single file mode
          (let ((canonical (om-validate-path file)))
            (scan-file canonical))
          ;; Scan all allowed paths
          (dolist (root (orgmode-allowed-paths))
            (when (<= remaining 0) (return))
            (dolist (f (om-collect-org-files root))
              (when (<= remaining 0) (return))
              (scan-file (namestring f))))))
    (nreverse results)))

(defun orgmode-list-todos (&key state priority tag file include-done (limit 50))
  "List TODO items across allowed paths.
   STATE: filter by specific keyword (\"TODO\", \"NEXT\", etc.).
   PRIORITY: filter by priority (\"A\", \"B\", \"C\").
   TAG: filter by org-roam tag (DB mode only).
   FILE: restrict to a single file path.
   INCLUDE-DONE: include DONE/CANCELLED items.
   Returns list of TODO item plists."
  (om-validate-enabled)
  (let ((db-results (unless file
                      (om-todo-from-db state priority tag limit include-done))))
    (if (and db-results (>= (length db-results) limit))
        db-results
        ;; Supplement with file scanning when DB results are insufficient
        (let ((file-results (om-todo-from-files
                             file state priority include-done limit
                             (length db-results))))
          (if db-results
              ;; Merge, dedup by file+headline
              (let ((seen (make-hash-table :test #'equal)))
                (dolist (r db-results)
                  (setf (gethash (cons (getf r :file) (getf r :title)) seen) t))
                (append db-results
                        (remove-if (lambda (r)
                                     (gethash (cons (getf r :file) (getf r :headline))
                                              seen))
                                   file-results)))
              file-results)))))

(defun om-format-timestamp ()
  "Format current time as an org-mode inactive timestamp: YYYY-MM-DD Day HH:MM."
  (multiple-value-bind (sec min hr day mon yr dow)
      (get-decoded-time)
    (declare (ignore sec))
    (let ((day-name (nth dow '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))))
      (format nil "~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D"
              yr mon day day-name hr min))))

(defun orgmode-set-filetags (path-or-id tags)
  "Set the file-level tags of an org file.
   PATH-OR-ID: file path or org-roam node ID.
   TAGS: list of tag strings (replaces all existing filetags).
   An empty list removes the #+FILETAGS line entirely.
   Returns the canonical path of the modified file."
  (om-validate-enabled)
  (let ((path (if (and (= (length path-or-id) 36)
                       (cl-ppcre:scan "^[0-9a-f-]+$" path-or-id))
                  (let ((node (org-roam-node-by-id path-or-id)))
                    (unless node
                      (error "Node ~A not found in org-roam database" path-or-id))
                    (getf node :file))
                  path-or-id)))
    (let ((canonical (om-validate-path path)))
      (bt:with-lock-held (*orgmode-write-lock*)
        (let* ((text (uiop:read-file-string canonical))
               (lines (cl-ppcre:split "\\n" text))
               (filetags-idx nil)
               (title-idx nil))
          ;; Find #+FILETAGS and #+TITLE lines in the preamble
          (loop for line in lines
                for idx from 0
                when (and (plusp (length line)) (char= (char line 0) #\*))
                  do (return)
                do (multiple-value-bind (match regs)
                       (cl-ppcre:scan-to-strings *file-keyword-re* line)
                     (when match
                       (let ((key (string-upcase (aref regs 0))))
                         (cond
                           ((string= key "FILETAGS") (setf filetags-idx idx))
                           ((string= key "TITLE") (setf title-idx idx)))))))
          (cond
            ;; Replace existing #+FILETAGS line
            ((and filetags-idx tags)
             (setf (nth filetags-idx lines)
                   (format nil "#+FILETAGS: ~{:~A~}:" tags)))
            ;; Remove existing #+FILETAGS line (empty tag list)
            ((and filetags-idx (null tags))
             (setf lines (append (subseq lines 0 filetags-idx)
                                 (subseq lines (1+ filetags-idx)))))
            ;; Insert new #+FILETAGS line after #+TITLE
            ((and (null filetags-idx) tags)
             (let ((insert-at (if title-idx (1+ title-idx) 0)))
               (setf lines (append (subseq lines 0 insert-at)
                                   (list (format nil "#+FILETAGS: ~{:~A~}:" tags))
                                   (subseq lines insert-at))))))
          ;; Write back
          (with-open-file (s canonical :direction :output :if-exists :supersede)
            (format s "~{~A~^~%~}" lines))))
      (log:info "Set filetags to ~S in ~A" tags canonical)
      canonical)))

(defun orgmode-set-todo (path-or-id headline new-state)
  "Change a headline's TODO state in an org file.
   PATH-OR-ID: file path or org-roam node ID.
   HEADLINE: title of the headline to modify.
   NEW-STATE: new TODO keyword (e.g. \"DONE\") or NIL/empty string to clear.
   Returns the canonical path of the modified file."
  (om-validate-enabled)
  (let ((path (if (and (= (length path-or-id) 36)
                       (cl-ppcre:scan "^[0-9a-f-]+$" path-or-id))
                  (let ((node (org-roam-node-by-id path-or-id)))
                    (unless node
                      (error "Node ~A not found in org-roam database" path-or-id))
                    (getf node :file))
                  path-or-id)))
    (let ((canonical (om-validate-path path)))
      (bt:with-lock-held (*orgmode-write-lock*)
        (let* ((text (uiop:read-file-string canonical))
               (lines (cl-ppcre:split "\\n" text))
               (lines-vec (coerce lines 'vector))
               (doc (parse-org-file canonical :text text))
               (target (find headline (doc-headlines doc)
                             :key #'headline-title
                             :test #'string-equal)))
          (unless target
            (error "Headline ~S not found in ~A" headline canonical))
          (let* ((line-idx (headline-start-line target))
                 (old-line (aref lines-vec line-idx))
                 (old-todo (headline-todo target))
                 (new-state (if (and new-state (plusp (length new-state)))
                                new-state
                                nil))
                 (new-line (cond
                             ;; Replace existing TODO keyword
                             ((and old-todo new-state)
                              (cl-ppcre:regex-replace
                               (format nil "\\b~A\\b" (cl-ppcre:quote-meta-chars old-todo))
                               old-line new-state))
                             ;; Add TODO keyword (none existed)
                             ((and (null old-todo) new-state)
                              (cl-ppcre:regex-replace
                               "^(\\*+\\s+)" old-line
                               (format nil "\\1~A " new-state)))
                             ;; Remove TODO keyword
                             ((and old-todo (null new-state))
                              (cl-ppcre:regex-replace
                               (format nil "\\b~A\\s+" (cl-ppcre:quote-meta-chars old-todo))
                               old-line ""))
                             (t old-line)))
                 (result-lines (coerce lines-vec 'list)))
            ;; Replace the headline line
            (setf (nth line-idx result-lines) new-line)
            ;; Handle CLOSED timestamp for done transitions
            (when (and new-state
                       (member new-state (om-done-keywords) :test #'string-equal)
                       (not (headline-closed target)))
              (let ((closed-line (format nil "    CLOSED: [~A]"
                                        (om-format-timestamp)))
                    (insert-at (1+ line-idx)))
                ;; Skip property drawer if present
                (when (and (< insert-at (length result-lines))
                           (cl-ppcre:scan *property-drawer-start-re*
                                          (nth insert-at result-lines)))
                  (loop for j from insert-at below (length result-lines)
                        when (cl-ppcre:scan *property-drawer-end-re*
                                            (nth j result-lines))
                        do (setf insert-at (1+ j)) (return)))
                ;; Skip existing planning line if present
                (when (and (< insert-at (length result-lines))
                           (cl-ppcre:scan "^\\s*(?:SCHEDULED|DEADLINE):"
                                          (nth insert-at result-lines)))
                  (setf (nth insert-at result-lines)
                        (format nil "~A ~A" closed-line
                                (string-left-trim " " (nth insert-at result-lines))))
                  (setf closed-line nil))
                (when closed-line
                  (setf result-lines
                        (append (subseq result-lines 0 insert-at)
                                (list closed-line)
                                (subseq result-lines insert-at))))))
            ;; Write back
            (with-open-file (s canonical :direction :output :if-exists :supersede)
              (format s "~{~A~^~%~}" result-lines)))))
      (log:info "Set TODO state to ~A for ~S in ~A" new-state headline canonical)
      canonical)))
