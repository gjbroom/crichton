;;;; skills/builtins/orgmode-roam.lisp
;;;;
;;;; Org-roam SQLite layer: database queries, writer helpers, and
;;;; recursive file scanning.  Depends on orgmode.lisp for config/path
;;;; policy and org-document data structures.

(in-package #:crichton/skills)

;;; ====================================================================
;;; Org-roam SQLite layer
;;; ====================================================================

(defun org-roam-db-available-p ()
  "Return T if the org-roam database exists and is readable."
  (let ((db-path (orgmode-db-path)))
    (and db-path
         (probe-file db-path)
         (plusp (with-open-file (s db-path :direction :input
                                          :element-type '(unsigned-byte 8)
                                          :if-does-not-exist nil)
                 (if s (file-length s) 0))))))

(defmacro with-org-roam-db ((db-var) &body body)
  "Open the org-roam database read-only, bind to DB-VAR, execute BODY."
  `(let ((db-path (orgmode-db-path)))
     (unless (org-roam-db-available-p)
       (error "Org-roam database not available at ~A" db-path))
     (sqlite:with-open-database (,db-var db-path)
       ,@body)))

(defun om-unquote-string (s)
  "Strip the surrounding double-quotes that org-roam's emacsql layer adds to all
   string values.  E.g. '\"programming\"' → 'programming'.  Non-strings and
   strings that do not start and end with '\"' are returned unchanged."
  (if (and (stringp s)
           (>= (length s) 2)
           (char= (char s 0) #\")
           (char= (char s (1- (length s))) #\"))
      (subseq s 1 (1- (length s)))
      s))

(defun om-db-string (s)
  "Wrap S in double-quotes for exact-match binding against the org-roam database.
   All string values are stored by emacsql with surrounding double-quotes, so
   exact comparisons (=, IN) must match that encoding."
  (format nil "\"~A\"" s))

(defun om-filter-by-allowed-paths (rows &key (file-key "file"))
  "Filter a list of row plists, keeping only those whose FILE-KEY value
   is under an allowed path.  Values are expected to be already unquoted."
  (remove-if-not (lambda (row)
                   (let ((file (getf row (intern (string-upcase file-key) :keyword))))
                     (and file (orgmode-path-allowed-p file))))
                 rows))

(defun om-sql-rows-to-plists (stmt column-names)
  "Read all rows from a sqlite statement, returning a list of plists.
   String values are unquoted via OM-UNQUOTE-STRING to strip the surrounding
   double-quotes that org-roam's emacsql layer adds to every string."
  (let (result)
    (loop for row = (sqlite:step-statement stmt)
          while row
          do (let (plist)
               (loop for i from (1- (length column-names)) downto 0
                     do (let ((v (sqlite:statement-column-value stmt i)))
                          (push (if (stringp v) (om-unquote-string v) v) plist))
                     do (push (intern (string-upcase
                                       (substitute #\- #\_ (nth i column-names)))
                                      :keyword)
                              plist))
               (push plist result)))
    (nreverse result)))

(defun org-roam-search-nodes (query &key tag (limit 50))
  "Search org-roam nodes by title (LIKE match) and/or tag.
   Returns a list of plists, filtered by allowed paths."
  (with-org-roam-db (db)
    (let ((sql (if tag
                   "SELECT DISTINCT n.id, n.file, n.title, n.level, n.todo, n.priority
                    FROM nodes n
                    JOIN tags t ON t.node_id = n.id
                    WHERE n.title LIKE ? AND t.tag = ?
                    LIMIT ?"
                   "SELECT n.id, n.file, n.title, n.level, n.todo, n.priority
                    FROM nodes n
                    WHERE n.title LIKE ?
                    LIMIT ?"))
          (cols '("id" "file" "title" "level" "todo" "priority")))
      (let ((stmt (sqlite:prepare-statement db sql)))
        (unwind-protect
             (progn
               (sqlite:bind-parameter stmt 1 (format nil "%~A%" query))
               (if tag
                   (progn
                     (sqlite:bind-parameter stmt 2 (om-db-string tag))
                     (sqlite:bind-parameter stmt 3 limit))
                   (sqlite:bind-parameter stmt 2 limit))
               (om-filter-by-allowed-paths
                (om-sql-rows-to-plists stmt cols)))
          (sqlite:finalize-statement stmt))))))

(defun org-roam-backlinks (node-id &key (limit 50))
  "Return nodes that link TO this node-id.
   Results filtered by allowed paths, capped at LIMIT."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT n.id, n.file, n.title, n.level
                 FROM links l
                 JOIN nodes n ON n.id = l.source
                 WHERE l.dest = ? AND l.type = '\"id\"'
                 LIMIT ?")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 (om-db-string node-id))
             (sqlite:bind-parameter stmt 2 limit)
             (om-filter-by-allowed-paths
              (om-sql-rows-to-plists stmt '("id" "file" "title" "level"))))
        (sqlite:finalize-statement stmt)))))

(defun org-roam-forward-links (node-id &key (limit 50))
  "Return nodes that this node-id links TO.
   Results filtered by allowed paths, capped at LIMIT."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT n.id, n.file, n.title, n.level
                 FROM links l
                 JOIN nodes n ON n.id = l.dest
                 WHERE l.source = ? AND l.type = '\"id\"'
                 LIMIT ?")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 (om-db-string node-id))
             (sqlite:bind-parameter stmt 2 limit)
             (om-filter-by-allowed-paths
              (om-sql-rows-to-plists stmt '("id" "file" "title" "level"))))
        (sqlite:finalize-statement stmt)))))

(defun org-roam-list-tags ()
  "Return all distinct tags from nodes under allowed paths."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT t.tag, n.file
                 FROM tags t
                 JOIN nodes n ON n.id = t.node_id
                 ORDER BY t.tag")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (let* ((rows (om-sql-rows-to-plists stmt '("tag" "file")))
                  (allowed (om-filter-by-allowed-paths rows))
                  (tag-names nil))
             (dolist (row allowed)
               (pushnew (getf row :tag) tag-names :test #'string-equal))
             (sort tag-names #'string<))
        (sqlite:finalize-statement stmt)))))

(defun org-roam-node-by-id (node-id)
  "Look up a single node by ID. Returns plist or NIL.
   Filtered by allowed paths."
  (with-org-roam-db (db)
    (let* ((sql "SELECT n.id, n.file, n.title, n.level, n.todo, n.priority
                 FROM nodes n WHERE n.id = ?")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 (om-db-string node-id))
             (let ((results (om-filter-by-allowed-paths
                             (om-sql-rows-to-plists
                              stmt '("id" "file" "title" "level" "todo" "priority")))))
               (first results)))
        (sqlite:finalize-statement stmt)))))

;;; ====================================================================
;;; Writer helpers
;;; ====================================================================

(defvar *orgmode-write-lock* (bt:make-lock "orgmode-write")
  "Serialize write operations to org files.")

(defun generate-roam-filename (title &key root)
  "Generate an org-roam style filename: YYYYMMDDHHMMSS-slug.org
   ROOT defaults to the first allowed path."
  (let* ((root (or root (first (orgmode-allowed-paths))))
         (timestamp (multiple-value-bind (sec min hr day mon yr)
                        (get-decoded-time)
                      (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
                              yr mon day hr min sec)))
         (slug (string-downcase
                (cl-ppcre:regex-replace-all
                 "[^a-zA-Z0-9]+" title "_")))
         (slug (string-right-trim "_" (string-left-trim "_" slug)))
         (filename (format nil "~A-~A.org" timestamp slug)))
    (merge-pathnames filename (parse-namestring
                               (om-ensure-trailing-slash root)))))

(defun render-org-preamble (title &key id filetags setupfile)
  "Generate an org file preamble string."
  (with-output-to-string (s)
    (when id
      (format s ":PROPERTIES:~%:ID:       ~A~%:END:~%" id))
    (when setupfile
      (format s "#+SETUPFILE: ~A~%" setupfile))
    (when filetags
      (format s "#+FILETAGS: ~{:~A~}:~%" filetags))
    (format s "#+TITLE: ~A~%~%" title)))

;;; ====================================================================
;;; Recursive file scanning
;;; ====================================================================

(defun om-recursive-scan-p ()
  "Return T if recursive scanning is enabled (default: T)."
  (let ((val (crichton/config:config-section-get :orgmode :recursive-scan)))
    (cond
      ((null val) t)
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      ((and (stringp val) (string-equal val "false")) nil)
      (t t))))

(defun om-max-scan-depth ()
  "Return max directory depth for recursive scan (default: 5)."
  (let ((val (crichton/config:config-section-get :orgmode :max-scan-depth)))
    (cond
      ((integerp val) val)
      ((stringp val) (handler-case (parse-integer val) (error () 5)))
      (t 5))))

(defun org-files-under (root &key (max-depth 5) (limit 1000))
  "Recursively collect .org files under ROOT, up to MAX-DEPTH levels.
   Returns a list of pathname objects, capped at LIMIT.
   Tracks visited directories by truename to avoid symlink cycles."
  (let ((results nil)
        (count 0)
        (visited (make-hash-table :test #'equal)))
    (labels ((scan-dir (dir depth)
               (when (or (> depth max-depth) (>= count limit))
                 (return-from scan-dir))
               (let ((truedir (handler-case
                                  (namestring (truename dir))
                                (error () nil))))
                 (unless truedir (return-from scan-dir))
                 (when (gethash truedir visited)
                   (return-from scan-dir))
                 (setf (gethash truedir visited) t))
               (handler-case
                   (progn
                     (dolist (file (directory (merge-pathnames "*.org" dir)))
                       (when (>= count limit) (return))
                       (push file results)
                       (incf count))
                     (dolist (subdir (uiop:subdirectories dir))
                       (when (>= count limit) (return))
                       (scan-dir subdir (1+ depth))))
                 (error () nil))))
      (let ((canonical (om-canonical-dir root)))
        (when canonical
          (scan-dir (parse-namestring canonical) 0))))
    (nreverse results)))

(defun om-collect-org-files (root)
  "Collect .org files under ROOT, respecting recursive_scan config."
  (if (om-recursive-scan-p)
      (org-files-under root :max-depth (om-max-scan-depth))
      (directory (merge-pathnames "*.org"
                                 (parse-namestring
                                  (om-ensure-trailing-slash root))))))
