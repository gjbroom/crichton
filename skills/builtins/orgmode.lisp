;;;; skills/builtins/orgmode.lisp
;;;;
;;;; Built-in skill: Org-mode file operations and org-roam integration.
;;;; Parses org-mode structure, queries the org-roam SQLite database,
;;;; and supports creating/appending to org files.
;;;;
;;;; LOCAL-ONLY: Gated by [orgmode] config section. Requires explicit
;;;; allowed_paths configuration. Never exposes file contents to channels.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; ====================================================================
;;; Config gate and path policy
;;; ====================================================================

(defun orgmode-enabled-p ()
  "Return T if org-mode skill is enabled in config.
   Requires [orgmode] enable = true in config.toml."
  (let ((val (crichton/config:config-section-get :orgmode :enable)))
    (cond
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      (t nil))))

(defun orgmode-allowed-paths ()
  "Return the list of allowed directory paths from config.
   Each entry is a directory path string."
  (let ((paths (crichton/config:config-section-get :orgmode :allowed-paths)))
    (cond
      ((null paths) nil)
      ((listp paths) paths)
      ((stringp paths) (list paths))
      (t nil))))

(defun orgmode-db-path ()
  "Return the org-roam database path from config, or the default."
  (or (crichton/config:config-section-get :orgmode :db-path)
      (namestring (merge-pathnames ".emacs.d/org-roam.db"
                                   (user-homedir-pathname)))))

(defun %om-ensure-trailing-slash (path)
  "Ensure PATH ends with a slash."
  (let ((s (namestring path)))
    (if (char= (char s (1- (length s))) #\/)
        s
        (concatenate 'string s "/"))))

(defun %om-canonical-dir (path)
  "Return a canonical directory namestring for PATH, or NIL if it doesn't exist."
  (handler-case
      (let ((truepath (truename (parse-namestring path))))
        (%om-ensure-trailing-slash truepath))
    (error () nil)))

(defun orgmode-path-allowed-p (path)
  "Return the matching allowed root if PATH is under an allowed root, or NIL.
   Unlike Amp, an empty allowlist means DENY all (never unrestricted)."
  (let ((roots (orgmode-allowed-paths)))
    (when (null roots)
      (return-from orgmode-path-allowed-p nil))
    (let ((canonical (handler-case
                         (namestring (truename path))
                       (error () nil))))
      (unless canonical
        (return-from orgmode-path-allowed-p nil))
      (dolist (root roots nil)
        (let ((canonical-root (%om-canonical-dir root)))
          (when (and canonical-root
                     (>= (length canonical) (length canonical-root))
                     (string= canonical canonical-root
                              :end1 (length canonical-root)))
            (return canonical-root)))))))

(defun %om-validate-enabled ()
  "Check that orgmode is enabled. Signals error if not."
  (unless (orgmode-enabled-p)
    (error "Org-mode skill is disabled (set [orgmode] enable = true in config.toml)")))

(defun %om-validate-path (path)
  "Validate PATH is allowed. Returns canonical path string. Signals error if denied."
  (%om-validate-enabled)
  (let ((canonical (handler-case
                       (namestring (truename path))
                     (error ()
                       (error "Path does not exist: ~A" path)))))
    (unless (orgmode-path-allowed-p canonical)
      (error "Path ~A is not under any allowed orgmode root" canonical))
    canonical))

(defun %om-validate-new-path (path)
  "Validate a path for a new file (parent must exist and be allowed).
   Returns the path string. Signals error if denied."
  (%om-validate-enabled)
  (let* ((dir (directory-namestring path))
         (canonical-dir (handler-case
                            (namestring (truename dir))
                          (error ()
                            (error "Parent directory does not exist: ~A" dir)))))
    (unless (orgmode-path-allowed-p canonical-dir)
      (error "Path ~A is not under any allowed orgmode root" path))
    (when (probe-file path)
      (error "File already exists: ~A" path))
    (namestring path)))

;;; ====================================================================
;;; Org parser — targeted line scanner
;;; ====================================================================

(defclass org-headline ()
  ((level :initarg :level :initform 0 :accessor headline-level)
   (todo :initarg :todo :initform nil :accessor headline-todo)
   (priority :initarg :priority :initform nil :accessor headline-priority)
   (title :initarg :title :initform "" :accessor headline-title)
   (tags :initarg :tags :initform nil :accessor headline-tags)
   (properties :initarg :properties :initform nil :accessor headline-properties)
   (start-line :initarg :start-line :initform 0 :accessor headline-start-line)
   (end-line :initarg :end-line :initform 0 :accessor headline-end-line)))

(defclass org-document ()
  ((path :initarg :path :initform nil :accessor doc-path)
   (title :initarg :title :initform "" :accessor doc-title)
   (filetags :initarg :filetags :initform nil :accessor doc-filetags)
   (file-id :initarg :file-id :initform nil :accessor doc-file-id)
   (headlines :initarg :headlines :initform nil :accessor doc-headlines)
   (links :initarg :links :initform nil :accessor doc-links)
   (raw-text :initarg :raw-text :initform nil :accessor doc-raw-text)))

;;; --- Regex patterns ---

(defparameter *headline-re*
  (cl-ppcre:create-scanner
   "^(\\*+)\\s+(?:(TODO|DONE|NEXT|WAITING|CANCELLED|SOMEDAY)\\s+)?(?:\\[#([A-C])\\]\\s+)?(.*?)(?:\\s+(:[\\w:]+:))?\\s*$")
  "Match org headline: stars, optional TODO keyword, optional priority, title, optional tags.")

(defparameter *todo-keywords*
  '("TODO" "DONE" "NEXT" "WAITING" "CANCELLED" "SOMEDAY")
  "Recognized TODO keywords.")

(defparameter *property-drawer-start-re*
  (cl-ppcre:create-scanner "^\\s*:PROPERTIES:\\s*$" :case-insensitive-mode t))

(defparameter *property-drawer-end-re*
  (cl-ppcre:create-scanner "^\\s*:END:\\s*$" :case-insensitive-mode t))

(defparameter *property-line-re*
  (cl-ppcre:create-scanner "^\\s*:(\\S+?):\\s+(.+?)\\s*$"))

(defparameter *file-keyword-re*
  (cl-ppcre:create-scanner "^#\\+(\\w+):\\s*(.*?)\\s*$" :case-insensitive-mode t))

(defparameter *id-link-re*
  (cl-ppcre:create-scanner "\\[\\[id:([0-9a-f-]+)\\](?:\\[([^\\]]+)\\])?\\]"
                           :case-insensitive-mode t))

(defparameter *src-block-begin-re*
  (cl-ppcre:create-scanner "^\\s*#\\+BEGIN_SRC\\s*(\\S*)" :case-insensitive-mode t))

(defparameter *src-block-end-re*
  (cl-ppcre:create-scanner "^\\s*#\\+END_SRC" :case-insensitive-mode t))

;;; --- Tag parser ---

(defun parse-headline-tags (tag-string)
  "Parse ':tag1:tag2:tag3:' into a list of tag strings."
  (when (and tag-string (plusp (length tag-string)))
    (remove-if (lambda (s) (zerop (length s)))
               (cl-ppcre:split ":" tag-string))))

;;; --- Property drawer parser ---

(defun parse-property-drawer (lines start)
  "Parse a property drawer starting at line index START.
   Returns (values properties end-index) where properties is an alist."
  (let ((props nil)
        (i (1+ start)))
    (loop while (< i (length lines))
          for line = (aref lines i)
          do (cond
               ((cl-ppcre:scan *property-drawer-end-re* line)
                (return-from parse-property-drawer (values (nreverse props) i)))
               ((multiple-value-bind (match regs)
                    (cl-ppcre:scan-to-strings *property-line-re* line)
                  (when match
                    (push (cons (aref regs 0) (aref regs 1)) props)
                    t)))
               (t nil))
          do (incf i))
    (values (nreverse props) i)))

;;; --- Preamble-only parse (fast) ---

(defun parse-org-preamble (path)
  "Fast preamble-only parse. Returns plist:
   (:path PATH :title TITLE :file-id ID :filetags (tag ...) :setupfile SF)"
  (let ((title nil) (file-id nil) (filetags nil) (setupfile nil)
        (in-preamble-drawer nil))
    (with-open-file (s path :direction :input :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              for line-num from 1
              while (and line (< line-num 50))
              do (cond
                   ;; Property drawer
                   ((cl-ppcre:scan *property-drawer-start-re* line)
                    (setf in-preamble-drawer t))
                   ((and in-preamble-drawer
                         (cl-ppcre:scan *property-drawer-end-re* line))
                    (setf in-preamble-drawer nil))
                   ((and in-preamble-drawer
                         (multiple-value-bind (match regs)
                             (cl-ppcre:scan-to-strings *property-line-re* line)
                           (when (and match (string-equal "ID" (aref regs 0)))
                             (setf file-id (aref regs 1))
                             t))))
                   ;; File keywords
                   ((multiple-value-bind (match regs)
                        (cl-ppcre:scan-to-strings *file-keyword-re* line)
                      (when match
                        (let ((key (string-upcase (aref regs 0)))
                              (val (aref regs 1)))
                          (cond
                            ((string= key "TITLE") (setf title val))
                            ((string= key "FILETAGS")
                             (setf filetags (parse-headline-tags val)))
                            ((string= key "SETUPFILE") (setf setupfile val))))
                        t)))
                   ;; Stop at first headline
                   ((and (plusp (length line)) (char= (char line 0) #\*))
                    (return))))))
    (list :path (namestring path)
          :title (or title "")
          :file-id file-id
          :filetags filetags
          :setupfile setupfile)))

;;; --- Full parse ---

(defun parse-org-file (path)
  "Parse an org file into an org-document instance.
   Returns the org-document."
  (let* ((text (uiop:read-file-string path))
         (lines (coerce (cl-ppcre:split "\\n" text) 'vector))
         (title nil) (file-id nil) (filetags nil)
         (headlines nil) (links nil)
         (current-headline nil)
         (in-preamble-drawer nil))
    ;; Scan all lines
    (loop for i from 0 below (length lines)
          for line = (aref lines i)
          do (cond
               ;; Property drawer boundaries
               ((cl-ppcre:scan *property-drawer-start-re* line)
                (when (null current-headline)
                  (setf in-preamble-drawer t))
                (when current-headline
                  (multiple-value-bind (props end-idx)
                      (parse-property-drawer lines i)
                    (setf (headline-properties current-headline) props
                          i end-idx))))
               ((and in-preamble-drawer
                     (cl-ppcre:scan *property-drawer-end-re* line))
                (setf in-preamble-drawer nil))
               ;; Preamble property lines
               ((and in-preamble-drawer
                     (multiple-value-bind (match regs)
                         (cl-ppcre:scan-to-strings *property-line-re* line)
                       (when (and match (string-equal "ID" (aref regs 0)))
                         (setf file-id (aref regs 1))
                         t))))
               ;; File keywords
               ((and (null current-headline)
                     (multiple-value-bind (match regs)
                         (cl-ppcre:scan-to-strings *file-keyword-re* line)
                       (when match
                         (let ((key (string-upcase (aref regs 0)))
                               (val (aref regs 1)))
                           (cond
                             ((string= key "TITLE") (setf title val))
                             ((string= key "FILETAGS")
                              (setf filetags (parse-headline-tags val)))))
                         t))))
               ;; Headlines
               ((multiple-value-bind (match regs)
                    (cl-ppcre:scan-to-strings *headline-re* line)
                  (when match
                    ;; Close previous headline
                    (when current-headline
                      (setf (headline-end-line current-headline) (1- i))
                      (push current-headline headlines))
                    (setf current-headline
                          (make-instance 'org-headline
                            :level (length (aref regs 0))
                            :todo (aref regs 1)
                            :priority (aref regs 2)
                            :title (aref regs 3)
                            :tags (parse-headline-tags (aref regs 4))
                            :start-line i))
                    t)))))
    ;; Close last headline
    (when current-headline
      (setf (headline-end-line current-headline) (1- (length lines)))
      (push current-headline headlines))
    ;; Extract ID links from full text
    (cl-ppcre:do-matches-as-strings (match *id-link-re* text)
      (multiple-value-bind (m regs)
          (cl-ppcre:scan-to-strings *id-link-re* match)
        (declare (ignore m))
        (push (list :type "id" :target (aref regs 0)
                    :description (aref regs 1))
              links)))
    (make-instance 'org-document
      :path (namestring path)
      :title (or title "")
      :file-id file-id
      :filetags filetags
      :headlines (nreverse headlines)
      :links (nreverse links)
      :raw-text text)))

;;; --- Conversion to plists ---

(defun headline-to-plist (h)
  "Convert an org-headline to a plist."
  (list :level (headline-level h)
        :todo (headline-todo h)
        :priority (headline-priority h)
        :title (headline-title h)
        :tags (headline-tags h)
        :properties (headline-properties h)
        :start-line (headline-start-line h)
        :end-line (headline-end-line h)))

(defun document-to-plist (doc &key include-raw)
  "Convert an org-document to a plist."
  (let ((result (list :path (doc-path doc)
                      :title (doc-title doc)
                      :file-id (doc-file-id doc)
                      :filetags (doc-filetags doc)
                      :headline-count (length (doc-headlines doc))
                      :headlines (mapcar #'headline-to-plist (doc-headlines doc))
                      :links (doc-links doc))))
    (when include-raw
      (setf (getf result :raw-text) (doc-raw-text doc)))
    result))

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

(defun %om-filter-by-allowed-paths (rows &key (file-key "file"))
  "Filter a list of row plists, keeping only those whose FILE-KEY value
   is under an allowed path."
  (remove-if-not (lambda (row)
                   (let ((file (getf row (intern (string-upcase file-key) :keyword))))
                     (and file (orgmode-path-allowed-p file))))
                 rows))

(defun %om-sql-rows-to-plists (stmt column-names)
  "Read all rows from a sqlite statement, returning a list of plists."
  (let (result)
    (loop for row = (sqlite:step-statement stmt)
          while row
          do (let (plist)
               (loop for i from (1- (length column-names)) downto 0
                     do (push (sqlite:statement-column-value stmt i) plist)
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
                     (sqlite:bind-parameter stmt 2 tag)
                     (sqlite:bind-parameter stmt 3 limit))
                   (sqlite:bind-parameter stmt 2 limit))
               (%om-filter-by-allowed-paths
                (%om-sql-rows-to-plists stmt cols)))
          (sqlite:finalize-statement stmt))))))

(defun org-roam-backlinks (node-id)
  "Return nodes that link TO this node-id.
   Results filtered by allowed paths."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT n.id, n.file, n.title, n.level
                 FROM links l
                 JOIN nodes n ON n.id = l.source
                 WHERE l.dest = ? AND l.type = 'id'")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 node-id)
             (%om-filter-by-allowed-paths
              (%om-sql-rows-to-plists stmt '("id" "file" "title" "level"))))
        (sqlite:finalize-statement stmt)))))

(defun org-roam-forward-links (node-id)
  "Return nodes that this node-id links TO.
   Results filtered by allowed paths."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT n.id, n.file, n.title, n.level
                 FROM links l
                 JOIN nodes n ON n.id = l.dest
                 WHERE l.source = ? AND l.type = 'id'")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 node-id)
             (%om-filter-by-allowed-paths
              (%om-sql-rows-to-plists stmt '("id" "file" "title" "level"))))
        (sqlite:finalize-statement stmt)))))

(defun org-roam-list-tags ()
  "Return all distinct tags from nodes under allowed paths."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT t.tag
                 FROM tags t
                 JOIN nodes n ON n.id = t.node_id
                 ORDER BY t.tag")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (let ((all-tags (%om-sql-rows-to-plists stmt '("tag"))))
             ;; Filter: only include tags from nodes under allowed paths
             ;; For efficiency, get the full node+tag list
             (let ((tag-names nil))
               (dolist (row all-tags)
                 (pushnew (getf row :tag) tag-names :test #'string-equal))
               ;; Return as simple list of strings
               (sort tag-names #'string<)))
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
             (sqlite:bind-parameter stmt 1 node-id)
             (let ((results (%om-filter-by-allowed-paths
                             (%om-sql-rows-to-plists
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
                               (%om-ensure-trailing-slash root)))))

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
;;; Public API
;;; ====================================================================

(defun orgmode-read (path-or-id &key include-raw)
  "Read and parse an org file.
   PATH-OR-ID can be a file path or an org-roam node ID (UUID).
   Returns parsed document as plist."
  (%om-validate-enabled)
  (let ((path (if (and (= (length path-or-id) 36)
                       (cl-ppcre:scan "^[0-9a-f-]+$" path-or-id))
                  ;; Looks like a UUID — resolve via org-roam DB
                  (let ((node (org-roam-node-by-id path-or-id)))
                    (unless node
                      (error "Node ~A not found in org-roam database" path-or-id))
                    (getf node :file))
                  path-or-id)))
    (let ((canonical (%om-validate-path path)))
      (document-to-plist (parse-org-file canonical) :include-raw include-raw))))

(defun orgmode-search (query &key (mode :auto) tag (limit 50))
  "Search for notes matching QUERY.
   MODE: :AUTO (default, uses DB then files), :DB (org-roam only), :FILES (scan only).
   TAG: filter by org-roam tag (DB modes only).
   Returns list of match plists."
  (%om-validate-enabled)
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
          (let ((dir (%om-canonical-dir root)))
            (when dir
              (dolist (file (directory (merge-pathnames "*.org" dir)))
                (when (<= remaining 0) (return))
                (let ((fpath (namestring file)))
                  (unless (gethash fpath seen-files)
                    (let ((preamble (parse-org-preamble file)))
                      (when (or (search query (getf preamble :title)
                                        :test #'char-equal)
                                (search query (namestring file)
                                        :test #'char-equal))
                        (push preamble file-results)
                        (decf remaining)))))))))))
    (append db-results (nreverse file-results))))

(defun orgmode-list-tags ()
  "List all tags from the org-roam database."
  (%om-validate-enabled)
  (if (org-roam-db-available-p)
      (org-roam-list-tags)
      (error "Org-roam database not available")))

(defun orgmode-backlinks (id-or-path &key (direction :backlinks) (limit 50))
  "Get links for a node.
   DIRECTION: :BACKLINKS (default), :FORWARD, :BOTH.
   ID-OR-PATH: org-roam node ID or file path."
  (%om-validate-enabled)
  (let ((node-id (if (and (= (length id-or-path) 36)
                          (cl-ppcre:scan "^[0-9a-f-]+$" id-or-path))
                     id-or-path
                     ;; Resolve file path to node ID via preamble parse
                     (let ((preamble (parse-org-preamble
                                      (%om-validate-path id-or-path))))
                       (or (getf preamble :file-id)
                           (error "File ~A has no :ID: property" id-or-path))))))
    (let ((back (when (member direction '(:backlinks :both))
                  (org-roam-backlinks node-id)))
          (fwd (when (member direction '(:forward :both))
                 (org-roam-forward-links node-id))))
      (let ((results (append back fwd)))
        (subseq results 0 (min limit (length results)))))))

(defun orgmode-create-note (title &key root body filetags id)
  "Create a new org-roam note file.
   ROOT defaults to the first allowed path.
   Generates a UUID for :ID: if not provided.
   Returns the path of the created file."
  (%om-validate-enabled)
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
    (%om-validate-new-path path)
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
  (%om-validate-enabled)
  (let ((path (if (and (= (length path-or-id) 36)
                       (cl-ppcre:scan "^[0-9a-f-]+$" path-or-id))
                  (let ((node (org-roam-node-by-id path-or-id)))
                    (unless node
                      (error "Node ~A not found in org-roam database" path-or-id))
                    (getf node :file))
                  path-or-id)))
    (let ((canonical (%om-validate-path path)))
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
  (%om-validate-enabled)
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
      (let ((dir (%om-canonical-dir r)))
        (when dir
          (dolist (file (directory (merge-pathnames "*.org" dir)))
            (when (>= count limit) (return))
            (push (parse-org-preamble file) results)
            (incf count)))))
    (nreverse results)))

(defun orgmode-status ()
  "Return orgmode skill status as a plist."
  (list :enabled (orgmode-enabled-p)
        :allowed-paths (orgmode-allowed-paths)
        :db-path (orgmode-db-path)
        :db-available (org-roam-db-available-p)))
