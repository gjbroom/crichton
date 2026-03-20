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
   (scheduled :initarg :scheduled :initform nil :accessor headline-scheduled
              :documentation "SCHEDULED timestamp plist, or NIL.")
   (deadline :initarg :deadline :initform nil :accessor headline-deadline
             :documentation "DEADLINE timestamp plist, or NIL.")
   (closed :initarg :closed :initform nil :accessor headline-closed
           :documentation "CLOSED timestamp plist, or NIL.")
   (start-line :initarg :start-line :initform 0 :accessor headline-start-line)
   (end-line :initarg :end-line :initform 0 :accessor headline-end-line)))

(defclass org-document ()
  ((path :initarg :path :initform nil :accessor doc-path)
   (title :initarg :title :initform "" :accessor doc-title)
   (filetags :initarg :filetags :initform nil :accessor doc-filetags)
   (file-id :initarg :file-id :initform nil :accessor doc-file-id)
   (headlines :initarg :headlines :initform nil :accessor doc-headlines)
   (tables :initarg :tables :initform nil :accessor doc-tables)
   (src-blocks :initarg :src-blocks :initform nil :accessor doc-src-blocks)
   (links :initarg :links :initform nil :accessor doc-links)
   (raw-text :initarg :raw-text :initform nil :accessor doc-raw-text)))

(defclass org-table ()
  ((header :initarg :header :initform nil :accessor table-header
           :documentation "List of column name strings, or NIL if no hline separator.")
   (rows :initarg :rows :initform nil :accessor table-rows
         :documentation "List of row lists (each row is a list of cell strings).")
   (start-line :initarg :start-line :initform 0 :accessor table-start-line)
   (end-line :initarg :end-line :initform 0 :accessor table-end-line)
   (parent-headline :initarg :parent-headline :initform nil
                    :accessor table-parent-headline
                    :documentation "Title of the containing headline, or NIL.")))

(defclass org-src-block ()
  ((language :initarg :language :initform "" :accessor src-block-language)
   (body :initarg :body :initform "" :accessor src-block-body)
   (start-line :initarg :start-line :initform 0 :accessor src-block-start-line)
   (end-line :initarg :end-line :initform 0 :accessor src-block-end-line)
   (name :initarg :name :initform nil :accessor src-block-name
         :documentation "#+NAME: value if present before the block.")
   (parent-headline :initarg :parent-headline :initform nil
                    :accessor src-block-parent-headline
                    :documentation "Title of the containing headline, or NIL.")))

;;; --- Regex patterns ---

(defparameter *todo-keywords*
  '("TODO" "DONE" "NEXT" "WAITING" "CANCELLED" "SOMEDAY")
  "Recognized TODO keywords.  *headline-re* is derived from this list.")

(defparameter *headline-re*
  (cl-ppcre:create-scanner
   (format nil "^(\\*+)\\s+(?:(~A)\\s+)?(?:\\[#([A-C])\\]\\s+)?(.*?)(?:\\s+(:[\\w:]+:))?\\s*$"
           (format nil "~{~A~^|~}" *todo-keywords*)))
  "Match org headline: stars, optional TODO keyword, optional priority, title, optional tags.
Derived from *todo-keywords* — update that list to extend recognised keywords.")

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
  (cl-ppcre:create-scanner "^\\s*#\\+BEGIN_SRC\\s*(.*?)\\s*$" :case-insensitive-mode t))

(defparameter *src-block-end-re*
  (cl-ppcre:create-scanner "^\\s*#\\+END_SRC" :case-insensitive-mode t))

(defparameter *name-keyword-re*
  (cl-ppcre:create-scanner "^\\s*#\\+NAME:\\s*(.*?)\\s*$" :case-insensitive-mode t))

(defparameter *table-row-re*
  (cl-ppcre:create-scanner "^\\s*\\|(.+)\\|\\s*$"))

(defparameter *table-hline-re*
  (cl-ppcre:create-scanner "^\\s*\\|[-+]+\\|\\s*$"))

(defparameter *timestamp-re*
  (cl-ppcre:create-scanner
   "([<\\[])([0-9]{4})-([0-9]{2})-([0-9]{2})\\s+\\w+(?:\\s+([0-9]{1,2}:[0-9]{2}))?(?:\\s+([.+]+[0-9]+[hdwmy]))?([>\\]])")
  "Match an org timestamp: <2026-03-19 Wed 10:30 +1w> or [2026-03-19 Wed].")

;;; --- Timestamp parser ---

(defun parse-org-timestamp (ts-string)
  "Parse an org timestamp string into a plist.
   Returns (:type :ACTIVE/:INACTIVE :date \"YYYY-MM-DD\" :time \"HH:MM\" :repeater \"+1w\")
   or NIL if not a valid timestamp."
  (when (and ts-string (plusp (length ts-string)))
    (multiple-value-bind (match regs)
        (cl-ppcre:scan-to-strings *timestamp-re* ts-string)
      (when match
        (let ((type (if (string= (aref regs 0) "<") :active :inactive))
              (date (format nil "~A-~A-~A" (aref regs 1) (aref regs 2) (aref regs 3)))
              (time (aref regs 4))
              (repeater (aref regs 5)))
          (list :type type :date date :time time :repeater repeater))))))

(defun parse-planning-line (line)
  "Parse a planning line for SCHEDULED/DEADLINE/CLOSED timestamps.
   Returns (values scheduled deadline closed) as timestamp plists or NIL."
  (let ((scheduled nil) (deadline nil) (closed nil))
    ;; Extract each keyword individually for robustness
    (cl-ppcre:do-matches-as-strings
        (m "(?:SCHEDULED|DEADLINE|CLOSED):\\s*[<\\[][^>\\]]+[>\\]]" line)
      (cond
        ((cl-ppcre:scan "^SCHEDULED:" m)
         (let ((ts (cl-ppcre:scan-to-strings "[<\\[][^>\\]]+[>\\]]" m)))
           (when ts (setf scheduled (parse-org-timestamp ts)))))
        ((cl-ppcre:scan "^DEADLINE:" m)
         (let ((ts (cl-ppcre:scan-to-strings "[<\\[][^>\\]]+[>\\]]" m)))
           (when ts (setf deadline (parse-org-timestamp ts)))))
        ((cl-ppcre:scan "^CLOSED:" m)
         (let ((ts (cl-ppcre:scan-to-strings "[<\\[][^>\\]]+[>\\]]" m)))
           (when ts (setf closed (parse-org-timestamp ts)))))))
    (values scheduled deadline closed)))

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

;;; --- Table parser ---

(defun parse-table-row-cells (line)
  "Parse a table row line into a list of trimmed cell strings.
   Input: '| col1 | col2 | col3 |'  Output: (\"col1\" \"col2\" \"col3\")"
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings *table-row-re* line)
    (when match
      (mapcar (lambda (s) (string-trim " " s))
              (cl-ppcre:split "\\|" (aref regs 0))))))

(defun parse-table-at (lines start)
  "Parse a table starting at line index START in the LINES vector.
   Returns (values org-table end-index)."
  (let ((rows-before-hline nil)
        (rows-after-hline nil)
        (found-hline nil)
        (i start))
    (loop while (< i (length lines))
          for line = (aref lines i)
          do (cond
               ((cl-ppcre:scan *table-hline-re* line)
                (setf found-hline t))
               ((cl-ppcre:scan *table-row-re* line)
                (let ((cells (parse-table-row-cells line)))
                  (if found-hline
                      (push cells rows-after-hline)
                      (push cells rows-before-hline))))
               (t (return)))
          do (incf i))
    (let ((header (when found-hline (nreverse rows-before-hline)))
          (data-rows (if found-hline
                        (nreverse rows-after-hline)
                        (nreverse rows-before-hline))))
      ;; When header is a single row, flatten to just that row
      (values (make-instance 'org-table
                :header (when (and header (= (length header) 1))
                          (first header))
                :rows data-rows
                :start-line start
                :end-line (1- i))
              (1- i)))))

;;; --- Source block parser ---

(defun parse-src-block-header (header-text)
  "Parse the text after #+BEGIN_SRC into (values language remaining-text).
   E.g. 'lisp :var x=5' => (values \"lisp\" \":var x=5\")"
  (let ((trimmed (string-trim " " header-text)))
    (if (zerop (length trimmed))
        (values "" "")
        (let ((space-pos (position #\Space trimmed)))
          (if space-pos
              (values (subseq trimmed 0 space-pos)
                      (string-trim " " (subseq trimmed space-pos)))
              (values trimmed ""))))))

;;; --- Preamble-only parse (fast) ---

(defun parse-org-preamble (path)
  "Fast preamble-only parse. Returns plist:
   (:path PATH :title TITLE :file-id ID :filetags (tag ...) :setupfile SF)"
  (let ((title nil) (file-id nil) (filetags nil) (setupfile nil)
        (in-preamble-drawer nil))
    (with-open-file (s path :direction :input :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
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

(defun parse-org-file (path &key text)
  "Parse an org file into an org-document instance.
   TEXT may be supplied to avoid a second disk read when the caller
   already holds the file contents.  Returns the org-document."
  (let* ((text (or text (uiop:read-file-string path)))
         (lines (coerce (cl-ppcre:split "\\n" text) 'vector))
         (title nil) (file-id nil) (filetags nil)
         (headlines nil) (links nil) (tables nil) (src-blocks nil)
         (current-headline nil)
         (in-preamble-drawer nil)
         (pending-name nil))
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
               ;; #+NAME: keyword (captured for next src block)
               ((multiple-value-bind (match regs)
                    (cl-ppcre:scan-to-strings *name-keyword-re* line)
                  (when match
                    (setf pending-name (aref regs 0))
                    t)))
               ;; Source blocks
               ((multiple-value-bind (match regs)
                    (cl-ppcre:scan-to-strings *src-block-begin-re* line)
                  (when match
                    (let ((header-text (aref regs 0)))
                      (multiple-value-bind (language _rest)
                          (parse-src-block-header header-text)
                        (declare (ignore _rest))
                        (let ((body-lines nil)
                              (start-line i)
                              (j (1+ i)))
                          (loop while (< j (length lines))
                                for bline = (aref lines j)
                                do (if (cl-ppcre:scan *src-block-end-re* bline)
                                       (return)
                                       (push bline body-lines))
                                do (incf j))
                          (push (make-instance 'org-src-block
                                  :language language
                                  :body (format nil "~{~A~^~%~}" (nreverse body-lines))
                                  :start-line start-line
                                  :end-line j
                                  :name pending-name
                                  :parent-headline (when current-headline
                                                     (headline-title current-headline)))
                                src-blocks)
                          (setf pending-name nil)
                          (setf i j))))
                    t)))
               ;; Tables
               ((cl-ppcre:scan *table-row-re* line)
                (multiple-value-bind (tbl end-idx)
                    (parse-table-at lines i)
                  (setf (table-parent-headline tbl)
                        (when current-headline
                          (headline-title current-headline)))
                  (push tbl tables)
                  (setf i end-idx)))
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
               ;; Planning lines (SCHEDULED/DEADLINE/CLOSED after a headline)
               ((and current-headline
                     (cl-ppcre:scan "^\\s*(?:SCHEDULED|DEADLINE|CLOSED):" line))
                (multiple-value-bind (sched dead clos)
                    (parse-planning-line line)
                  (when sched (setf (headline-scheduled current-headline) sched))
                  (when dead (setf (headline-deadline current-headline) dead))
                  (when clos (setf (headline-closed current-headline) clos))))
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
                    (setf pending-name nil)
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
      :tables (nreverse tables)
      :src-blocks (nreverse src-blocks)
      :links (nreverse links)
      :raw-text text)))

;;; --- Conversion to plists ---

(defun headline-to-plist (h)
  "Convert an org-headline to a plist."
  (let ((result (list :level (headline-level h)
                      :todo (headline-todo h)
                      :priority (headline-priority h)
                      :title (headline-title h)
                      :tags (headline-tags h)
                      :properties (headline-properties h)
                      :start-line (headline-start-line h)
                      :end-line (headline-end-line h))))
    (when (headline-scheduled h)
      (setf (getf result :scheduled) (headline-scheduled h)))
    (when (headline-deadline h)
      (setf (getf result :deadline) (headline-deadline h)))
    (when (headline-closed h)
      (setf (getf result :closed) (headline-closed h)))
    result))

(defun table-to-plist (tbl)
  "Convert an org-table to a plist."
  (list :header (table-header tbl)
        :rows (table-rows tbl)
        :row-count (length (table-rows tbl))
        :start-line (table-start-line tbl)
        :end-line (table-end-line tbl)
        :parent-headline (table-parent-headline tbl)))

(defun src-block-to-plist (blk)
  "Convert an org-src-block to a plist."
  (list :language (src-block-language blk)
        :name (src-block-name blk)
        :body (src-block-body blk)
        :start-line (src-block-start-line blk)
        :end-line (src-block-end-line blk)
        :parent-headline (src-block-parent-headline blk)))

(defun document-to-plist (doc &key include-raw)
  "Convert an org-document to a plist."
  (let ((result (list :path (doc-path doc)
                      :title (doc-title doc)
                      :file-id (doc-file-id doc)
                      :filetags (doc-filetags doc)
                      :headline-count (length (doc-headlines doc))
                      :headlines (mapcar #'headline-to-plist (doc-headlines doc))
                      :links (doc-links doc))))
    (when (doc-tables doc)
      (setf (getf result :tables)
            (mapcar #'table-to-plist (doc-tables doc))))
    (when (doc-src-blocks doc)
      (setf (getf result :src-blocks)
            (mapcar #'src-block-to-plist (doc-src-blocks doc))))
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

(defun org-roam-backlinks (node-id &key (limit 50))
  "Return nodes that link TO this node-id.
   Results filtered by allowed paths, capped at LIMIT."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT n.id, n.file, n.title, n.level
                 FROM links l
                 JOIN nodes n ON n.id = l.source
                 WHERE l.dest = ? AND l.type = 'id'
                 LIMIT ?")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 node-id)
             (sqlite:bind-parameter stmt 2 limit)
             (%om-filter-by-allowed-paths
              (%om-sql-rows-to-plists stmt '("id" "file" "title" "level"))))
        (sqlite:finalize-statement stmt)))))

(defun org-roam-forward-links (node-id &key (limit 50))
  "Return nodes that this node-id links TO.
   Results filtered by allowed paths, capped at LIMIT."
  (with-org-roam-db (db)
    (let* ((sql "SELECT DISTINCT n.id, n.file, n.title, n.level
                 FROM links l
                 JOIN nodes n ON n.id = l.dest
                 WHERE l.source = ? AND l.type = 'id'
                 LIMIT ?")
           (stmt (sqlite:prepare-statement db sql)))
      (unwind-protect
           (progn
             (sqlite:bind-parameter stmt 1 node-id)
             (sqlite:bind-parameter stmt 2 limit)
             (%om-filter-by-allowed-paths
              (%om-sql-rows-to-plists stmt '("id" "file" "title" "level"))))
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
           (let* ((rows (%om-sql-rows-to-plists stmt '("tag" "file")))
                  (allowed (%om-filter-by-allowed-paths rows))
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
;;; Recursive file scanning
;;; ====================================================================

(defun %om-recursive-scan-p ()
  "Return T if recursive scanning is enabled (default: T)."
  (let ((val (crichton/config:config-section-get :orgmode :recursive-scan)))
    (cond
      ((null val) t)
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      ((and (stringp val) (string-equal val "false")) nil)
      (t t))))

(defun %om-max-scan-depth ()
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
      (let ((canonical (%om-canonical-dir root)))
        (when canonical
          (scan-dir (parse-namestring canonical) 0))))
    (nreverse results)))

(defun %om-collect-org-files (root)
  "Collect .org files under ROOT, respecting recursive_scan config."
  (if (%om-recursive-scan-p)
      (org-files-under root :max-depth (%om-max-scan-depth))
      (directory (merge-pathnames "*.org"
                                 (parse-namestring
                                  (%om-ensure-trailing-slash root))))))

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
          (dolist (file (%om-collect-org-files root))
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
      (dolist (file (%om-collect-org-files r))
        (when (>= count limit) (return))
        (push (parse-org-preamble file) results)
        (incf count)))
    (nreverse results)))

(defun orgmode-status ()
  "Return orgmode skill status as a plist."
  (list :enabled (orgmode-enabled-p)
        :allowed-paths (orgmode-allowed-paths)
        :db-path (orgmode-db-path)
        :db-available (org-roam-db-available-p)))

;;; ====================================================================
;;; TODO management
;;; ====================================================================

(defun %om-active-todo-keywords ()
  "Return active (non-done) TODO keywords from config or defaults."
  (or (let ((val (crichton/config:config-section-get :orgmode :todo-keywords)))
        (when (listp val) val))
      '("TODO" "NEXT" "WAITING" "SOMEDAY")))

(defun %om-done-keywords ()
  "Return done TODO keywords from config or defaults."
  (or (let ((val (crichton/config:config-section-get :orgmode :done-keywords)))
        (when (listp val) val))
      '("DONE" "CANCELLED")))

(defun %om-todo-from-db (state priority tag limit include-done)
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
              (let ((done (%om-done-keywords)))
                (push (format nil "n.todo NOT IN (~{?~^, ~})"
                              (make-list (length done) :initial-element nil))
                      conditions)
                (dolist (d done) (push d params))))
            (when state
              (push (format nil "n.todo = ?") conditions)
              (push state params))
            (when priority
              (push (format nil "n.priority = ?") conditions)
              (push priority params))
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
                       (sqlite:bind-parameter stmt (incf param-idx) tag))
                     (sqlite:bind-parameter stmt (incf param-idx) limit)
                     (%om-filter-by-allowed-paths
                      (%om-sql-rows-to-plists
                       stmt '("id" "file" "title" "level" "todo" "priority"))))
                (sqlite:finalize-statement stmt)))))
      (error (c)
        (log:warn "Org-roam TODO query failed: ~A" c)
        nil))))

(defun %om-scan-file-todos (path active-kws done-kws state priority include-done)
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

(defun %om-todo-from-files (file state priority include-done limit existing-count)
  "Scan org files for TODO headlines. Returns list of plists."
  (let ((results nil)
        (remaining (- limit existing-count))
        (active-kws (%om-active-todo-keywords))
        (done-kws (%om-done-keywords)))
    (flet ((scan-file (path)
             (when (<= remaining 0) (return-from scan-file))
             (let ((todos (%om-scan-file-todos
                           path active-kws done-kws
                           state priority include-done)))
               (dolist (todo todos)
                 (when (<= remaining 0) (return))
                 (push todo results)
                 (decf remaining)))))
      (if file
          ;; Single file mode
          (let ((canonical (%om-validate-path file)))
            (scan-file canonical))
          ;; Scan all allowed paths
          (dolist (root (orgmode-allowed-paths))
            (when (<= remaining 0) (return))
            (dolist (f (%om-collect-org-files root))
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
  (%om-validate-enabled)
  (let ((db-results (unless file
                      (%om-todo-from-db state priority tag limit include-done))))
    (if (and db-results (>= (length db-results) limit))
        db-results
        ;; Supplement with file scanning when DB results are insufficient
        (let ((file-results (%om-todo-from-files
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

(defun %om-format-timestamp ()
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
                       (member new-state (%om-done-keywords) :test #'string-equal)
                       (not (headline-closed target)))
              (let ((closed-line (format nil "    CLOSED: [~A]"
                                        (%om-format-timestamp)))
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
