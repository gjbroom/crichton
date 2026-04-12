;;;; skills/builtins/orgmode.lisp
;;;;
;;;; Built-in skill: Org-mode file operations and org-roam integration.
;;;; Config gate, path policy, org parser, and data structures.
;;;; The skill is split across three files:
;;;;   orgmode.lisp      — config gate, path policy, org parser (this file)
;;;;   orgmode-roam.lisp — SQLite layer, writer helpers, file scanner
;;;;   orgmode-api.lisp  — public API and TODO management
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

(defun om-ensure-trailing-slash (path)
  "Ensure PATH ends with a slash."
  (let ((s (namestring path)))
    (if (char= (char s (1- (length s))) #\/)
        s
        (concatenate 'string s "/"))))

(defun om-canonical-dir (path)
  "Return a canonical directory namestring for PATH, or NIL if it doesn't exist."
  (handler-case
      (let ((truepath (truename (parse-namestring path))))
        (om-ensure-trailing-slash truepath))
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
        (let ((canonical-root (om-canonical-dir root)))
          (when (and canonical-root
                     (>= (length canonical) (length canonical-root))
                     (string= canonical canonical-root
                              :end1 (length canonical-root)))
            (return canonical-root)))))))

(defun om-validate-enabled ()
  "Check that orgmode is enabled. Signals error if not."
  (unless (orgmode-enabled-p)
    (error "Org-mode skill is disabled (set [orgmode] enable = true in config.toml)")))

(defun om-validate-path (path)
  "Validate PATH is allowed. Returns canonical path string. Signals error if denied."
  (om-validate-enabled)
  (let ((canonical (handler-case
                       (namestring (truename path))
                     (error ()
                       (error "Path does not exist: ~A" path)))))
    (unless (orgmode-path-allowed-p canonical)
      (error "Path ~A is not under any allowed orgmode root" canonical))
    canonical))

(defun om-validate-new-path (path)
  "Validate a path for a new file (parent must exist and be allowed).
   Returns the path string. Signals error if denied."
  (om-validate-enabled)
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
