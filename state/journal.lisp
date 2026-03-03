;;;; state/journal.lisp
;;;;
;;;; Daily journal files and memory append operations.
;;;;
;;;; Journal files are append-only daily logs stored as org-mode files
;;;; in ~/.crichton/state/journal/YYYY-MM-DD.org.  They are never
;;;; auto-injected into the system prompt — they are searchable
;;;; on-demand via the memory_search agent tool.
;;;;
;;;; MEMORY.org is the curated long-term memory file.  The agent appends
;;;; to it when it learns durable facts (user preferences, project
;;;; context, lessons learned).

(in-package #:crichton/state)

;;; --- Character budget for context estimation ---

(defparameter *chars-per-token* 4
  "Rough estimate: ~4 characters per token for English text.
Used for context window budget estimation, not billing.")

;;; --- Journal directory ---

(defun journal-dir ()
  "Return the journal directory, creating it if needed."
  (let ((dir (merge-pathnames #p"journal/" (state-dir))))
    (ensure-directories-exist dir)
    #+sbcl (sb-posix:chmod (namestring dir) #o700)
    dir))

;;; --- Date helpers ---

(defun %today-date-string ()
  "Return today's date as YYYY-MM-DD in local time."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec min hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun %date-string-to-filename (date-string)
  "Convert a YYYY-MM-DD date string to a journal filename."
  (format nil "~A.org" date-string))

;;; --- Journal paths ---

(defun journal-today-path ()
  "Return the full path for today's journal file."
  (merge-pathnames (%date-string-to-filename (%today-date-string))
                   (journal-dir)))

(defun journal-path-for-date (date-string)
  "Return the full path for the journal file for DATE-STRING (YYYY-MM-DD)."
  (merge-pathnames (%date-string-to-filename date-string)
                   (journal-dir)))

;;; --- Journal append ---

(defun journal-append (content)
  "Append CONTENT to today's journal file.
Adds a timestamped entry in org-mode format.  Creates the file with
a #+TITLE header if it doesn't exist yet.  Returns the journal file path."
  (let* ((path (journal-today-path))
         (exists (probe-file path))
         (timestamp (crichton/config:iso8601-now)))
    (with-open-file (s path :direction :output
                            :if-exists :append
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (unless exists
        (format s "#+TITLE: Journal — ~A~%~%" (%today-date-string)))
      (format s "** ~A~%~A~%~%" timestamp content))
    #+sbcl (sb-posix:chmod (namestring (truename path)) #o600)
    (log:debug "Journal append: ~A (~D chars)" path (length content))
    path))

;;; --- Memory append ---

(defun append-to-memory (content)
  "Append CONTENT to MEMORY.org.
Adds content under a timestamped sub-heading.  Creates the file with
a #+TITLE header if it doesn't exist yet.  Returns the file path."
  (let* ((path (state-file-path "MEMORY.org"))
         (exists (probe-file path))
         (timestamp (crichton/config:iso8601-now)))
    (with-open-file (s path :direction :output
                            :if-exists :append
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (unless exists
        (format s "#+TITLE: Long-Term Memory~%~%"))
      (format s "** ~A~%~A~%~%" timestamp content))
    #+sbcl (sb-posix:chmod (namestring (truename path)) #o600)
    (log:info "Memory append: ~D chars" (length content))
    path))

;;; --- Journal search ---

(defun %journal-files-in-range (days-back)
  "Return a list of (date-string . path) pairs for journal files
within the last DAYS-BACK days, most recent first."
  (let* ((now (get-universal-time))
         (cutoff (- now (* days-back 24 60 60)))
         (dir (journal-dir))
         (results nil))
    (dolist (path (directory (merge-pathnames "*.org" dir)))
      (let* ((name (pathname-name path))
             (len (length name)))
        ;; Validate YYYY-MM-DD format (10 chars)
        (when (= len 10)
          (let ((file-date (ignore-errors
                            (encode-universal-time
                             0 0 0
                             (parse-integer name :start 8 :end 10)
                             (parse-integer name :start 5 :end 7)
                             (parse-integer name :start 0 :end 4)
                             0))))
            (when (and file-date (>= file-date cutoff))
              (push (cons name path) results))))))
    (sort results #'string> :key #'car)))

(defun %search-file-for-query (path query)
  "Search PATH for lines containing QUERY (case-insensitive).
Returns a list of matching context strings (the matched line plus
surrounding heading context)."
  (let ((matches nil)
        (current-heading "")
        (query-down (string-downcase query)))
    (with-open-file (s path :direction :input
                            :external-format :utf-8
                            :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   ;; Track current org heading
                   (when (and (plusp (length trimmed))
                              (char= (char trimmed 0) #\*))
                     (setf current-heading trimmed))
                   ;; Check for query match
                   (when (search query-down (string-downcase line))
                     (push (format nil "~A~%  ~A" current-heading line)
                           matches))))))
    (nreverse matches)))

(defun journal-search (query &key (days-back 7) (max-results 50))
  "Search journal files for QUERY within the last DAYS-BACK days.
Returns a formatted string with matching excerpts, limited to MAX-RESULTS.
Case-insensitive substring matching."
  (let ((files (%journal-files-in-range days-back))
        (total-matches 0)
        (output (make-string-output-stream)))
    (when (null files)
      (return-from journal-search
        (format nil "No journal entries found in the last ~D days." days-back)))
    (dolist (file-pair files)
      (when (>= total-matches max-results)
        (format output "~%... (truncated at ~D results)~%" max-results)
        (return))
      (let ((matches (%search-file-for-query (cdr file-pair) query)))
        (when matches
          (format output "~%--- ~A ---~%" (car file-pair))
          (dolist (match matches)
            (when (>= total-matches max-results)
              (return))
            (format output "~A~%" match)
            (incf total-matches)))))
    (if (zerop total-matches)
        (format nil "No matches for \"~A\" in the last ~D days." query days-back)
        (get-output-stream-string output))))

;;; --- Pre-compaction journal flush ---

(defun %extract-message-text (message)
  "Extract text content from a MESSAGE plist.
Content may be a string or a list of content blocks."
  (let ((content (getf message :content)))
    (typecase content
      (string content)
      (list
       (with-output-to-string (s)
         (dolist (block content)
           (when (and (listp block) (eq (getf block :type) :text))
             (write-string (or (getf block :text) "") s)
             (terpri s)))))
      (t ""))))

(defun estimate-messages-tokens (messages)
  "Estimate the total token count for MESSAGES using character count heuristic."
  (let ((total-chars 0))
    (dolist (msg messages)
      (incf total-chars (length (%extract-message-text msg))))
    (ceiling total-chars *chars-per-token*)))

(defun flush-session-to-journal (messages &key (max-chars 8000))
  "Flush the current session's message history to today's journal.
Extracts user and assistant messages and writes a summary to the journal
as a pre-compaction snapshot.  Truncates to MAX-CHARS to avoid huge
journal entries.

Call this before pruning the message history to preserve session knowledge.
Returns the journal file path, or NIL if there was nothing to flush."
  (let ((output (make-string-output-stream))
        (msg-count 0)
        (char-count 0))
    (format output "Pre-compaction session snapshot:~%")
    (dolist (msg messages)
      (let* ((role (getf msg :role))
             (text (%extract-message-text msg))
             (trimmed (string-trim '(#\Space #\Tab #\Newline) text)))
        (when (and (plusp (length trimmed))
                   (member role '(:user :assistant)))
          (let ((prefix (if (eq role :user) "User" "Crichton")))
            (let ((entry (format nil "- ~A: ~A~%"
                                 prefix
                                 (if (> (length trimmed) 500)
                                     (format nil "~A..." (subseq trimmed 0 497))
                                     trimmed))))
              (when (> (+ char-count (length entry)) max-chars)
                (format output "- ... (truncated)~%")
                (return))
              (write-string entry output)
              (incf char-count (length entry))
              (incf msg-count))))))
    (when (zerop msg-count)
      (return-from flush-session-to-journal nil))
    (log:info "Flushing ~D messages to journal (pre-compaction)" msg-count)
    (journal-append (get-output-stream-string output))))
