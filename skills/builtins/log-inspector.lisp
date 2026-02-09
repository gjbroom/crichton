;;;; skills/builtins/log-inspector.lisp
;;;;
;;;; Built-in skill: Inspect the daemon's own JSONL log files.
;;;; Provides tail, search, summary, and formatted report interfaces.
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.
;;;; Logs are already sanitized by the redaction system in logging/logger.lisp.

(in-package #:crichton/skills)

;;; --- Log file path ---

(defun log-file-path ()
  "Return the path to the daemon.jsonl log file."
  (merge-pathnames "daemon.jsonl"
                   (crichton/config:agent-path "logs")))

;;; --- Reading and parsing ---

(defun parse-log-line (line)
  "Parse a single JSONL line into a hash-table. Returns NIL on failure."
  (handler-case
      (let ((ht (shasht:read-json (make-string-input-stream line))))
        (when (hash-table-p ht) ht))
    (error () nil)))

(defun read-log-lines ()
  "Read all lines from the log file. Returns a list of strings, or NIL."
  (let ((path (log-file-path)))
    (when (probe-file path)
      (handler-case
          (with-open-file (s path :direction :input :if-does-not-exist nil)
            (when s
              (loop for line = (read-line s nil nil)
                    while line
                    when (plusp (length line))
                      collect line)))
        (error () nil)))))

(defun read-log-tail (&key (count 50))
  "Read the last COUNT lines from the log file.
   Returns a list of parsed hash-tables (one per JSON line).
   Lines that fail to parse are skipped."
  (let* ((lines (read-log-lines))
         (tail (last lines count)))
    (loop for line in tail
          for ht = (parse-log-line line)
          when ht collect ht)))

;;; --- Searching ---

(defun entry-matches-p (entry &key pattern level)
  "Return T if ENTRY matches the given PATTERN and/or LEVEL filter."
  (and (or (null level)
           (string-equal level (gethash "level" entry "")))
       (or (null pattern)
           (search (string-downcase pattern)
                   (string-downcase (gethash "message" entry ""))))))

(defun search-log (&key pattern level (count 50))
  "Search log entries. PATTERN is a substring match against the message field
   (case-insensitive). LEVEL filters by log level. Both can be combined.
   Returns the last COUNT matching entries."
  (let* ((lines (read-log-lines))
         (matches nil))
    (dolist (line lines)
      (let ((ht (parse-log-line line)))
        (when (and ht (entry-matches-p ht :pattern pattern :level level))
          (push ht matches))))
    (let ((reversed (nreverse matches)))
      (last reversed count))))

;;; --- Summary ---

(defun log-summary (&key (count 100))
  "Return a plist summarizing the last COUNT log entries.
   Keys: :total, :by-level, :errors, :warnings, :time-range."
  (let* ((entries (read-log-tail :count count))
         (total (length entries))
         (level-counts (make-hash-table :test #'equal))
         (errors nil)
         (warnings nil))
    (dolist (entry entries)
      (let ((level (gethash "level" entry "UNKNOWN")))
        (incf (gethash level level-counts 0))
        (when (string-equal level "ERROR")
          (push entry errors))
        (when (string-equal level "WARN")
          (push entry warnings))))
    (let ((by-level nil))
      (maphash (lambda (k v) (push (cons k v) by-level)) level-counts)
      (setf by-level (sort by-level #'string< :key #'car))
      (list :total total
            :by-level by-level
            :errors (last (nreverse errors) 5)
            :warnings (last (nreverse warnings) 5)
            :time-range (when (plusp total)
                          (list :from (gethash "timestamp" (first entries) "")
                                :to (gethash "timestamp" (car (last entries)) "")))))))

;;; --- Formatted report ---

(defun format-log-entry-brief (entry &optional (stream *standard-output*))
  "Format a single log entry as a brief one-liner."
  (format stream "  [~A] [~A] ~A~%"
          (gethash "timestamp" entry "?")
          (gethash "level" entry "?")
          (gethash "message" entry "")))

(defun log-report (&key (stream *standard-output*) (count 100))
  "Display a human-readable log summary report. Returns the summary plist."
  (let ((summary (log-summary :count count)))
    (format stream "~&Log Summary (last ~D entries):~%" (getf summary :total))
    (format stream "~&Levels:~%")
    (dolist (pair (getf summary :by-level))
      (format stream "  ~A: ~D~%" (car pair) (cdr pair)))
    (let ((time-range (getf summary :time-range)))
      (when time-range
        (format stream "~&Time Range:~%")
        (format stream "  From: ~A~%" (getf time-range :from))
        (format stream "  To:   ~A~%" (getf time-range :to))))
    (let ((errors (getf summary :errors)))
      (when errors
        (format stream "~&Recent Errors (~D):~%" (length errors))
        (dolist (e errors)
          (format-log-entry-brief e stream))))
    (let ((warnings (getf summary :warnings)))
      (when warnings
        (format stream "~&Recent Warnings (~D):~%" (length warnings))
        (dolist (w warnings)
          (format-log-entry-brief w stream))))
    summary))
