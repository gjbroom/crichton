;;;; logging/logger.lisp
;;;;
;;;; Structured JSON logging to ~/.crichton/logs/
;;;; Files: 0600. Directory: 0700. No secrets in output.

(in-package #:crichton/logging)

;;; --- Redaction ---

(defvar *redaction-patterns*
  '(("(?i)bearer\\s+[A-Za-z0-9._\\-]+" . "Bearer [REDACTED]")
    ("sk-[A-Za-z0-9]{20,}" . "sk-[REDACTED]")
    ("(?i)(api[_-]?key|secret|password|token)([\"']?\\s*[:=]\\s*[\"']?)([^\"'\\s,}]+)"
     . "\\1\\2[REDACTED]"))
  "Alist of (regex . replacement) for scrubbing secrets from log output.")

(defun redact-string (string)
  "Apply all *redaction-patterns* to STRING, returning a cleaned copy."
  (let ((s string))
    (dolist (p *redaction-patterns* s)
      (setf s (cl-ppcre:regex-replace-all (car p) s (cdr p))))))

(defmacro with-redaction ((&rest patterns) &body body)
  "Execute BODY with additional redaction patterns active."
  `(let ((*redaction-patterns* (append (list ,@patterns) *redaction-patterns*)))
     ,@body))

;;; --- Timestamp ---

(defun iso8601-now ()
  "Return current UTC time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

;;; --- JSON Lines Appender ---

(defvar *json-appender* nil
  "The active JSON lines appender instance, for idempotent setup.")

(defvar *json-log-stream* nil
  "Open file stream for JSON log output.")

(defclass json-lines-appender (log4cl:appender)
  ((log-path :initarg :log-path :accessor appender-log-path))
  (:documentation "log4cl appender that writes one JSON object per line."))

(defun make-log-entry (level-name category message)
  "Build a hash-table representing one JSON log line."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "timestamp" ht) (iso8601-now)
          (gethash "level" ht) level-name
          (gethash "logger" ht) (or category "")
          (gethash "message" ht) message)
    ht))

(defmethod log4cl:appender-do-append ((appender json-lines-appender)
                                       logger
                                       level
                                       log-func)
  (let* ((level-name (log4cl:log-level-to-string level))
         (category (log4cl:logger-category logger))
         (message-raw (with-output-to-string (s)
                        (funcall log-func s)))
         (message (redact-string message-raw))
         (entry (make-log-entry level-name category message)))
    (when *json-log-stream*
      (handler-case
          (let ((*print-pretty* nil))
            (shasht:write-json entry *json-log-stream*)
            (terpri *json-log-stream*)
            (finish-output *json-log-stream*))
        (error (c)
          (declare (ignore c))
          nil)))))

(defun open-log-file (path)
  "Open a log file for appending with 0600 permissions."
  (let ((stream (open path
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :create
                      :element-type 'character
                      :external-format :utf-8)))
    #+sbcl (sb-posix:chmod (namestring (truename path)) #o600)
    stream))

(defun close-log-file ()
  "Close the current JSON log stream if open."
  (when (and *json-log-stream* (open-stream-p *json-log-stream*))
    (close *json-log-stream*))
  (setf *json-log-stream* nil))

;;; --- Setup ---

(defun setup-logging (&key (level :info) (log-dir nil))
  "Initialize JSON lines logging to LOG-DIR (default: ~/.crichton/logs/).
   Idempotent — removes previous appender on re-invocation."
  (let* ((dir (or log-dir
                  (merge-pathnames #p"logs/"
                                   (symbol-value
                                    (find-symbol "*AGENT-HOME*" :crichton/config)))))
         (log-path (merge-pathnames "daemon.jsonl" dir)))
    (ensure-directories-exist dir)
    #+sbcl (sb-posix:chmod (namestring dir) #o700)
    (when *json-appender*
      (log4cl:remove-appender log4cl:*root-logger* *json-appender*)
      (close-log-file))
    (close-log-file)
    (setf *json-log-stream* (open-log-file log-path))
    (let ((appender (make-instance 'json-lines-appender :log-path log-path)))
      (setf *json-appender* appender)
      (log4cl:add-appender log4cl:*root-logger* appender))
    (log:config level)
    (log:info "Logging initialized: ~A" (namestring log-path))))
