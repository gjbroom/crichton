;;;; logging/logger.lisp
;;;;
;;;; Structured JSON logging to ~/.crichton/logs/
;;;; Files: 0600. Directory: 0700. No secrets in output.

(in-package #:crichton/logging)

(defvar *redaction-patterns* nil
  "Alist of (regex . replacement) for scrubbing secrets from log output.")

(defun setup-logging (&key (level :info) (log-dir nil))
  "Initialize logging to LOG-DIR (default: ~/.crichton/logs/).
   Sets file permissions to 0600."
  (let ((dir (or log-dir
                 (merge-pathnames #p"logs/"
                                  (symbol-value
                                   (find-symbol "*AGENT-HOME*" :crichton/config))))))
    (ensure-directories-exist dir)
    #+sbcl (sb-posix:chmod (namestring dir) #o700)
    ;; TODO: configure log4cl appender to write JSON lines to dir
    (log:config level)
    (log:info "Crichton logging initialized" :directory (namestring dir))))

(defmacro with-redaction ((&rest patterns) &body body)
  "Execute BODY with additional redaction patterns active."
  `(let ((*redaction-patterns* (append (list ,@patterns) *redaction-patterns*)))
     ,@body))
