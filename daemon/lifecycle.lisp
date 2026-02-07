;;;; daemon/lifecycle.lisp
;;;;
;;;; Daemon start/stop/status with PID file management.

(in-package #:crichton/daemon)

(defvar *running* nil "T when the daemon event loop is active.")

(defun pid-file-path ()
  (merge-pathnames "daemon.pid"
                   (symbol-value (find-symbol "*AGENT-HOME*" :crichton/config))))

(defun write-pid-file ()
  (let ((path (namestring (pid-file-path))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "~D~%" (sb-posix:getpid)))
    #+sbcl (sb-posix:chmod path #o600)))

(defun read-pid-file ()
  (let ((path (pid-file-path)))
    (when (probe-file path)
      (with-open-file (s path)
        (parse-integer (read-line s) :junk-allowed t)))))

(defun remove-pid-file ()
  (let ((path (pid-file-path)))
    (when (probe-file path)
      (delete-file path))))

(defvar *shutdown-lock* (bt:make-lock "shutdown-lock"))
(defvar *shutdown-cv* (bt:make-condition-variable :name "shutdown-cv"))

(defun start-daemon (&key (foreground nil))
  "Start the Crichton daemon. Idempotent — returns NIL if already running.
   When FOREGROUND is T, blocks until stop-daemon is called (for systemd)."
  (when *running*
    (log:warn "Daemon already running.")
    (return-from start-daemon nil))
  (crichton/config:ensure-directories)
  (crichton/config:load-config)
  (crichton/logging:setup-logging)
  (write-pid-file)
  (setf *running* t)
  (start-swank)
  (log:info "Crichton daemon started (PID ~D)" (sb-posix:getpid))
  (when foreground
    (install-signal-handlers)
    (bt:with-lock-held (*shutdown-lock*)
      (loop while *running*
            do (bt:condition-wait *shutdown-cv* *shutdown-lock*)))
    (log:info "Crichton daemon exiting."))
  t)

(defun stop-daemon ()
  "Stop the Crichton daemon. Wakes the foreground loop if running."
  (unless *running*
    (log:warn "Daemon not running.")
    (return-from stop-daemon nil))
  (setf *running* nil)
  (bt:with-lock-held (*shutdown-lock*)
    (bt:condition-notify *shutdown-cv*))
  (remove-pid-file)
  (log:info "Crichton daemon stopped.")
  t)

(defun install-signal-handlers ()
  "Handle SIGTERM/SIGINT for clean shutdown under systemd."
  #+sbcl
  (flet ((shutdown-handler (sig info context)
           (declare (ignore sig info context))
           (stop-daemon)))
    (sb-sys:enable-interrupt sb-posix:sigterm #'shutdown-handler)
    (sb-sys:enable-interrupt sb-posix:sigint #'shutdown-handler)))

(defun daemon-status ()
  "Return daemon status as a plist."
  (let ((pid (read-pid-file)))
    (list :running *running*
          :pid (when *running* (sb-posix:getpid))
          :stale-pid (when (and pid (not *running*)) pid))))
