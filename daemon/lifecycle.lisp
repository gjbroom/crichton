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
  (crichton/config:delete-file-if-exists (pid-file-path)))

(defvar *shutdown-lock* (bt:make-lock "shutdown-lock"))
(defvar *shutdown-cv* (bt:make-condition-variable :name "shutdown-cv"))

(defmacro guarded (description &body body)
  "Execute BODY, logging a warning with DESCRIPTION if an error occurs.
  Continues execution after the warning."
  (let ((c (gensym "C")))
    `(handler-case
         (progn ,@body)
       (error (,c)
         (log:warn "~A: ~A" ,description ,c)))))

(defun init-storage ()
  "Initialize storage subsystems: credentials, sessions, KV cache, data store, meters, state."
  (crichton/credentials:ensure-credential-store)
  (guarded "State file initialization failed"
    (crichton/state:ensure-default-state-files))
  (guarded "Session purge at startup failed"
    (crichton/sessions:purge-expired-sessions))
  (guarded "KV cache preload at startup failed"
    (ensure-directories-exist
     (merge-pathnames "kv/" crichton/config:*agent-home*))
    (crichton/skills:preload-kv-cache))
  (guarded "Storage preload at startup failed"
    (ensure-directories-exist
     (merge-pathnames "data/" crichton/config:*agent-home*))
    (crichton/storage:preload-storage))
  (guarded "Meter restoration at startup failed"
    (crichton/skills:load-meters)))

(defun init-skills ()
  "Initialize skill subsystems: scheduler, tasks, discovery, battery monitoring."
  (crichton/skills:start-scheduler)
  (guarded "Task restoration at startup failed"
    (crichton/skills:restore-user-tasks))
  (guarded "RSS monitor restoration at startup failed"
    (crichton/skills:restore-rss-monitors))
  (guarded "Saved pipeline restoration at startup failed"
    (crichton/skills:restore-saved-pipelines))
  (guarded "Skill discovery at startup failed"
    (crichton/skills:discover-skills))
  (guarded "Battery monitoring restoration failed"
    (crichton/skills:restore-battery-monitoring))
  (guarded "Battery monitoring startup failed"
    (let ((interval (or (crichton/config:config-section-get :battery :interval) 300)))
      (crichton/skills:start-battery-monitoring :interval interval)))
  (guarded "System monitoring restoration failed"
    (crichton/skills:restore-system-monitoring)))

(defun init-network ()
  "Initialize network subsystems: RPC server, external channels."
  (guarded "RPC server startup failed"
    (start-rpc-server))
  (guarded "Channel startup failed"
    (crichton/channels:start-channels)))

(defun start-daemon (&key (foreground nil))
  "Start the Crichton daemon.  Idempotent — returns NIL if already running.
   When FOREGROUND is T, blocks until stop-daemon is called (for systemd)."
  (when *running*
    (log:warn "Daemon already running.")
    (return-from start-daemon nil))
  (crichton/config:ensure-directories)
  (crichton/config:load-config)
  (crichton/logging:setup-logging
   :level (crichton/config:config-section-get-keyword :logging :level :info))
  (write-pid-file)
  (setf *running* t)
  (start-swank)
  (init-storage)
  (init-skills)
  (init-network)
  (log:info "Crichton ~A daemon started (PID ~D)"
            crichton/config:*crichton-version* (sb-posix:getpid))
  (when foreground
    (install-signal-handlers)
    (bt:with-lock-held (*shutdown-lock*)
      (loop while *running*
            do (bt:condition-wait *shutdown-cv* *shutdown-lock*)))
    (log:info "Crichton daemon exiting."))
  t)

(defun persist-all-storage ()
  "Persist all the memory data structures back to permanent storage."
  (guarded "KV cache flush at shutdown failed"
    (crichton/skills:flush-all-kv))
  (guarded "Task persistence at shutdown failed"
    (crichton/skills:persist-user-tasks))
  (guarded "Meter persistence at shutdown failed"
    (crichton/skills:save-meters))
  (guarded "Storage flush at shutdown failed"
    (crichton/storage:flush-all-storage)))

(defun stop-daemon ()
  "Stop the Crichton daemon.  Wakes the foreground loop if running."
  (unless *running*
    (log:warn "Daemon not running.")
    (return-from stop-daemon nil))
  (guarded "Channel shutdown error"
    (crichton/channels:stop-channels))
  (guarded "RPC server shutdown error"
    (stop-rpc-server))
  (guarded "Battery monitoring shutdown error"
    (crichton/skills:stop-battery-monitoring))
  (guarded "System monitoring shutdown error"
    (crichton/skills:stop-system-monitoring))
  (persist-all-storage)
  (crichton/skills:stop-scheduler)
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
