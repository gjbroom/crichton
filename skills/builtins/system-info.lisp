;;;; skills/builtins/system-info.lisp
;;;;
;;;; Built-in skill: Linux system introspection via /proc and /sys.
;;;; Provides load average, memory, thermal, and disk usage metrics
;;;; for daemon throttling decisions.
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.
;;;; All reads are direct from procfs/sysfs — no shell invocations.

(in-package #:crichton/skills)

;;; --- CFFI for statvfs (sb-posix lacks it) ---

(cffi:defcstruct statvfs
  (f-bsize :unsigned-long)
  (f-frsize :unsigned-long)
  (f-blocks :unsigned-long)
  (f-bfree :unsigned-long)
  (f-bavail :unsigned-long)
  (f-files :unsigned-long)
  (f-ffree :unsigned-long)
  (f-favail :unsigned-long)
  (f-fsid :unsigned-long)
  (f-flag :unsigned-long)
  (f-namemax :unsigned-long))

(cffi:defcfun ("statvfs" %statvfs) :int
  (path :string)
  (buf :pointer))

;;; --- Helpers ---

(defun read-proc-file (path)
  "Read the entire contents of a /proc or /sys file. Returns NIL on failure."
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil)
        (when s
          (let ((contents (make-string-output-stream)))
            (loop for line = (read-line s nil nil)
                  while line
                  do (write-line line contents))
            (get-output-stream-string contents))))
    (error () nil)))

(defun read-proc-line (path)
  "Read the first line of a /proc or /sys file. Returns NIL on failure."
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil)
        (when s (read-line s nil nil)))
    (error () nil)))

(defun parse-integer-safe (string &key (start 0) (end nil))
  "Parse an integer from STRING, returning NIL on failure."
  (when string
    (parse-integer string :start start :end end :junk-allowed t)))

(defun parse-float-safe (string)
  "Parse a float from STRING, returning NIL on failure."
  (when (and string (plusp (length string)))
    (handler-case
        (let ((*read-default-float-format* 'double-float))
          (let ((val (read-from-string string)))
            (when (numberp val) (coerce val 'double-float))))
      (error () nil))))

(defun safe-ratio (used total)
  "Compute USED/TOTAL as a float, returning 0.0 if TOTAL is zero or nil."
  (if (and total used (plusp total))
      (coerce (/ used total) 'double-float)
      0.0d0))

;;; --- Load average ---

(defun cpu-count ()
  "Count CPUs by reading /proc/stat. Returns integer or NIL."
  (let ((content (read-proc-file "/proc/stat")))
    (when content
      (let ((count 0))
        (with-input-from-string (s content)
          (loop for line = (read-line s nil nil)
                while line
                when (and (> (length line) 3)
                          (string= line "cpu" :end1 3)
                          (digit-char-p (char line 3)))
                  do (incf count)))
        (when (plusp count) count)))))

(defun system-loadavg ()
  "Read /proc/loadavg. Returns a plist with load averages and task counts."
  (let ((line (read-proc-line "/proc/loadavg")))
    (unless line
      (return-from system-loadavg nil))
    (let* ((parts (cl-ppcre:split "\\s+" line))
           (load1 (parse-float-safe (first parts)))
           (load5 (parse-float-safe (second parts)))
           (load15 (parse-float-safe (third parts)))
           (tasks-str (fourth parts))
           (slash-pos (when tasks-str (position #\/ tasks-str)))
           (runnable (when slash-pos
                       (parse-integer-safe tasks-str :end slash-pos)))
           (total-tasks (when slash-pos
                          (parse-integer-safe tasks-str :start (1+ slash-pos))))
           (last-pid (parse-integer-safe (fifth parts)))
           (cpus (cpu-count)))
      (list :load1 load1
            :load5 load5
            :load15 load15
            :runnable runnable
            :tasks total-tasks
            :last-pid last-pid
            :cpu-count cpus
            :load1-per-cpu (when (and load1 cpus) (/ load1 cpus))))))

;;; --- Memory ---

(defun parse-meminfo-line (line)
  "Parse a /proc/meminfo line like 'MemTotal:  32135816 kB'.
   Returns (values keyword bytes) or NIL."
  (let ((colon (position #\: line)))
    (when colon
      (let* ((key-str (subseq line 0 colon))
             (rest (string-trim '(#\Space #\Tab) (subseq line (1+ colon))))
             (space (position #\Space rest))
             (val-str (if space (subseq rest 0 space) rest))
             (val (parse-integer-safe val-str)))
        (when val
          (let ((is-kb (search "kB" rest :start2 (or space 0))))
            (values (intern (string-upcase
                             (substitute #\- #\_ key-str))
                            :keyword)
                    (if is-kb (* val 1024) val))))))))

(defun system-memory ()
  "Read /proc/meminfo. Returns a plist with memory statistics in bytes."
  (let ((content (read-proc-file "/proc/meminfo")))
    (unless content
      (return-from system-memory nil))
    (let (mem-total mem-free mem-available buffers cached
          swap-total swap-free)
      (with-input-from-string (s content)
        (loop for line = (read-line s nil nil)
              while line
              do (multiple-value-bind (key val) (parse-meminfo-line line)
                   (case key
                     (:MEMTOTAL (setf mem-total val))
                     (:MEMFREE (setf mem-free val))
                     (:MEMAVAILABLE (setf mem-available val))
                     (:BUFFERS (setf buffers val))
                     (:CACHED (setf cached val))
                     (:SWAPTOTAL (setf swap-total val))
                     (:SWAPFREE (setf swap-free val))))))
      (let* ((available (or mem-available
                            (when (and mem-free buffers cached)
                              (+ mem-free buffers cached))))
             (used (when (and mem-total available)
                     (- mem-total available)))
             (swap-used (when (and swap-total swap-free)
                          (- swap-total swap-free))))
        (list :mem-total-bytes mem-total
              :mem-available-bytes available
              :mem-free-bytes mem-free
              :buffers-bytes buffers
              :cached-bytes cached
              :mem-used-bytes used
              :mem-used-ratio (safe-ratio used mem-total)
              :swap-total-bytes swap-total
              :swap-free-bytes swap-free
              :swap-used-bytes swap-used
              :swap-used-ratio (safe-ratio swap-used swap-total))))))

;;; --- Thermal zones ---

(defun system-thermal-zones ()
  "Read /sys/class/thermal/thermal_zone*/temp. Returns a list of plists,
   or NIL if no zones exist (VMs, containers)."
  (let ((zones nil)
        (base "/sys/class/thermal/"))
    (handler-case
        (let ((entries (directory (merge-pathnames "thermal_zone*/" base))))
          (dolist (entry entries)
            (let* ((name (car (last (pathname-directory entry))))
                   (temp-str (read-proc-line
                              (format nil "~A~A/temp" base name)))
                   (type-str (read-proc-line
                              (format nil "~A~A/type" base name)))
                   (temp-mc (parse-integer-safe temp-str)))
              (when temp-mc
                (push (list :zone name
                            :type (when type-str (string-trim '(#\Space #\Newline) type-str))
                            :temp-mc temp-mc
                            :temp-c (/ temp-mc 1000.0d0)
                            :path (format nil "~A~A" base name))
                      zones)))))
      (error () nil))
    (sort zones #'string< :key (lambda (z) (getf z :zone)))))

;;; --- Disk usage ---

(defun statvfs-usage (mount-path)
  "Call statvfs(2) on MOUNT-PATH. Returns a plist or NIL on failure."
  (cffi:with-foreign-object (buf '(:struct statvfs))
    (let ((rc (%statvfs mount-path buf)))
      (when (zerop rc)
        (let* ((frsize (cffi:foreign-slot-value buf '(:struct statvfs) 'f-frsize))
               (blocks (cffi:foreign-slot-value buf '(:struct statvfs) 'f-blocks))
               (bfree (cffi:foreign-slot-value buf '(:struct statvfs) 'f-bfree))
               (bavail (cffi:foreign-slot-value buf '(:struct statvfs) 'f-bavail))
               (total (* blocks frsize))
               (free-bytes (* bfree frsize))
               (avail (* bavail frsize))
               (used (- total free-bytes)))
          (list :mount mount-path
                :total-bytes total
                :free-bytes free-bytes
                :avail-bytes avail
                :used-bytes used
                :used-ratio (safe-ratio used total)))))))

(defun system-disk-usage (&key (mounts '("/" "/home")))
  "Get disk usage for each mount in MOUNTS via statvfs(2).
   Returns a list of plists. Skips mounts that fail."
  (loop for m in mounts
        for info = (handler-case (statvfs-usage m)
                     (error () nil))
        when info collect info))

;;; --- Snapshot ---

(defun system-snapshot (&key (mounts '("/" "/home"))
                             (include-load t)
                             (include-memory t)
                             (include-thermal t)
                             (include-disk t)
                             (include-battery t))
  "Collect all system metrics into a single plist.
   Partial failures are recorded in :errors, not signalled."
  (let ((errors nil)
        loadavg memory thermals disks batteries)
    (when include-load
      (handler-case (setf loadavg (system-loadavg))
        (error (c) (push (format nil "loadavg: ~A" c) errors))))
    (when include-memory
      (handler-case (setf memory (system-memory))
        (error (c) (push (format nil "memory: ~A" c) errors))))
    (when include-thermal
      (handler-case (setf thermals (system-thermal-zones))
        (error (c) (push (format nil "thermal: ~A" c) errors))))
    (when include-disk
      (handler-case (setf disks (system-disk-usage :mounts mounts))
        (error (c) (push (format nil "disk: ~A" c) errors))))
    (when include-battery
      (handler-case (setf batteries (all-batteries-snapshot))
        (error (c) (push (format nil "battery: ~A" c) errors))))
    (list :timestamp-unix (get-universal-time)
          :loadavg loadavg
          :memory memory
          :thermals thermals
          :disks disks
          :batteries batteries
          :status (if errors :partial :ok)
          :errors (nreverse errors))))

;;; --- Formatted output ---

(defun format-bytes (bytes)
  "Format BYTES as a human-readable string (GiB/MiB/KiB)."
  (cond
    ((null bytes) "?")
    ((>= bytes (* 1024 1024 1024))
     (format nil "~,1F GiB" (/ bytes (* 1024.0d0 1024 1024))))
    ((>= bytes (* 1024 1024))
     (format nil "~,1F MiB" (/ bytes (* 1024.0d0 1024))))
    ((>= bytes 1024)
     (format nil "~,1F KiB" (/ bytes 1024.0d0)))
    (t (format nil "~D B" bytes))))

(defun format-loadavg (load &optional (stream *standard-output*))
  (when load
    (format stream "~&Load Average:~%")
    (format stream "  1m: ~,2F  5m: ~,2F  15m: ~,2F~%"
            (or (getf load :load1) 0)
            (or (getf load :load5) 0)
            (or (getf load :load15) 0))
    (let ((cpus (getf load :cpu-count)))
      (when cpus
        (format stream "  CPUs: ~D  Load/CPU: ~,2F~%"
                cpus (or (getf load :load1-per-cpu) 0))))))

(defun format-memory (mem &optional (stream *standard-output*))
  (when mem
    (format stream "~&Memory:~%")
    (format stream "  Total: ~A  Available: ~A  Used: ~A (~,1F%)~%"
            (format-bytes (getf mem :mem-total-bytes))
            (format-bytes (getf mem :mem-available-bytes))
            (format-bytes (getf mem :mem-used-bytes))
            (* 100 (or (getf mem :mem-used-ratio) 0)))
    (when (and (getf mem :swap-total-bytes)
               (plusp (or (getf mem :swap-total-bytes) 0)))
      (format stream "  Swap: ~A total, ~A used (~,1F%)~%"
              (format-bytes (getf mem :swap-total-bytes))
              (format-bytes (getf mem :swap-used-bytes))
              (* 100 (or (getf mem :swap-used-ratio) 0))))))

(defun format-thermal (zones &optional (stream *standard-output*))
  (when zones
    (format stream "~&Thermal:~%")
    (dolist (z zones)
      (format stream "  ~A (~A): ~,1F°C~%"
              (getf z :zone)
              (or (getf z :type) "unknown")
              (or (getf z :temp-c) 0)))))

(defun format-disk (disks &optional (stream *standard-output*))
  (when disks
    (format stream "~&Disk:~%")
    (dolist (d disks)
      (format stream "  ~A: ~A used / ~A total (~,1F%)~%"
              (getf d :mount)
              (format-bytes (getf d :used-bytes))
              (format-bytes (getf d :total-bytes))
              (* 100 (or (getf d :used-ratio) 0))))))

(defun battery-display-name (name)
  "Extract a short display name from a battery NAME (pathname or string)."
  (typecase name
    (pathname (car (last (pathname-directory name))))
    (string name)
    (t (format nil "~A" name))))

(defun format-battery-section (batteries &optional (stream *standard-output*))
  (when batteries
    (format stream "~&Battery:~%")
    (dolist (bat batteries)
      (let ((name (battery-display-name (getf bat :name)))
            (capacity (getf bat :capacity))
            (status (getf bat :status))
            (power-now (getf bat :power-now))
            (time-to-empty (getf bat :time-to-empty))
            (time-to-full (getf bat :time-to-full)))
        (format stream "  ~A: ~A% (~A)~%" name (or capacity "?") (or status "?"))
        (when power-now
          (format stream "    Power: ~,2F W~%" (/ power-now 1000000.0)))
        (when time-to-empty
          (format stream "    Time to empty: ~A~%" (format-duration-seconds time-to-empty)))
        (when time-to-full
          (format stream "    Time to full: ~A~%" (format-duration-seconds time-to-full)))))))

(defun system-report (&key (mounts '("/" "/home")) (stream *standard-output*))
  "Display a formatted system status report. Returns the snapshot plist."
  (let ((snap (system-snapshot :mounts mounts)))
    (format-loadavg (getf snap :loadavg) stream)
    (format-memory (getf snap :memory) stream)
    (format-thermal (getf snap :thermals) stream)
    (format-disk (getf snap :disks) stream)
    (format-battery-section (getf snap :batteries) stream)
    (when (eq (getf snap :status) :partial)
      (format stream "~&Errors:~%")
      (dolist (e (getf snap :errors))
        (format stream "  ~A~%" e)))
    snap))

;;; --- Continuous monitoring ---

(defparameter *system-monitor-default-mem-alert-percent* 90
  "Default memory usage threshold for proactive alerts (percent used).")

(defparameter *system-monitor-default-cpu-alert-load* 2.0d0
  "Default per-CPU load average threshold for proactive alerts.
   E.g. 2.0 means alert when 1-minute load / CPU-count >= 2.0.")

(defparameter *system-monitor-default-disk-alert-percent* 90
  "Default disk usage threshold for proactive alerts (percent used).")

(defparameter *system-monitor-default-temp-alert-celsius* 85.0d0
  "Default thermal zone temperature threshold for proactive alerts (degrees C).")

(defvar *system-monitoring-task-name* "system-monitor"
  "Scheduler task name for periodic system monitoring.")

(defvar *system-monitor-active-alerts* (make-hash-table)
  "Hash table tracking which metric conditions are currently alerting.
   Keys are keywords (:memory, :cpu, :disk-/, :temp-thermal_zone0, ...).
   Value is T while the condition persists (prevents duplicate alerts).")

(defvar *system-monitor-active-alerts-lock* (bt:make-lock "system-monitor-alerts-lock")
  "Lock protecting *system-monitor-active-alerts*.")

(defun system-monitor-config ()
  "Return the effective system monitoring configuration as a plist.
   Values come from config.toml [system] section, falling back to defaults."
  (list :mem-alert-percent
        (or (crichton/config:config-section-get :system :mem-alert-percent)
            *system-monitor-default-mem-alert-percent*)
        :cpu-alert-load
        (or (crichton/config:config-section-get :system :cpu-alert-load)
            *system-monitor-default-cpu-alert-load*)
        :disk-alert-percent
        (or (crichton/config:config-section-get :system :disk-alert-percent)
            *system-monitor-default-disk-alert-percent*)
        :temp-alert-celsius
        (or (crichton/config:config-section-get :system :temp-alert-celsius)
            *system-monitor-default-temp-alert-celsius*)
        :interval
        (or (crichton/config:config-section-get :system :interval) 300)))

(defun %system-alert-new-p (key currently-over)
  "Return T if CURRENTLY-OVER and the alert for KEY was not already active.
   Updates the stored alert state. Used to prevent duplicate notifications."
  (bt:with-lock-held (*system-monitor-active-alerts-lock*)
    (let ((was-active (gethash key *system-monitor-active-alerts*)))
      (setf (gethash key *system-monitor-active-alerts*) currently-over)
      (and currently-over (not was-active)))))

(defun %clear-all-system-alerts ()
  "Clear all system monitoring alert states (call on stop/restart)."
  (bt:with-lock-held (*system-monitor-active-alerts-lock*)
    (clrhash *system-monitor-active-alerts*)))

(defun %system-check-memory (memory config)
  "Check memory against the threshold. Returns a list of alert strings, or NIL."
  (let ((ratio (getf memory :mem-used-ratio))
        (alert-pct (getf config :mem-alert-percent)))
    (when (and ratio alert-pct)
      (let ((over-p (>= ratio (/ (coerce alert-pct 'double-float) 100.0d0))))
        (when (%system-alert-new-p :memory over-p)
          (list (format nil "Memory usage at ~,1F% (threshold: ~A%)"
                        (* 100 ratio) alert-pct)))))))

(defun %system-check-cpu (loadavg config)
  "Check CPU load-per-CPU against the threshold. Returns a list of alert strings, or NIL."
  (let ((load-per-cpu (getf loadavg :load1-per-cpu))
        (alert-load (coerce (or (getf config :cpu-alert-load)
                                *system-monitor-default-cpu-alert-load*)
                            'double-float)))
    (when load-per-cpu
      (let ((over-p (>= load-per-cpu alert-load)))
        (when (%system-alert-new-p :cpu over-p)
          (list (format nil "CPU load/CPU at ~,2F (threshold: ~,2F); 1m avg: ~,2F on ~A core~:P"
                        load-per-cpu alert-load
                        (or (getf loadavg :load1) 0)
                        (or (getf loadavg :cpu-count) 1))))))))

(defun %system-check-disks (disks config)
  "Check disk usage. Returns a list of alert strings, or NIL."
  (let ((alert-pct (getf config :disk-alert-percent))
        alerts)
    (when alert-pct
      (dolist (disk disks)
        (let* ((ratio (getf disk :used-ratio))
               (mount (getf disk :mount))
               ;; Key: :DISK-/ or :DISK-/home etc.
               (key (intern (string-upcase (format nil "DISK-~A" mount)) :keyword))
               (over-p (and ratio (>= ratio (/ (coerce alert-pct 'double-float) 100.0d0)))))
          (when (and ratio (%system-alert-new-p key over-p))
            (push (format nil "Disk ~A at ~,1F% full (threshold: ~A%)"
                          mount (* 100 ratio) alert-pct)
                  alerts)))))
    (nreverse alerts)))

(defun %system-check-thermal (thermals config)
  "Check thermal zones. Returns a list of alert strings, or NIL."
  (let ((alert-c (coerce (or (getf config :temp-alert-celsius)
                             *system-monitor-default-temp-alert-celsius*)
                         'double-float))
        alerts)
    (dolist (zone thermals)
      (let* ((temp (getf zone :temp-c))
             (zone-name (getf zone :zone))
             (zone-type (or (getf zone :type) "unknown"))
             (key (intern (string-upcase (format nil "TEMP-~A" zone-name)) :keyword))
             (over-p (and temp (>= temp alert-c))))
        (when (and temp (%system-alert-new-p key over-p))
          (push (format nil "Thermal zone ~A (~A) at ~,1F°C (threshold: ~,1F°C)"
                        zone-name zone-type temp alert-c)
                alerts))))
    (nreverse alerts)))

(defun system-monitor-callback ()
  "Periodic callback for system monitoring.
   Collects a system snapshot and posts notifications when thresholds are first crossed."
  (handler-case
      (let* ((config (system-monitor-config))
             (snap (system-snapshot :mounts '("/" "/home") :include-battery nil))
             (alerts nil))
        (when (getf snap :memory)
          (setf alerts (nconc alerts (%system-check-memory (getf snap :memory) config))))
        (when (getf snap :loadavg)
          (setf alerts (nconc alerts (%system-check-cpu (getf snap :loadavg) config))))
        (when (getf snap :disks)
          (setf alerts (nconc alerts (%system-check-disks (getf snap :disks) config))))
        (when (getf snap :thermals)
          (setf alerts (nconc alerts (%system-check-thermal (getf snap :thermals) config))))
        (dolist (msg alerts)
          (log:warn "System monitor: ~A" msg)
          (crichton/daemon:notification-post "system" msg "system-monitor")))
    (error (c)
      (log:error "System monitor callback failed: ~A" c))))

(defun persist-system-monitoring-state (enabled interval)
  "Save system monitoring state to encrypted storage so it survives restarts."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "enabled" ht) enabled
          (gethash "interval" ht) interval)
    (crichton/storage:store-set "system" "monitoring" ht))
  (log:debug "Persisted system monitoring state: enabled=~A interval=~D" enabled interval))

(defun restore-system-monitoring ()
  "Restore system monitoring from persistent state.
   Call after start-scheduler during daemon init."
  (let ((data (crichton/storage:store-get "system" "monitoring")))
    (when (and data (hash-table-p data))
      (let ((enabled (gethash "enabled" data))
            (interval (gethash "interval" data)))
        (cond
          ((not enabled)
           (log:info "System monitoring disabled by saved state"))
          (t
           (start-system-monitoring :interval (or interval 300) :persist nil)
           (log:info "Restored system monitoring (interval: ~Ds)" (or interval 300))))))))

(defun start-system-monitoring (&key (interval 300) (persist t))
  "Start periodic system monitoring. INTERVAL is in seconds (default: 5 minutes).
   When PERSIST is T (default), saves the state to storage so it survives restarts.
   Returns T if monitoring started, NIL if already running."
  (when (find *system-monitoring-task-name*
              (list-tasks)
              :key (lambda (task) (getf task :name))
              :test #'string=)
    (log:info "System monitor already running.")
    (return-from start-system-monitoring nil))
  (schedule-every *system-monitoring-task-name* interval #'system-monitor-callback)
  (when persist
    (persist-system-monitoring-state t interval))
  (log:info "System monitoring started (interval: ~Ds)" interval)
  t)

(defun stop-system-monitoring (&key (persist t))
  "Stop periodic system monitoring. Returns T if stopped, NIL if not running.
   When PERSIST is T (default), saves the disabled state to storage."
  (when (cancel-task *system-monitoring-task-name*)
    (log:info "System monitoring stopped.")
    (%clear-all-system-alerts)
    (when persist
      (persist-system-monitoring-state nil 300))
    t))
