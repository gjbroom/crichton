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
                             (include-disk t))
  "Collect all system metrics into a single plist.
   Partial failures are recorded in :errors, not signalled."
  (let ((errors nil)
        loadavg memory thermals disks)
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
    (list :timestamp-unix (get-universal-time)
          :loadavg loadavg
          :memory memory
          :thermals thermals
          :disks disks
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

(defun system-report (&key (mounts '("/" "/home")) (stream *standard-output*))
  "Display a formatted system status report. Returns the snapshot plist."
  (let ((snap (system-snapshot :mounts mounts)))
    (format-loadavg (getf snap :loadavg) stream)
    (format-memory (getf snap :memory) stream)
    (format-thermal (getf snap :thermals) stream)
    (format-disk (getf snap :disks) stream)
    (when (eq (getf snap :status) :partial)
      (format stream "~&Errors:~%")
      (dolist (e (getf snap :errors))
        (format stream "  ~A~%" e)))
    snap))
