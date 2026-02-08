;;;; skills/builtins/battery.lisp
;;;;
;;;; Built-in skill: Battery monitoring and proactive low-battery alerts.
;;;; Reads battery state from /sys/class/power_supply/ on Linux.
;;;; Provides both programmatic (plist) and human-readable interfaces.
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.
;;;; All reads are direct from sysfs — no shell invocations.

(in-package #:crichton/skills)

;;; --- Constants and configuration ---

(defparameter *battery-power-supply-path* "/sys/class/power_supply/"
  "Base path for Linux power supply sysfs interface.")

(defparameter *battery-default-thresholds* '(20 10 5)
  "Default battery percentage thresholds for proactive alerts.")

(defvar *battery-last-alert-level* nil
  "Last battery level at which an alert was sent. Used to prevent spam.")

(defvar *battery-monitoring-task-name* "battery-monitor"
  "Scheduler task name for periodic battery monitoring.")

;;; --- Helpers for reading sysfs ---

(defun read-sysfs-file (path)
  "Read and trim the contents of a sysfs file. Returns NIL on failure."
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil)
        (when s
          (let ((line (read-line s nil nil)))
            (when line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))))
    (error () nil)))

;;; --- Battery detection and enumeration ---

(defun battery-path (battery-name)
  "Construct the full sysfs path for a battery named BATTERY-NAME."
  (merge-pathnames battery-name (pathname *battery-power-supply-path*)))

(defun battery-type-file (battery-name)
  "Return the path to the type file for BATTERY-NAME."
  (merge-pathnames "type" (battery-path battery-name)))

(defun battery-device-p (battery-name)
  "Return T if BATTERY-NAME appears to be a battery device (not AC adapter)."
  (let* ((type-file (battery-type-file battery-name))
         (type-str (read-sysfs-file (namestring type-file))))
    (and type-str (string-equal type-str "Battery"))))

(defun list-batteries ()
  "List all battery devices found in /sys/class/power_supply/.
   Returns a list of battery device names (strings), or NIL if none found."
  (let ((base-path (pathname *battery-power-supply-path*)))
    (when (probe-file base-path)
      (handler-case
          (let ((entries (uiop:directory-files base-path)))
            (loop for entry in entries
                  for name = (car (last (pathname-directory entry)))
                  when (and name (battery-device-p name))
                    collect name))
        (error () nil)))))

(defun has-battery-p ()
  "Return T if the system has at least one battery."
  (not (null (list-batteries))))

;;; --- Reading battery attributes ---

(defun battery-attribute (battery-name attribute)
  "Read a battery attribute from sysfs. Returns the trimmed string or NIL."
  (let ((attr-path (merge-pathnames attribute (battery-path battery-name))))
    (read-sysfs-file (namestring attr-path))))

(defun battery-capacity (battery-name)
  "Return battery capacity as percentage (integer 0-100), or NIL."
  (let ((val (battery-attribute battery-name "capacity")))
    (when val (parse-integer val :junk-allowed t))))

(defun battery-status (battery-name)
  "Return battery status string (Charging, Discharging, Full, etc.), or NIL."
  (battery-attribute battery-name "status"))

(defun battery-energy-now (battery-name)
  "Return current energy in microjoules (integer), or NIL."
  (let ((val (battery-attribute battery-name "energy_now")))
    (when val (parse-integer val :junk-allowed t))))

(defun battery-energy-full (battery-name)
  "Return full energy in microjoules (integer), or NIL."
  (let ((val (battery-attribute battery-name "energy_full")))
    (when val (parse-integer val :junk-allowed t))))

(defun battery-power-now (battery-name)
  "Return current power draw in microwatts (integer), or NIL."
  (let ((val (battery-attribute battery-name "power_now")))
    (when val (parse-integer val :junk-allowed t))))

(defun battery-time-to-empty (battery-name)
  "Estimate time to empty in seconds. Returns NIL if charging or data unavailable."
  (let ((status (battery-status battery-name))
        (energy-now (battery-energy-now battery-name))
        (power-now (battery-power-now battery-name)))
    (when (and status
               (string-equal status "Discharging")
               energy-now
               power-now
               (plusp power-now))
      ;; energy_now (µJ) / power_now (µW) = seconds
      (truncate energy-now power-now))))

(defun battery-time-to-full (battery-name)
  "Estimate time to full in seconds. Returns NIL if discharging or data unavailable."
  (let ((status (battery-status battery-name))
        (energy-now (battery-energy-now battery-name))
        (energy-full (battery-energy-full battery-name))
        (power-now (battery-power-now battery-name)))
    (when (and status
               (string-equal status "Charging")
               energy-now
               energy-full
               power-now
               (plusp power-now))
      ;; (energy_full - energy_now) / power_now = seconds
      (truncate (- energy-full energy-now) power-now))))

;;; --- Battery snapshot (plist representation) ---

(defun battery-snapshot (battery-name)
  "Return a plist with battery state for BATTERY-NAME.
   Keys: :name, :capacity, :status, :energy-now, :energy-full, :power-now,
         :time-to-empty, :time-to-full, :present."
  (let ((capacity (battery-capacity battery-name))
        (status (battery-status battery-name))
        (energy-now (battery-energy-now battery-name))
        (energy-full (battery-energy-full battery-name))
        (power-now (battery-power-now battery-name))
        (time-to-empty (battery-time-to-empty battery-name))
        (time-to-full (battery-time-to-full battery-name)))
    (list :name battery-name
          :capacity capacity
          :status status
          :energy-now energy-now
          :energy-full energy-full
          :power-now power-now
          :time-to-empty time-to-empty
          :time-to-full time-to-full
          :present (and capacity status))))

(defun all-batteries-snapshot ()
  "Return a list of battery snapshot plists, one per battery."
  (mapcar #'battery-snapshot (list-batteries)))

;;; --- Formatted output ---

(defun format-duration-seconds (seconds)
  "Format SECONDS as a human-readable duration (e.g., '2h 15m')."
  (when (and seconds (integerp seconds) (plusp seconds))
    (let* ((hours (truncate seconds 3600))
           (minutes (truncate (mod seconds 3600) 60)))
      (cond
        ((plusp hours) (format nil "~Dh ~Dm" hours minutes))
        ((plusp minutes) (format nil "~Dm" minutes))
        (t (format nil "<1m"))))))

(defun format-battery (snapshot &optional (stream *standard-output*))
  "Format a battery snapshot plist as a human-readable report."
  (let ((name (getf snapshot :name))
        (capacity (getf snapshot :capacity))
        (status (getf snapshot :status))
        (power-now (getf snapshot :power-now))
        (time-to-empty (getf snapshot :time-to-empty))
        (time-to-full (getf snapshot :time-to-full)))
    (format stream "~&Battery: ~A~%" (or name "Unknown"))
    (format stream "  Charge: ~A%~%" (or capacity "?"))
    (format stream "  Status: ~A~%" (or status "?"))
    (when power-now
      (format stream "  Power: ~,2F W~%" (/ power-now 1000000.0)))
    (when time-to-empty
      (format stream "  Time to empty: ~A~%" (format-duration-seconds time-to-empty)))
    (when time-to-full
      (format stream "  Time to full: ~A~%" (format-duration-seconds time-to-full)))))

;;; --- Public interface: battery-status (plist) ---

(defun battery-status-plist ()
  "Return battery status as a plist. Suitable for programmatic use.
   Keys: :has-battery, :batteries (list of battery snapshot plists)."
  (let ((batteries (all-batteries-snapshot)))
    (list :has-battery (not (null batteries))
          :batteries batteries)))

;;; --- Public interface: battery-report (human-readable) ---

(defun battery-report (&optional (stream *standard-output*))
  "Display a human-readable battery report. Returns the plist."
  (let ((status (battery-status-plist)))
    (if (getf status :has-battery)
        (let ((batteries (getf status :batteries)))
          (format stream "~&System has ~D batter~:@P:~%" (length batteries))
          (dolist (bat batteries)
            (format-battery bat stream)))
        (format stream "~&No battery detected.~%"))
    status))

;;; --- Proactive monitoring and alerts ---

(defun battery-thresholds ()
  "Return the list of battery alert thresholds from config, or defaults."
  (let ((config-thresholds (crichton/config:config-section-get :battery :thresholds)))
    (cond
      ;; Config provides a list
      ((and config-thresholds (listp config-thresholds))
       (remove-if-not #'numberp config-thresholds))
      ;; Config provides a single number
      ((numberp config-thresholds)
       (list config-thresholds))
      ;; Use defaults
      (t *battery-default-thresholds*))))

(defun battery-check-thresholds ()
  "Check all batteries against configured thresholds.
   Returns a plist (:alert-needed T/NIL, :batteries (list), :lowest-capacity N).
   If alert is needed, includes :threshold-crossed N."
  (let ((batteries (all-batteries-snapshot))
        (thresholds (sort (copy-list (battery-thresholds)) #'>))
        (lowest-capacity nil)
        (threshold-crossed nil))
    (unless batteries
      (return-from battery-check-thresholds
        (list :alert-needed nil :batteries nil :lowest-capacity nil)))
    ;; Find the lowest capacity across all batteries
    (dolist (bat batteries)
      (let ((cap (getf bat :capacity)))
        (when (and cap (or (null lowest-capacity) (< cap lowest-capacity)))
          (setf lowest-capacity cap))))
    ;; Determine if we should alert
    (when lowest-capacity
      (dolist (threshold thresholds)
        (when (<= lowest-capacity threshold)
          (setf threshold-crossed threshold)
          (return))))
    ;; Only alert if we crossed a NEW threshold (prevent spam)
    (let ((alert-needed (and threshold-crossed
                             (or (null *battery-last-alert-level*)
                                 (< lowest-capacity *battery-last-alert-level*)))))
      (when alert-needed
        (setf *battery-last-alert-level* lowest-capacity))
      (list :alert-needed alert-needed
            :batteries batteries
            :lowest-capacity lowest-capacity
            :threshold-crossed threshold-crossed))))

(defun battery-alert-message (check-result)
  "Generate a human-readable alert message from a threshold check result."
  (let ((threshold (getf check-result :threshold-crossed))
        (capacity (getf check-result :lowest-capacity))
        (batteries (getf check-result :batteries)))
    (with-output-to-string (s)
      (format s "BATTERY ALERT: Battery at ~A%~%" capacity)
      (format s "Threshold crossed: ~A%~%" threshold)
      (format s "~%")
      (dolist (bat batteries)
        (format-battery bat s)))))

(defun battery-monitor-callback ()
  "Periodic callback for battery monitoring. Logs alerts when thresholds crossed."
  (handler-case
      (let ((check (battery-check-thresholds)))
        (when (getf check :alert-needed)
          (let ((msg (battery-alert-message check)))
            (log:warn "~A" msg))))
    (error (c)
      (log:error "Battery monitor callback failed: ~A" c))))

(defun start-battery-monitoring (&key (interval 300))
  "Start periodic battery monitoring. INTERVAL is in seconds (default: 5 minutes).
   Returns T if monitoring started, NIL if already running or no battery present."
  (unless (has-battery-p)
    (log:info "No battery detected; skipping battery monitoring.")
    (return-from start-battery-monitoring nil))
  ;; Cancel existing monitor if present
  (when (find *battery-monitoring-task-name*
              (list-tasks)
              :key (lambda (task) (getf task :name))
              :test #'string=)
    (log:info "Battery monitor already running.")
    (return-from start-battery-monitoring nil))
  ;; Schedule periodic check
  (schedule-every *battery-monitoring-task-name* interval #'battery-monitor-callback)
  (log:info "Battery monitoring started (interval: ~Ds, thresholds: ~S)"
            interval (battery-thresholds))
  t)

(defun stop-battery-monitoring ()
  "Stop periodic battery monitoring. Returns T if stopped, NIL if not running."
  (when (cancel-task *battery-monitoring-task-name*)
    (log:info "Battery monitoring stopped.")
    (setf *battery-last-alert-level* nil)
    t))
