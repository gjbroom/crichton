;;;; skills/builtins/battery.lisp
;;;;
;;;; Built-in skill: Battery level monitoring with proactive alerts.
;;;; Detects batteries via /sys/class/power_supply, monitors charge level,
;;;; charging status, and sends proactive notifications at configurable
;;;; thresholds.
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.
;;;; Linux-only (reads /sys/class/power_supply/).

(in-package #:crichton/skills)

;;; --- Configuration ---

(defparameter *battery-default-thresholds* '(20 10 5)
  "Default battery percentage thresholds for proactive alerts.")

(defparameter *battery-check-interval* 300
  "Default interval in seconds for battery checks (5 minutes).")

(defvar *battery-last-alert-level* nil
  "Tracks the last alert level sent to avoid spamming.")

;;; --- Battery detection and data reading ---

(defun find-batteries ()
  "Find all battery devices in /sys/class/power_supply/.
   Returns a list of battery names (e.g. \"BAT0\" \"BAT1\")."
  (let ((base "/sys/class/power_supply/"))
    (handler-case
        (let ((entries (directory (merge-pathnames "*/" base)))
              (batteries nil))
          (dolist (entry entries)
            (let* ((name (car (last (pathname-directory entry))))
                   (type-path (format nil "~A~A/type" base name))
                   (type-str (read-proc-line type-path)))
              (when (and type-str
                         (string-equal (string-trim '(#\Space #\Newline) type-str)
                                       "Battery"))
                (push name batteries))))
          (nreverse batteries))
      (error () nil))))

(defun read-battery-capacity (battery-name)
  "Read the capacity percentage for BATTERY-NAME.
   Returns an integer 0-100 or NIL on failure."
  (let ((path (format nil "/sys/class/power_supply/~A/capacity" battery-name)))
    (let ((line (read-proc-line path)))
      (when line
        (parse-integer-safe (string-trim '(#\Space #\Newline) line))))))

(defun read-battery-status (battery-name)
  "Read the charging status for BATTERY-NAME.
   Returns a keyword: :charging, :discharging, :full, :not-charging, or :unknown."
  (let ((path (format nil "/sys/class/power_supply/~A/status" battery-name)))
    (let ((line (read-proc-line path)))
      (if line
          (let ((status (string-trim '(#\Space #\Newline) line)))
            (cond
              ((string-equal status "Charging") :charging)
              ((string-equal status "Discharging") :discharging)
              ((string-equal status "Full") :full)
              ((string-equal status "Not charging") :not-charging)
              (t :unknown)))
          :unknown))))

(defun read-battery-energy (battery-name)
  "Read energy information for BATTERY-NAME.
   Returns a plist with :energy-now, :energy-full, :power-now (in microwatts/microjoules)
   or NIL if not available."
  (let ((base (format nil "/sys/class/power_supply/~A/" battery-name)))
    (flet ((read-value (filename)
             (parse-integer-safe (read-proc-line (concatenate 'string base filename)))))
      (let ((energy-now (read-value "energy_now"))
            (energy-full (read-value "energy_full"))
            (power-now (read-value "power_now")))
        (when (or energy-now energy-full power-now)
          (list :energy-now energy-now
                :energy-full energy-full
                :power-now power-now))))))

(defun estimate-time-remaining (battery-info)
  "Estimate time remaining in seconds based on battery energy and power draw.
   Returns NIL if estimation is not possible."
  (let ((energy-now (getf battery-info :energy-now))
        (power-now (getf battery-info :power-now))
        (status (getf battery-info :status)))
    (when (and energy-now power-now (plusp power-now)
               (eq status :discharging))
      ;; energy-now is in microjoules, power-now is in microwatts
      ;; time = energy / power (in seconds)
      (truncate (/ energy-now power-now)))))

;;; --- Battery status retrieval ---

(defun battery-info (battery-name)
  "Get detailed information for a single battery.
   Returns a plist with battery details or NIL if battery doesn't exist."
  (let ((capacity (read-battery-capacity battery-name))
        (status (read-battery-status battery-name))
        (energy (read-battery-energy battery-name)))
    (when capacity
      (let ((info (list :name battery-name
                        :capacity capacity
                        :status status)))
        (when energy
          (setf info (append info energy))
          (let ((time-remaining (estimate-time-remaining
                                 (append info (list :status status)))))
            (when time-remaining
              (setf info (append info (list :time-remaining-seconds time-remaining))))))
        info))))

(defun all-batteries-info ()
  "Get information for all detected batteries.
   Returns a list of battery info plists."
  (let ((batteries (find-batteries)))
    (loop for bat in batteries
          for info = (battery-info bat)
          when info collect info)))

(defun primary-battery-info ()
  "Get info for the primary battery (first one found).
   Returns a plist or NIL if no batteries exist."
  (let ((batteries (find-batteries)))
    (when batteries
      (battery-info (first batteries)))))

;;; --- Alert logic ---

(defun get-battery-thresholds ()
  "Get battery alert thresholds from config, falling back to defaults."
  (let ((configured (crichton/config:config-section-get :battery :thresholds)))
    (if (and configured (listp configured))
        (sort (copy-list configured) #'>)
        *battery-default-thresholds*)))

(defun should-alert-p (capacity)
  "Determine if we should send an alert for CAPACITY percentage.
   Returns the threshold level if alert needed, NIL otherwise."
  (let ((thresholds (get-battery-thresholds)))
    (loop for threshold in thresholds
          when (<= capacity threshold)
            return threshold)))

(defun send-battery-alert (capacity status battery-name)
  "Send a battery alert. This logs a warning that should be visible to the user."
  (log:warn "BATTERY ALERT: ~A at ~D% (~A)" battery-name capacity status)
  ;; Future enhancement: could integrate with notification system
  ;; or use the scheduler to trigger agent messages
  )

(defun check-battery-and-alert ()
  "Check battery status and send alerts if thresholds are crossed.
   Avoids duplicate alerts for the same threshold level."
  (let ((batteries (all-batteries-info)))
    (when batteries
      (dolist (battery batteries)
        (let* ((capacity (getf battery :capacity))
               (status (getf battery :status))
               (name (getf battery :name))
               (alert-threshold (should-alert-p capacity)))
          (when (and alert-threshold
                     (eq status :discharging)
                     (or (null *battery-last-alert-level*)
                         (< capacity *battery-last-alert-level*)))
            (send-battery-alert capacity status name)
            (setf *battery-last-alert-level* capacity)))
        ;; Reset alert tracking when battery is charging or full
        (when (member (getf battery :status) '(:charging :full))
          (setf *battery-last-alert-level* nil))))))

;;; --- Formatted output ---

(defun format-time-remaining (seconds &optional (stream *standard-output*))
  "Format time remaining in human-readable format."
  (if (null seconds)
      (format stream "N/A")
      (let* ((hours (truncate seconds 3600))
             (minutes (truncate (mod seconds 3600) 60)))
        (format stream "~Dh ~Dm" hours minutes))))

(defun format-battery-info (info &optional (stream *standard-output*))
  "Format a single battery's information as human-readable text."
  (let ((name (getf info :name))
        (capacity (getf info :capacity))
        (status (getf info :status))
        (time-remaining (getf info :time-remaining-seconds)))
    (format stream "~&Battery: ~A~%" name)
    (format stream "  Capacity: ~D%~%" capacity)
    (format stream "  Status: ~A~%" (string-capitalize (symbol-name status)))
    (when time-remaining
      (format stream "  Time remaining: ")
      (format-time-remaining time-remaining stream)
      (format stream "~%"))))

(defun format-all-batteries (batteries &optional (stream *standard-output*))
  "Format all batteries information."
  (if (null batteries)
      (format stream "~&No batteries detected.~%")
      (dolist (bat batteries)
        (format-battery-info bat stream))))

;;; --- Public interface ---

(defun battery-status ()
  "Get battery status for all batteries. Returns a plist.
   Suitable for programmatic use by other daemon components."
  (let ((batteries (all-batteries-info)))
    (list :batteries batteries
          :count (length batteries)
          :has-battery (not (null batteries)))))

(defun battery-report (&key (stream *standard-output*))
  "Display a human-friendly battery status report.
   Returns the battery status plist."
  (let ((status (battery-status)))
    (if (getf status :has-battery)
        (progn
          (format stream "~&Battery Status~%")
          (format stream "~&=============~%")
          (format-all-batteries (getf status :batteries) stream)
          (format stream "~&Configured alert thresholds: ~{~D%~^, ~}~%"
                  (get-battery-thresholds)))
        (format stream "~&No batteries detected (desktop system).~%"))
    status))

;;; --- Scheduler integration ---

(defun start-battery-monitoring ()
  "Start periodic battery monitoring with alerts.
   Checks every N seconds (configurable via [battery] check_interval_seconds)."
  (let ((interval (or (crichton/config:config-section-get :battery :check-interval-seconds)
                      *battery-check-interval*)))
    (when (find-batteries)
      (schedule-every "battery-monitor" interval #'check-battery-and-alert
                      :replace t)
      (log:info "Battery monitoring started (checking every ~Ds)" interval)
      t)))

(defun stop-battery-monitoring ()
  "Stop periodic battery monitoring."
  (when (cancel-task "battery-monitor")
    (log:info "Battery monitoring stopped")
    t))
