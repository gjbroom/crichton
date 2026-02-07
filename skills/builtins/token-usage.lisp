;;;; skills/builtins/token-usage.lisp
;;;;
;;;; Built-in skill: metered resource tracking and burn rate introspection.
;;;; A general-purpose token/unit accounting service — any subsystem that
;;;; consumes a metered resource (LLM tokens, API calls, rate-limited
;;;; endpoints) registers a meter and records usage through it.
;;;;
;;;; Provides per-meter and aggregate: totals, burn rate, cost estimation.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; ====================================================================
;;; Pricing tables
;;; ====================================================================

(defparameter *pricing-tables* (make-hash-table :test #'equal)
  "Per-meter pricing tables. Key = meter name, value = alist of
   (tier-name :input price-per-million :output price-per-million).")

(defun register-pricing (meter-name entries)
  "Register pricing for METER-NAME. ENTRIES is an alist:
   ((\"tier\" :input 3.0d0 :output 15.0d0) ...)"
  (setf (gethash meter-name *pricing-tables*) entries))

(defun tier-cost-per-million (meter-name tier direction)
  "Return cost per million units for METER-NAME's TIER in DIRECTION (:input/:output).
   Returns NIL if unknown."
  (let ((table (gethash meter-name *pricing-tables*)))
    (when table
      (let ((entry (assoc tier table :test #'string=)))
        (when entry
          (getf (cdr entry) direction))))))

(defun estimate-cost (meter-name tier input-units output-units)
  "Estimate cost for a usage event. Returns NIL if pricing is unknown."
  (let ((in-price (tier-cost-per-million meter-name tier :input))
        (out-price (tier-cost-per-million meter-name tier :output)))
    (when (and in-price out-price)
      (+ (* input-units (/ in-price 1000000.0d0))
         (* output-units (/ out-price 1000000.0d0))))))

;;; ====================================================================
;;; Default pricing: Anthropic Claude models
;;; ====================================================================

(register-pricing "llm"
  '(("claude-sonnet-4-20250514"    :input 3.00d0 :output 15.00d0)
    ("claude-opus-4-20250514"      :input 15.00d0 :output 75.00d0)
    ("claude-haiku-3-20240307"     :input 0.25d0 :output 1.25d0)
    ("claude-3-5-sonnet-20241022"  :input 3.00d0 :output 15.00d0)))

;;; ====================================================================
;;; Usage record
;;; ====================================================================

(defstruct (usage-record (:constructor %make-usage-record))
  (timestamp 0 :type integer)
  (tier "" :type string)
  (input-units 0 :type integer)
  (output-units 0 :type integer)
  (estimated-cost nil :type (or null double-float)))

(defun make-usage-record (meter-name tier input-units output-units)
  (%make-usage-record
   :timestamp (get-universal-time)
   :tier tier
   :input-units input-units
   :output-units output-units
   :estimated-cost (estimate-cost meter-name tier input-units output-units)))

;;; ====================================================================
;;; Meter — thread-safe accumulator for one named service
;;; ====================================================================

(defstruct (meter (:constructor %make-meter))
  (name "" :type string)
  (start-time (get-universal-time) :type integer)
  (total-input 0 :type integer)
  (total-output 0 :type integer)
  (total-cost 0.0d0 :type double-float)
  (call-count 0 :type fixnum)
  (history nil :type list)
  (max-history 1000 :type fixnum)
  (lock (bt:make-lock "meter") :type t))

;;; ====================================================================
;;; Meter registry
;;; ====================================================================

(defvar *meters* (make-hash-table :test #'equal)
  "Registry of named meters.")
(defvar *meters-lock* (bt:make-lock "meters-registry"))

(defun ensure-meter (name)
  "Get or create a named meter. Thread-safe."
  (let ((key (string name)))
    (bt:with-lock-held (*meters-lock*)
      (or (gethash key *meters*)
          (setf (gethash key *meters*)
                (%make-meter :name key))))))

(defun list-meters ()
  "Return a list of all meter names."
  (let (names)
    (bt:with-lock-held (*meters-lock*)
      (maphash (lambda (k v) (declare (ignore v)) (push k names)) *meters*))
    (sort names #'string<)))

(defun reset-meter (name)
  "Reset a named meter, starting fresh."
  (let ((key (string name)))
    (bt:with-lock-held (*meters-lock*)
      (setf (gethash key *meters*)
            (%make-meter :name key)))))

(defun reset-all-meters ()
  "Reset all meters."
  (bt:with-lock-held (*meters-lock*)
    (maphash (lambda (k v)
               (declare (ignore v))
               (setf (gethash k *meters*) (%make-meter :name k)))
             *meters*)))

;;; ====================================================================
;;; Recording usage
;;; ====================================================================

(defun record-usage (meter-name tier input-units output-units)
  "Record a usage event on METER-NAME. TIER identifies the pricing tier
   (e.g. model name, API plan). Thread-safe. Returns the usage record."
  (let* ((m (ensure-meter meter-name))
         (record (make-usage-record meter-name tier input-units output-units)))
    (bt:with-lock-held ((meter-lock m))
      (incf (meter-total-input m) input-units)
      (incf (meter-total-output m) output-units)
      (when (usage-record-estimated-cost record)
        (incf (meter-total-cost m) (usage-record-estimated-cost record)))
      (incf (meter-call-count m))
      (push record (meter-history m))
      (when (> (length (meter-history m)) (meter-max-history m))
        (setf (meter-history m)
              (subseq (meter-history m) 0 (meter-max-history m)))))
    record))

;;; ====================================================================
;;; Queries — programmatic interface (plist-returning)
;;; ====================================================================

(defun meter-snapshot (name)
  "Return a plist snapshot of a single meter. Thread-safe."
  (let ((m (ensure-meter name)))
    (bt:with-lock-held ((meter-lock m))
      (let* ((now (get-universal-time))
             (elapsed (max 1 (- now (meter-start-time m))))
             (total-units (+ (meter-total-input m) (meter-total-output m)))
             (elapsed-min (/ elapsed 60.0d0)))
        (list :name (meter-name m)
              :elapsed-seconds elapsed
              :call-count (meter-call-count m)
              :total-input (meter-total-input m)
              :total-output (meter-total-output m)
              :total-units total-units
              :total-cost (meter-total-cost m)
              :units-per-minute (/ total-units elapsed-min)
              :cost-per-minute (/ (meter-total-cost m) elapsed-min))))))

(defun all-meters-snapshot ()
  "Return a list of plist snapshots for all meters."
  (mapcar #'meter-snapshot (list-meters)))

(defun meter-recent (name &optional (n 10))
  "Return the N most recent usage records for a meter as plists."
  (let ((m (ensure-meter name)))
    (bt:with-lock-held ((meter-lock m))
      (mapcar (lambda (r)
                (list :timestamp (usage-record-timestamp r)
                      :tier (usage-record-tier r)
                      :input-units (usage-record-input-units r)
                      :output-units (usage-record-output-units r)
                      :estimated-cost (usage-record-estimated-cost r)))
              (subseq (meter-history m)
                      0 (min n (length (meter-history m))))))))

(defun aggregate-snapshot ()
  "Return a plist of aggregate usage across all meters."
  (let ((total-input 0)
        (total-output 0)
        (total-cost 0.0d0)
        (total-calls 0)
        (earliest most-positive-fixnum))
    (bt:with-lock-held (*meters-lock*)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (bt:with-lock-held ((meter-lock v))
                   (incf total-input (meter-total-input v))
                   (incf total-output (meter-total-output v))
                   (incf total-cost (meter-total-cost v))
                   (incf total-calls (meter-call-count v))
                   (setf earliest (min earliest (meter-start-time v)))))
               *meters*))
    (let* ((now (get-universal-time))
           (elapsed (max 1 (- now earliest)))
           (total-units (+ total-input total-output))
           (elapsed-min (/ elapsed 60.0d0)))
      (list :meter-count (hash-table-count *meters*)
            :elapsed-seconds elapsed
            :total-calls total-calls
            :total-input total-input
            :total-output total-output
            :total-units total-units
            :total-cost total-cost
            :units-per-minute (/ total-units elapsed-min)
            :cost-per-minute (/ total-cost elapsed-min)))))

;;; ====================================================================
;;; Formatted report — human interface
;;; ====================================================================

(defun format-cost (cost)
  "Format a dollar amount for display."
  (if (and cost (> cost 0))
      (if (< cost 0.01d0)
          (format nil "$~,4F" cost)
          (format nil "$~,2F" cost))
      "$0.00"))

(defun format-session-duration (seconds)
  "Format elapsed seconds as human-readable duration."
  (let ((hours (floor seconds 3600))
        (minutes (floor (mod seconds 3600) 60))
        (secs (mod seconds 60)))
    (cond
      ((>= hours 1) (format nil "~Dh ~Dm ~Ds" hours minutes secs))
      ((>= minutes 1) (format nil "~Dm ~Ds" minutes secs))
      (t (format nil "~Ds" secs)))))

(defun meter-report (name &key (stream *standard-output*) (recent 5))
  "Print a human-readable report for a single meter."
  (let ((snap (meter-snapshot name)))
    (format stream "--- ~A ---~%" (getf snap :name))
    (format stream "  Duration:    ~A~%"
            (format-session-duration (getf snap :elapsed-seconds)))
    (format stream "  Calls:       ~D~%" (getf snap :call-count))
    (format stream "  Input:       ~:D units~%" (getf snap :total-input))
    (format stream "  Output:      ~:D units~%" (getf snap :total-output))
    (format stream "  Total:       ~:D units~%" (getf snap :total-units))
    (format stream "  Cost:        ~A~%" (format-cost (getf snap :total-cost)))
    (format stream "  Burn rate:   ~,1F units/min~%" (getf snap :units-per-minute))
    (format stream "  Cost rate:   ~A/min~%" (format-cost (getf snap :cost-per-minute)))
    (let ((cost-min (getf snap :cost-per-minute)))
      (when (> cost-min 0)
        (format stream "  Projected:   ~A/hour~%"
                (format-cost (* cost-min 60.0d0)))))
    (let ((calls (meter-recent name recent)))
      (when calls
        (format stream "  Recent (~D):~%" (length calls))
        (dolist (c calls)
          (format stream "    ~A ~:Din/~:Dout ~A~%"
                  (getf c :tier)
                  (getf c :input-units)
                  (getf c :output-units)
                  (format-cost (getf c :estimated-cost))))))))

(defun usage-report (&key (stream *standard-output*) (recent 5))
  "Print an aggregate usage report across all meters."
  (let ((agg (aggregate-snapshot))
        (names (list-meters)))
    (format stream "=== Resource Usage Report ===~%")
    (format stream "Active meters: ~D~%" (getf agg :meter-count))
    (format stream "Session duration: ~A~%"
            (format-session-duration (getf agg :elapsed-seconds)))
    (format stream "Total calls: ~D~%" (getf agg :total-calls))
    (format stream "Total units: ~:D (~:Din / ~:Dout)~%"
            (getf agg :total-units) (getf agg :total-input) (getf agg :total-output))
    (format stream "Total cost:  ~A~%" (format-cost (getf agg :total-cost)))
    (format stream "Burn rate:   ~,1F units/min (~A/min)~%"
            (getf agg :units-per-minute) (format-cost (getf agg :cost-per-minute)))
    (let ((cost-min (getf agg :cost-per-minute)))
      (when (> cost-min 0)
        (format stream "Projected:   ~A/hour~%"
                (format-cost (* cost-min 60.0d0)))))
    (when names
      (format stream "~%")
      (dolist (name names)
        (meter-report name :stream stream :recent recent)
        (format stream "~%")))))
