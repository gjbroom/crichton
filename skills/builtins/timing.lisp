;;;; skills/builtins/timing.lisp
;;;;
;;;; Built-in skill: execution timing and stopwatch accumulators.
;;;; Uses SBCL's microsecond-resolution internal-real-time.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Core timing ---

(declaim (inline elapsed-us))
(defun elapsed-us (start end)
  "Compute elapsed microseconds between two internal-real-time values."
  (let ((units-per-us (/ internal-time-units-per-second 1000000)))
    (round (/ (- end start) units-per-us))))

(defun %log-timing (level label elapsed-us)
  "Log a timing measurement at the given level."
  (let ((msg (format nil "~@[~A: ~]~D µs (~,3F ms)"
                     label elapsed-us (/ elapsed-us 1000.0d0))))
    (ecase level
      (:debug (log:debug "~A" msg))
      (:info  (log:info  "~A" msg))
      (:warn  (log:warn  "~A" msg)))))

(defmacro with-timing ((&key label (log-level :debug) log-p) &body body)
  "Execute BODY and return its values plus elapsed microseconds as an extra value.
   When LOG-P is true, log the elapsed time at LOG-LEVEL."
  (let ((t0 (gensym "T0"))
        (t1 (gensym "T1"))
        (elapsed (gensym "ELAPSED"))
        (vals (gensym "VALS")))
    `(let* ((,t0 (get-internal-real-time))
            (,vals (multiple-value-list (progn ,@body)))
            (,t1 (get-internal-real-time))
            (,elapsed (elapsed-us ,t0 ,t1)))
       ,@(when log-p
           `((%log-timing ,log-level ,label ,elapsed)))
       (values-list (append ,vals (list ,elapsed))))))

;;; --- Stopwatch accumulator ---

(defstruct (stopwatch (:constructor %make-stopwatch))
  (name "" :type string)
  (count 0 :type fixnum)
  (total-us 0 :type integer)
  (min-us most-positive-fixnum :type integer)
  (max-us 0 :type integer)
  (last-us 0 :type integer)
  (lock (bt:make-lock "stopwatch") :type t))

(defun make-stopwatch (name)
  "Create a new stopwatch accumulator with the given NAME."
  (%make-stopwatch :name (string name)))

(defun stopwatch-record (sw elapsed-us)
  "Record an elapsed time (in microseconds) into stopwatch SW. Thread-safe."
  (bt:with-lock-held ((stopwatch-lock sw))
    (incf (stopwatch-count sw))
    (incf (stopwatch-total-us sw) elapsed-us)
    (setf (stopwatch-last-us sw) elapsed-us)
    (when (< elapsed-us (stopwatch-min-us sw))
      (setf (stopwatch-min-us sw) elapsed-us))
    (when (> elapsed-us (stopwatch-max-us sw))
      (setf (stopwatch-max-us sw) elapsed-us)))
  elapsed-us)

(defun stopwatch-reset (sw)
  "Reset all counters in stopwatch SW. Thread-safe."
  (bt:with-lock-held ((stopwatch-lock sw))
    (setf (stopwatch-count sw) 0
          (stopwatch-total-us sw) 0
          (stopwatch-min-us sw) most-positive-fixnum
          (stopwatch-max-us sw) 0
          (stopwatch-last-us sw) 0))
  sw)

(defun stopwatch-snapshot (sw)
  "Return a plist snapshot of stopwatch SW. Thread-safe."
  (bt:with-lock-held ((stopwatch-lock sw))
    (let ((count (stopwatch-count sw))
          (total (stopwatch-total-us sw)))
      (list :name (stopwatch-name sw)
            :count count
            :total-us total
            :avg-us (if (plusp count) (round total count) 0)
            :min-us (if (plusp count) (stopwatch-min-us sw) 0)
            :max-us (stopwatch-max-us sw)
            :last-us (stopwatch-last-us sw)))))

(defmacro with-stopwatch ((sw &key label log-p (log-level :debug)) &body body)
  "Execute BODY, record elapsed time into stopwatch SW, return BODY's values.
   Elapsed microseconds is appended as an extra return value."
  (let ((t0 (gensym "T0"))
        (t1 (gensym "T1"))
        (elapsed (gensym "ELAPSED"))
        (vals (gensym "VALS"))
        (sw-var (gensym "SW")))
    `(let* ((,sw-var ,sw)
            (,t0 (get-internal-real-time))
            (,vals (multiple-value-list (progn ,@body)))
            (,t1 (get-internal-real-time))
            (,elapsed (elapsed-us ,t0 ,t1)))
       (stopwatch-record ,sw-var ,elapsed)
       ,@(when log-p
           `((%log-timing ,log-level ,(or label `(stopwatch-name ,sw-var))
                          ,elapsed)))
       (values-list (append ,vals (list ,elapsed))))))

;;; --- Named stopwatch registry ---

(defvar *stopwatches* (make-hash-table :test #'equal)
  "Registry of named stopwatches.")
(defvar *stopwatches-lock* (bt:make-lock "stopwatches-registry"))

(defun get-stopwatch (name)
  "Get or create a named stopwatch. Thread-safe."
  (let ((key (string name)))
    (bt:with-lock-held (*stopwatches-lock*)
      (or (gethash key *stopwatches*)
          (setf (gethash key *stopwatches*) (make-stopwatch key))))))

(defun list-stopwatches ()
  "Return a list of plist snapshots for all registered stopwatches."
  (let (result)
    (bt:with-lock-held (*stopwatches-lock*)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (push (stopwatch-snapshot v) result))
               *stopwatches*))
    (sort result #'string< :key (lambda (p) (getf p :name)))))

(defun clear-stopwatches ()
  "Remove all registered stopwatches."
  (bt:with-lock-held (*stopwatches-lock*)
    (clrhash *stopwatches*)))

;;; --- Convenience ---

(defun format-duration-us (us)
  "Format microseconds as a human-readable string."
  (cond
    ((>= us 1000000) (format nil "~,2Fs" (/ us 1000000.0d0)))
    ((>= us 1000) (format nil "~,2Fms" (/ us 1000.0d0)))
    (t (format nil "~Dµs" us))))
