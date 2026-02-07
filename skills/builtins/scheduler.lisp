;;;; skills/builtins/scheduler.lisp
;;;;
;;;; Built-in skill: wall-clock task scheduler.
;;;; Supports one-shot, interval, and daily recurring tasks.
;;;; Single scheduler thread sleeps until next due task (no polling).
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Task representation ---

(defstruct (scheduled-task (:constructor %make-scheduled-task))
  (name "" :type string)
  (kind :one-shot :type keyword)              ; :one-shot :every :daily
  (fn (constantly nil) :type function)
  (next-ut 0 :type integer)                   ; universal-time of next fire
  (interval-seconds 0 :type integer)          ; for :every
  (daily-hour 0 :type (integer 0 23))         ; for :daily
  (daily-minute 0 :type (integer 0 59))       ; for :daily
  (allow-overlap-p nil :type boolean)
  (running-p nil :type boolean)
  (run-count 0 :type integer)
  (last-start-ut nil :type (or null integer))
  (last-end-ut nil :type (or null integer))
  (last-duration-us nil :type (or null integer))
  (last-error nil :type (or null string)))

;;; --- Scheduler state ---

(defvar *scheduler-lock* (bt:make-lock "scheduler"))
(defvar *scheduler-cv* (bt:make-condition-variable :name "scheduler-cv"))
(defvar *scheduler-running* nil)
(defvar *scheduler-thread* nil)
(defvar *scheduled-tasks* nil
  "List of scheduled-task structs, sorted by next-ut ascending.")

;;; --- Internal helpers ---

(defun insert-task-sorted (task)
  "Insert TASK into *scheduled-tasks* maintaining next-ut sort order.
   Caller must hold *scheduler-lock*."
  (let ((ut (scheduled-task-next-ut task)))
    (if (or (null *scheduled-tasks*)
            (<= ut (scheduled-task-next-ut (first *scheduled-tasks*))))
        (push task *scheduled-tasks*)
        (loop for tail on *scheduled-tasks*
              for next = (cdr tail)
              when (or (null next)
                       (<= ut (scheduled-task-next-ut (car next))))
                do (setf (cdr tail) (cons task next))
                   (return)))))

(defun remove-task-by-name (name)
  "Remove task named NAME from *scheduled-tasks*.
   Caller must hold *scheduler-lock*. Returns the removed task or NIL."
  (let ((found nil))
    (setf *scheduled-tasks*
          (remove-if (lambda (task)
                       (when (string= name (scheduled-task-name task))
                         (setf found task)
                         t))
                     *scheduled-tasks*))
    found))

(defun next-daily-ut (hour minute)
  "Compute the next universal-time for HOUR:MINUTE today or tomorrow."
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (let ((today-ut (encode-universal-time 0 minute hour day month year)))
      (if (> today-ut (get-universal-time))
          today-ut
          (let* ((tomorrow (+ (get-universal-time) 86400)))
            (multiple-value-bind (s2 m2 h2 d2 mo2 y2)
                (decode-universal-time tomorrow)
              (declare (ignore s2 m2 h2))
              (encode-universal-time 0 minute hour d2 mo2 y2)))))))

(defun reschedule-task (task)
  "Compute the next fire time for a recurring task and reinsert it.
   Caller must hold *scheduler-lock*."
  (case (scheduled-task-kind task)
    (:every
     (setf (scheduled-task-next-ut task)
           (+ (get-universal-time) (scheduled-task-interval-seconds task)))
     (insert-task-sorted task))
    (:daily
     (setf (scheduled-task-next-ut task)
           (next-daily-ut (scheduled-task-daily-hour task)
                          (scheduled-task-daily-minute task)))
     (insert-task-sorted task))))

(defun dispatch-task (task)
  "Run TASK in a worker thread. Records timing and errors."
  (setf (scheduled-task-running-p task) t)
  (bt:make-thread
   (lambda ()
     (let ((start-ut (get-universal-time))
           (t0 (get-internal-real-time)))
       (setf (scheduled-task-last-start-ut task) start-ut)
       (handler-case
           (progn
             (funcall (scheduled-task-fn task))
             (setf (scheduled-task-last-error task) nil))
         (error (c)
           (let ((msg (format nil "~A" c)))
             (setf (scheduled-task-last-error task) msg)
             (log:warn "Scheduled task ~A failed: ~A"
                       (scheduled-task-name task) msg))))
       (let* ((t1 (get-internal-real-time))
              (elapsed (elapsed-us t0 t1)))
         (setf (scheduled-task-last-end-ut task) (get-universal-time)
               (scheduled-task-last-duration-us task) elapsed
               (scheduled-task-running-p task) nil)
         (incf (scheduled-task-run-count task))
         (log:debug "Task ~A completed in ~A"
                    (scheduled-task-name task) (format-duration-us elapsed)))
       (bt:with-lock-held (*scheduler-lock*)
         (reschedule-task task)
         (bt:condition-notify *scheduler-cv*))))
   :name (format nil "task:~A" (scheduled-task-name task))))

;;; --- Scheduler thread ---

(defun scheduler-loop ()
  "Main scheduler loop. Sleeps until next task is due or notified."
  (log:info "Scheduler started")
  (loop
    (bt:with-lock-held (*scheduler-lock*)
      (unless *scheduler-running*
        (log:info "Scheduler stopping")
        (return))
      (cond
        ((null *scheduled-tasks*)
         (bt:condition-wait *scheduler-cv* *scheduler-lock*))
        (t
         (let* ((next-task (first *scheduled-tasks*))
                (now (get-universal-time))
                (delta (- (scheduled-task-next-ut next-task) now)))
           (cond
             ((<= delta 0)
              (pop *scheduled-tasks*)
              (if (and (scheduled-task-running-p next-task)
                       (not (scheduled-task-allow-overlap-p next-task)))
                  (progn
                    (log:debug "Skipping ~A (still running)"
                               (scheduled-task-name next-task))
                    (reschedule-task next-task))
                  (dispatch-task next-task)))
             (t
              (bt:condition-wait *scheduler-cv* *scheduler-lock*
                                 :timeout delta)))))))))

;;; --- Public API: lifecycle ---

(defun start-scheduler ()
  "Start the scheduler thread. Idempotent."
  (bt:with-lock-held (*scheduler-lock*)
    (when *scheduler-running*
      (return-from start-scheduler :already-running))
    (setf *scheduler-running* t
          *scheduler-thread*
          (bt:make-thread #'scheduler-loop :name "crichton-scheduler")))
  :started)

(defun stop-scheduler ()
  "Stop the scheduler and cancel all pending tasks. Idempotent."
  (let ((thread nil))
    (bt:with-lock-held (*scheduler-lock*)
      (unless *scheduler-running*
        (return-from stop-scheduler :not-running))
      (setf *scheduler-running* nil
            *scheduled-tasks* nil
            thread *scheduler-thread*)
      (bt:condition-notify *scheduler-cv*))
    (when (and thread (bt:thread-alive-p thread))
      (bt:join-thread thread))
    (setf *scheduler-thread* nil)
    :stopped))

;;; --- Public API: scheduling ---

(defun schedule-at (name time fn &key replace)
  "Schedule a one-shot task to run at universal-time TIME.
   If REPLACE is true, cancel any existing task with the same NAME."
  (let ((task (%make-scheduled-task
               :name (string name) :kind :one-shot
               :fn fn :next-ut time)))
    (bt:with-lock-held (*scheduler-lock*)
      (when replace (remove-task-by-name (string name)))
      (when (find (string name) *scheduled-tasks*
                  :key #'scheduled-task-name :test #'string=)
        (error "Task ~S already exists (use :replace t)" name))
      (insert-task-sorted task)
      (bt:condition-notify *scheduler-cv*))
    (log:info "Scheduled ~A at ~A" name (format-ut time))
    name))

(defun schedule-every (name interval-seconds fn
                       &key (start-at nil) replace (allow-overlap nil))
  "Schedule a recurring task to run every INTERVAL-SECONDS seconds.
   START-AT is a universal-time for the first run (default: now + interval)."
  (let* ((start (or start-at (+ (get-universal-time) interval-seconds)))
         (task (%make-scheduled-task
                :name (string name) :kind :every
                :fn fn :next-ut start
                :interval-seconds interval-seconds
                :allow-overlap-p allow-overlap)))
    (bt:with-lock-held (*scheduler-lock*)
      (when replace (remove-task-by-name (string name)))
      (when (find (string name) *scheduled-tasks*
                  :key #'scheduled-task-name :test #'string=)
        (error "Task ~S already exists (use :replace t)" name))
      (insert-task-sorted task)
      (bt:condition-notify *scheduler-cv*))
    (log:info "Scheduled ~A every ~Ds (first at ~A)"
              name interval-seconds (format-ut start))
    name))

(defun schedule-daily (name hour minute fn &key replace (allow-overlap nil))
  "Schedule a task to run daily at HOUR:MINUTE (local time)."
  (let* ((next (next-daily-ut hour minute))
         (task (%make-scheduled-task
                :name (string name) :kind :daily
                :fn fn :next-ut next
                :daily-hour hour :daily-minute minute
                :allow-overlap-p allow-overlap)))
    (bt:with-lock-held (*scheduler-lock*)
      (when replace (remove-task-by-name (string name)))
      (when (find (string name) *scheduled-tasks*
                  :key #'scheduled-task-name :test #'string=)
        (error "Task ~S already exists (use :replace t)" name))
      (insert-task-sorted task)
      (bt:condition-notify *scheduler-cv*))
    (log:info "Scheduled ~A daily at ~2,'0D:~2,'0D (next: ~A)"
              name hour minute (format-ut next))
    name))

;;; --- Public API: management ---

(defun cancel-task (name)
  "Cancel a scheduled task by NAME. Returns T if found and removed."
  (let ((found nil))
    (bt:with-lock-held (*scheduler-lock*)
      (setf found (remove-task-by-name (string name))))
    (when found
      (log:info "Cancelled task ~A" name))
    (not (null found))))

(defun task-snapshot (task)
  "Return a plist representation of a scheduled-task struct."
  (list :name (scheduled-task-name task)
        :kind (scheduled-task-kind task)
        :next-fire (format-ut (scheduled-task-next-ut task))
        :next-fire-ut (scheduled-task-next-ut task)
        :running-p (scheduled-task-running-p task)
        :run-count (scheduled-task-run-count task)
        :last-duration-us (scheduled-task-last-duration-us task)
        :last-error (scheduled-task-last-error task)))

(defun list-tasks ()
  "Return a list of plist snapshots for all scheduled tasks."
  (bt:with-lock-held (*scheduler-lock*)
    (mapcar #'task-snapshot *scheduled-tasks*)))

(defun scheduler-status ()
  "Return a plist describing the scheduler state."
  (bt:with-lock-held (*scheduler-lock*)
    (list :running *scheduler-running*
          :task-count (length *scheduled-tasks*)
          :next-fire (when *scheduled-tasks*
                       (format-ut (scheduled-task-next-ut
                                   (first *scheduled-tasks*))))
          :tasks (mapcar #'task-snapshot *scheduled-tasks*))))

;;; --- Formatting ---

(defun format-ut (ut)
  "Format a universal-time as an ISO-ish string for display."
  (multiple-value-bind (s mi h d mo y)
      (decode-universal-time ut)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            y mo d h mi s)))
