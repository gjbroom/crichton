;;;; skills/builtins/scheduler.lisp
;;;;
;;;; Built-in skill: wall-clock task scheduler.
;;;; Supports one-shot, interval, and daily recurring tasks.
;;;; Single scheduler thread sleeps until next due task (no polling).
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Task representation ---

(defclass scheduled-task ()
  ((name                      :initarg :name
                              :accessor scheduled-task-name
                              :initform ""
                              :type string)
   (kind                      :initarg :kind
                              :accessor scheduled-task-kind
                              :initform :one-shot
                              :type keyword)
   (fn                        :initarg :fn
                              :accessor scheduled-task-fn
                              :initform (constantly nil)
                              :type function)
   (action-name               :initarg :action-name
                              :accessor scheduled-task-action-name
                              :initform nil
                              :type (or null string))
   (agent-prompt              :initarg :agent-prompt
                              :accessor scheduled-task-agent-prompt
                              :initform nil
                              :type (or null string))
   (next-ut                   :initarg :next-ut
                              :accessor scheduled-task-next-ut
                              :initform 0
                              :type integer)
   (interval-seconds          :initarg :interval-seconds
                              :accessor scheduled-task-interval-seconds
                              :initform 0
                              :type integer)
   (daily-hour                :initarg :daily-hour
                              :accessor scheduled-task-daily-hour
                              :initform 0
                              :type (integer 0 23))
   (daily-minute              :initarg :daily-minute
                              :accessor scheduled-task-daily-minute
                              :initform 0
                              :type (integer 0 59))
   (allow-overlap-p           :initarg :allow-overlap-p
                              :accessor scheduled-task-allow-overlap-p
                              :initform nil
                              :type boolean)
   (running-p                 :initarg :running-p
                              :accessor scheduled-task-running-p
                              :initform nil
                              :type boolean)
   (run-count                 :initarg :run-count
                              :accessor scheduled-task-run-count
                              :initform 0
                              :type integer)
   (last-start-ut             :initarg :last-start-ut
                              :accessor scheduled-task-last-start-ut
                              :initform nil
                              :type (or null integer))
   (last-end-ut               :initarg :last-end-ut
                              :accessor scheduled-task-last-end-ut
                              :initform nil
                              :type (or null integer))
   (last-duration-us          :initarg :last-duration-us
                              :accessor scheduled-task-last-duration-us
                              :initform nil
                              :type (or null integer))
   (last-error                :initarg :last-error
                              :accessor scheduled-task-last-error
                              :initform nil
                              :type (or null string))))

(defun make-scheduled-task (&key (name "")
                               (kind :one-shot)
                               (fn (constantly nil))
                               action-name
                               agent-prompt
                               (next-ut 0)
                               (interval-seconds 0)
                               (daily-hour 0)
                               (daily-minute 0)
                               allow-overlap-p
                               running-p
                               (run-count 0)
                               last-start-ut
                               last-end-ut
                               last-duration-us
                               last-error)
  (make-instance 'scheduled-task :name name
                                 :kind kind
                                 :fn fn
                                 :action-name action-name
                                 :agent-prompt agent-prompt
                                 :next-ut next-ut
                                 :interval-seconds interval-seconds
                                 :daily-hour daily-hour
                                 :daily-minute daily-minute
                                 :allow-overlap-p allow-overlap-p
                                 :running-p running-p
                                 :run-count run-count
                                 :last-start-ut last-start-ut
                                 :last-end-ut last-end-ut
                                 :last-duration-us last-duration-us
                                 :last-error last-error))

;;; --- Schedulable action registry ---

(defvar *schedulable-actions* (make-hash-table :test #'equal))

(defun register-schedulable-action (name description fn)
  "Register a named action that can be scheduled via the agent tool."
  (setf (gethash name *schedulable-actions*)
        (list :name name :description description :fn fn)))

(defun get-schedulable-action (name)
  "Look up a schedulable action by name. Returns plist or NIL."
  (gethash name *schedulable-actions*))

(defun list-schedulable-actions ()
  "Return a list of plists for all registered schedulable actions."
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore k))
               (push (list :name (getf v :name) :description (getf v :description)) result))
             *schedulable-actions*)
    (sort result #'string< :key (lambda (x) (getf x :name)))))

(defun register-default-actions ()
  "Register built-in schedulable actions."
  (register-schedulable-action
   "weather-check" "Log current weather conditions"
   (lambda ()
     (let ((report (with-output-to-string (s)
                     (weather-report :stream s))))
       (log:info "Weather check:~%~A" report))))
  (register-schedulable-action
   "system-check" "Log system health metrics"
   (lambda ()
     (let ((snap (system-snapshot)))
       (log:info "System check: load=~,2F mem=~,1F%"
                 (or (getf (getf snap :loadavg) :load1) 0)
                 (* 100 (or (getf (getf snap :memory) :mem-used-ratio) 0))))))
  (register-schedulable-action
   "battery-check" "Check battery levels against thresholds"
   #'battery-monitor-callback))

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
    (declare (ignore s m h))
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
    (register-default-actions)
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

(defun schedule-at (name time fn &key replace action-name agent-prompt)
  "Schedule a one-shot task to run at universal-time TIME.
   If REPLACE is true, cancel any existing task with the same NAME."
  (let ((task (make-scheduled-task
               :name (string name) :kind :one-shot
               :fn fn :next-ut time
               :action-name action-name
               :agent-prompt agent-prompt)))
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
                       &key (start-at nil) replace (allow-overlap nil) action-name agent-prompt)
  "Schedule a recurring task to run every INTERVAL-SECONDS seconds.
   START-AT is a universal-time for the first run (default: now + interval)."
  (let* ((start (or start-at (+ (get-universal-time) interval-seconds)))
         (task (make-scheduled-task
                :name (string name) :kind :every
                :fn fn :next-ut start
                :interval-seconds interval-seconds
                :allow-overlap-p allow-overlap
                :action-name action-name
                :agent-prompt agent-prompt)))
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

(defun schedule-daily (name hour minute fn &key replace (allow-overlap nil) action-name agent-prompt)
  "Schedule a task to run daily at HOUR:MINUTE (local time)."
  (let* ((next (next-daily-ut hour minute))
         (task (make-scheduled-task
                :name (string name) :kind :daily
                :fn fn :next-ut next
                :daily-hour hour :daily-minute minute
                :allow-overlap-p allow-overlap
                :action-name action-name
                :agent-prompt agent-prompt)))
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

;;; --- Prompt-task helpers ---

(defun schedule-prompt-every (name interval-seconds prompt &key (start-at nil) replace)
  "Schedule an agent prompt to run every INTERVAL-SECONDS seconds.
   The prompt string is persisted so the task survives daemon restarts."
  (schedule-every name interval-seconds
                  (lambda () (crichton/agent:run-agent prompt))
                  :start-at start-at :replace replace
                  :agent-prompt prompt))

(defun schedule-prompt-daily (name hour minute prompt &key replace)
  "Schedule an agent prompt to run daily at HOUR:MINUTE (local time).
   The prompt string is persisted so the task survives daemon restarts."
  (schedule-daily name hour minute
                  (lambda () (crichton/agent:run-agent prompt))
                  :replace replace
                  :agent-prompt prompt))

(defun schedule-prompt-at (name time prompt &key replace)
  "Schedule an agent prompt to run once at universal-time TIME.
   The prompt string is persisted so the task survives daemon restarts."
  (schedule-at name time
               (lambda () (crichton/agent:run-agent prompt))
               :replace replace
               :agent-prompt prompt))

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
        :action-name (scheduled-task-action-name task)
        :agent-prompt (scheduled-task-agent-prompt task)
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

;;; --- Current time reporting ---

(defun current-time-plist ()
  "Return current time as a plist with both ISO string and universal-time formats."
  (let ((now (get-universal-time)))
    (multiple-value-bind (s mi h d mo y dow)
        (decode-universal-time now)
      (let ((day-names #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
        (list :iso (format-ut now)
              :unix-seconds now
              :year y
              :month mo
              :day d
              :hour h
              :minute mi
              :second s
              :day-of-week (aref day-names dow)
              :dow-num dow)))))

(defun current-time-report (&key (stream t))
  "Print current time in a human-friendly format."
  (let ((now (get-universal-time)))
    (multiple-value-bind (s mi h d mo y dow)
        (decode-universal-time now)
      (let ((day-names #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
        (format stream "~A~%" (format-ut now))
        (format stream "~A, ~A ~D, ~D~%" 
                (aref day-names dow) 
                (svref #("" "January" "February" "March" "April" "May" "June"
                         "July" "August" "September" "October" "November" "December")
                       mo)
                d y)
        (format stream "~2,'0D:~2,'0D:~2,'0D~%" h mi s)))))

;;; --- Task persistence ---

(defun persist-user-tasks ()
  "Save all current user tasks to encrypted storage."
  (bt:with-lock-held (*scheduler-lock*)
    (let ((user-tasks (remove-if-not #'user-task-p *scheduled-tasks*)))
      (save-user-tasks user-tasks))))

(defun restore-user-tasks ()
  "Restore user tasks from encrypted storage. Call after start-scheduler."
  (let ((task-plists (load-user-tasks)))
    (when task-plists
      (let ((restored 0) (failed 0))
        (dolist (plist task-plists)
          (handler-case
              (let* ((action-name (getf plist :action-name))
                     (agent-prompt (getf plist :agent-prompt))
                     (action (when action-name
                               (get-schedulable-action action-name)))
                     (fn (cond
                           (agent-prompt
                            (let ((p agent-prompt))
                              (lambda () (crichton/agent:run-agent p))))
                           (action (getf action :fn))
                           (t nil))))
                (if (null fn)
                    (progn
                      (log:warn "Cannot restore task ~A: action ~A not found"
                                (getf plist :name) action-name)
                      (incf failed))
                    (progn
                      (case (getf plist :kind)
                        (:every
                         (schedule-every (getf plist :name)
                                         (getf plist :interval-seconds)
                                         fn
                                         :replace t
                                         :action-name action-name
                                         :agent-prompt agent-prompt))
                        (:daily
                         (schedule-daily (getf plist :name)
                                         (getf plist :daily-hour)
                                         (getf plist :daily-minute)
                                         fn
                                         :replace t
                                         :action-name action-name
                                         :agent-prompt agent-prompt))
                        (:one-shot
                         (let ((next-ut (getf plist :next-ut)))
                           (if (> next-ut (get-universal-time))
                               (schedule-at (getf plist :name) next-ut fn
                                            :replace t
                                            :action-name action-name
                                            :agent-prompt agent-prompt)
                               (progn
                                 (log:info "Skipping past one-shot task ~A"
                                           (getf plist :name))
                                 (incf failed))))))
                      (incf restored))))
            (error (c)
              (log:warn "Error restoring task ~A: ~A" (getf plist :name) c)
              (incf failed))))
        (log:info "Restored ~D user tasks (~D failed)" restored failed)))))
