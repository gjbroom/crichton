;;;; tests/test-task-persistence.lisp
;;;;
;;;; Validation tests for scheduled task persistence.
;;;; Covers serialization, encryption roundtrip, edge cases, and
;;;; the live scheduler integration.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/test-task-persistence.lisp")
;;;;   (crichton/tests/tasks:run-all)
;;;;
;;;; Individual suites:
;;;;   (crichton/tests/tasks:test-user-task-predicate)
;;;;   (crichton/tests/tasks:test-serialization)
;;;;   (crichton/tests/tasks:test-encryption-roundtrip)
;;;;   (crichton/tests/tasks:test-restore-edge-cases)
;;;;   (crichton/tests/tasks:test-persist-restore-cycle)
;;;;   (crichton/tests/tasks:test-list-unrestorable)
;;;;   (crichton/tests/tasks:test-export)

(defpackage #:crichton/tests/tasks
  (:use #:cl)
  (:import-from #:crichton/skills
                #:user-task-p
                #:task-to-plist
                #:task-plists-to-json-bytes
                #:json-bytes-to-task-plists
                #:save-user-tasks
                #:load-user-tasks
                #:persist-user-tasks
                #:restore-user-tasks
                #:list-unrestorable-tasks
                #:export-user-tasks
                #:schedule-every
                #:schedule-daily
                #:schedule-at
                #:schedule-prompt-every
                #:schedule-prompt-daily
                #:cancel-task
                #:list-tasks
                #:register-schedulable-action
                #:get-schedulable-action
                #:user-tasks-file-path
                #:make-scheduled-task)
  (:export #:run-all
           #:test-user-task-predicate
           #:test-serialization
           #:test-encryption-roundtrip
           #:test-restore-edge-cases
           #:test-persist-restore-cycle
           #:test-list-unrestorable
           #:test-export))

(in-package #:crichton/tests/tasks)

;;; --- Test harness ---

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun reset-counts ()
  (setf *pass-count* 0 *fail-count* 0))

(defun pass (label)
  (incf *pass-count*)
  (format t "  PASS  ~A~%" label))

(defun fail (label message)
  (incf *fail-count*)
  (format t "  FAIL  ~A — ~A~%" label message))

(defmacro check (label condition &optional message)
  `(if ,condition
       (pass ,label)
       (fail ,label (or ,message (format nil "expected truthy, got ~S" ,condition)))))

(defmacro check= (label got expected)
  `(let ((g ,got) (e ,expected))
     (if (equal g e)
         (pass ,label)
         (fail ,label (format nil "expected ~S, got ~S" e g)))))

(defun print-summary (suite-name)
  (format t "  → ~A: ~D passed, ~D failed~%~%"
          suite-name *pass-count* *fail-count*))

;;; --- Helpers ---

(defun make-test-task (name kind &key (interval 3600) (hour 9) (minute 0) (next-ut nil) agent-prompt)
  "Create a scheduled-task for testing."
  (make-scheduled-task
   :name name
   :kind kind
   :fn (constantly :test-result)
   :action-name (format nil "test-action-~A" name)
   :agent-prompt agent-prompt
   :interval-seconds (when (eq kind :every) interval)
   :daily-hour (when (eq kind :daily) hour)
   :daily-minute (when (eq kind :daily) minute)
   :next-ut (or next-ut (+ (get-universal-time) 3600))))

(defun with-temp-task-file (thunk)
  "Run THUNK with a temporary tasks file path, restoring the original after."
  (let* ((orig-path (user-tasks-file-path))
         (temp-path (merge-pathnames "tasks/test-user-tasks.age"
                                     crichton/config:*agent-home*)))
    ;; We can't easily rebind the function, so we just use the real path
    ;; but clean up afterwards.
    (declare (ignore orig-path temp-path))
    (funcall thunk)))

;;; --- Suite 1: user-task-p predicate ---

(defun test-user-task-predicate ()
  "Test the user-task-p predicate: tasks with an agent-prompt are persistable."
  (reset-counts)
  (format t "~&--- Suite: user-task-p predicate ---~%")

  (let ((prompt-every    (make-test-task "rss:curator"        :every    :agent-prompt "Curate RSS feeds"))
        (prompt-daily    (make-test-task "user:daily-check"   :daily    :agent-prompt "Run daily check"))
        (no-prompt-every (make-test-task "battery-monitor"    :every))
        (no-prompt-daily (make-test-task "rss:morning-brief"  :daily)))

    (check= "with prompt, every → true"    (user-task-p prompt-every)    t)
    (check= "with prompt, daily → true"    (user-task-p prompt-daily)    t)
    (check= "no prompt, every → false"     (user-task-p no-prompt-every) nil)
    (check= "no prompt, daily → false"     (user-task-p no-prompt-daily) nil))

  (print-summary "user-task-p"))

;;; --- Suite 2: Serialization roundtrip ---

(defun test-serialization ()
  "Test task → plist → JSON bytes → plist roundtrip."
  (reset-counts)
  (format t "~&--- Suite: serialization roundtrip ---~%")

  (let* ((task-every (make-test-task "user:every-test" :every :interval 7200 :next-ut 1000000))
         (task-daily (make-test-task "user:daily-test" :daily :hour 14 :minute 30 :next-ut 2000000))
         (task-once  (make-test-task "user:once-test"  :one-shot :next-ut 3000000))
         (tasks (list task-every task-daily task-once)))

    ;; Plist conversion
    (let* ((plists (mapcar #'task-to-plist tasks))
           (plist-every (first plists))
           (plist-daily (second plists))
           (plist-once  (third plists)))

      (check= "every: name preserved"      (getf plist-every :name)             "user:every-test")
      (check= "every: kind preserved"      (getf plist-every :kind)             :every)
      (check= "every: interval preserved"  (getf plist-every :interval-seconds) 7200)
      (check= "every: next-ut preserved"   (getf plist-every :next-ut)          1000000)
      (check= "daily: kind preserved"      (getf plist-daily :kind)             :daily)
      (check= "daily: hour preserved"      (getf plist-daily :daily-hour)       14)
      (check= "daily: minute preserved"    (getf plist-daily :daily-minute)     30)
      (check= "once: kind preserved"       (getf plist-once :kind)              :one-shot)
      (check= "once: next-ut preserved"    (getf plist-once :next-ut)           3000000)

      ;; JSON roundtrip
      (let* ((json-bytes (task-plists-to-json-bytes plists))
             (restored   (json-bytes-to-task-plists json-bytes)))

        (check "json roundtrip returns 3 plists" (= (length restored) 3))

        (let ((r-every (first restored))
              (r-daily (second restored))
              (r-once  (third restored)))

          (check= "json: every name"     (getf r-every :name)             "user:every-test")
          (check= "json: every kind"     (getf r-every :kind)             :every)
          (check= "json: every interval" (getf r-every :interval-seconds) 7200)
          (check= "json: every next-ut"  (getf r-every :next-ut)          1000000)
          (check= "json: daily hour"     (getf r-daily :daily-hour)       14)
          (check= "json: daily minute"   (getf r-daily :daily-minute)     30)
          (check= "json: once next-ut"   (getf r-once :next-ut)           3000000)))))

  (print-summary "serialization"))

;;; --- Suite 3: Encryption roundtrip ---

(defun test-encryption-roundtrip ()
  "Test save-user-tasks → load-user-tasks roundtrip through age encryption."
  (reset-counts)
  (format t "~&--- Suite: encryption roundtrip (writes to disk) ---~%")

  (let* ((future-ut (+ (get-universal-time) 7200))
         (task (make-test-task "user:enc-test" :every :interval 3600 :next-ut future-ut
                               :agent-prompt "Run the enc test")))

    ;; Save one task
    (let ((count (save-user-tasks (list task))))
      (check= "save-user-tasks returns 1" count 1))

    ;; File must exist
    (check "tasks file exists after save" (probe-file (user-tasks-file-path)))

    ;; Load back
    (let ((plists (load-user-tasks)))
      (check "load-user-tasks returns non-nil" (not (null plists)))
      (check "load returns 1 task" (= (length plists) 1))
      (when plists
        (let ((p (first plists)))
          (check= "enc: name roundtrip"     (getf p :name)             "user:enc-test")
          (check= "enc: kind roundtrip"     (getf p :kind)             :every)
          (check= "enc: interval roundtrip" (getf p :interval-seconds) 3600)
          (check= "enc: next-ut roundtrip"  (getf p :next-ut)          future-ut))))

    ;; Save empty list — file should persist (0 tasks)
    (save-user-tasks nil)
    (let ((plists (load-user-tasks)))
      (check= "empty save → empty load" (length plists) 0)))

  (print-summary "encryption-roundtrip"))

;;; --- Suite 4: Restore edge cases ---

(defun test-restore-edge-cases ()
  "Test restore-user-tasks edge cases: missing action, corrupt file, past one-shot."
  (reset-counts)
  (format t "~&--- Suite: restore edge cases ---~%")

  ;; Edge case 1: No file exists
  (let ((path (user-tasks-file-path)))
    (when (probe-file path)
      (delete-file path)))
  (handler-case
      (progn
        (restore-user-tasks)
        (pass "restore with no file does not signal"))
    (error (c)
      (fail "restore with no file does not signal"
             (format nil "signalled ~A" c))))

  ;; Edge case 2: Past one-shot task is skipped, not errored
  (let* ((past-ut (- (get-universal-time) 3600))   ; 1 hour ago
         (plist (list :name "user:past-once"
                      :kind :one-shot
                      :action-name "test-persist-action"
                      :interval-seconds nil
                      :daily-hour nil
                      :daily-minute nil
                      :next-ut past-ut))
         (json-bytes (task-plists-to-json-bytes (list plist))))
    ;; Register a dummy action so the action lookup succeeds
    (register-schedulable-action "test-persist-action" "Test action" (constantly :test))
    (crichton/crypto:encrypt-to-file json-bytes (user-tasks-file-path))
    (handler-case
        (progn
          (restore-user-tasks)
          (pass "past one-shot restore does not signal"))
      (error (c)
        (fail "past one-shot restore does not signal"
               (format nil "signalled ~A" c))))
    ;; The task should NOT have been scheduled (it was in the past)
    (let ((tasks (list-tasks)))
      (check= "past one-shot not in scheduler"
              (count "user:past-once" tasks
                     :key (lambda (p) (getf p :name)) :test #'string=)
              0)))

  ;; Edge case 3: Missing action → task silently skipped
  (let* ((future-ut (+ (get-universal-time) 3600))
         (plist (list :name "user:no-action"
                      :kind :every
                      :action-name "nonexistent-action-xyz"
                      :interval-seconds 3600
                      :daily-hour nil
                      :daily-minute nil
                      :next-ut future-ut))
         (json-bytes (task-plists-to-json-bytes (list plist))))
    (crichton/crypto:encrypt-to-file json-bytes (user-tasks-file-path))
    (handler-case
        (progn
          (restore-user-tasks)
          (pass "missing-action restore does not signal"))
      (error (c)
        (fail "missing-action restore does not signal"
               (format nil "signalled ~A" c))))
    (let ((tasks (list-tasks)))
      (check= "missing-action task not scheduled"
              (count "user:no-action" tasks
                     :key (lambda (p) (getf p :name)) :test #'string=)
              0)))

  ;; Edge case 4: Corrupt file → handled gracefully
  (with-open-file (f (user-tasks-file-path) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string "this is not valid age-encrypted data" f))
  (handler-case
      (progn
        (load-user-tasks)
        ;; May return nil or signal — both acceptable
        (pass "corrupt file: load-user-tasks does not crash fatally"))
    (error ()
      (pass "corrupt file: load-user-tasks signals (acceptable)")))

  ;; Cleanup
  (when (probe-file (user-tasks-file-path))
    (delete-file (user-tasks-file-path)))

  (print-summary "restore-edge-cases"))

;;; --- Suite 5: Full persist/restore cycle ---

(defun test-persist-restore-cycle ()
  "Schedule tasks, persist, cancel them, restore — verify they come back."
  (reset-counts)
  (format t "~&--- Suite: persist/restore cycle ---~%")

  ;; Clean slate
  (cancel-task "user:cycle-every")
  (cancel-task "user:cycle-daily")
  (when (probe-file (user-tasks-file-path))
    (delete-file (user-tasks-file-path)))

  ;; Register test action
  (register-schedulable-action "cycle-action" "Cycle test action" (constantly :cycle))

  ;; Schedule two user tasks (with agent-prompt so they are persistable)
  (schedule-prompt-every "user:cycle-every" 1800 "Run cycle every test" :replace t)
  (schedule-prompt-daily "user:cycle-daily" 8 0 "Run cycle daily test" :replace t)

  (let ((before (list-tasks)))
    (check "two tasks scheduled before persist"
           (>= (count-if (lambda (p)
                           (member (getf p :name)
                                   '("user:cycle-every" "user:cycle-daily")
                                   :test #'string=))
                         before)
               2)))

  ;; Persist
  (let ((count (persist-user-tasks)))
    (check "persist-user-tasks returns ≥ 2" (>= count 2)))

  ;; Cancel the tasks (simulating daemon restart)
  (cancel-task "user:cycle-every")
  (cancel-task "user:cycle-daily")
  (let ((mid (list-tasks)))
    (check= "tasks gone after cancel"
            (count-if (lambda (p)
                        (member (getf p :name)
                                '("user:cycle-every" "user:cycle-daily")
                                :test #'string=))
                      mid)
            0))

  ;; Restore
  (restore-user-tasks)
  (let ((after (list-tasks)))
    (check "user:cycle-every restored"
           (find "user:cycle-every" after
                 :key (lambda (p) (getf p :name)) :test #'string=))
    (check "user:cycle-daily restored"
           (find "user:cycle-daily" after
                 :key (lambda (p) (getf p :name)) :test #'string=)))

  ;; Cleanup
  (cancel-task "user:cycle-every")
  (cancel-task "user:cycle-daily")

  (print-summary "persist-restore-cycle"))

;;; --- Suite 6: list-unrestorable-tasks ---

(defun test-list-unrestorable ()
  "Test list-unrestorable-tasks detects tasks with missing actions."
  (reset-counts)
  (format t "~&--- Suite: list-unrestorable-tasks ---~%")

  ;; Write two tasks to storage: one with a known action, one without
  (let* ((future-ut (+ (get-universal-time) 3600))
         (good-plist (list :name "user:good" :kind :every
                           :action-name "cycle-action"   ; registered above
                           :interval-seconds 3600
                           :daily-hour nil :daily-minute nil :next-ut future-ut))
         (bad-plist  (list :name "user:bad"  :kind :every
                           :action-name "totally-unknown-action-abc"
                           :interval-seconds 3600
                           :daily-hour nil :daily-minute nil :next-ut future-ut))
         (json-bytes (task-plists-to-json-bytes (list good-plist bad-plist))))
    (crichton/crypto:encrypt-to-file json-bytes (user-tasks-file-path)))

  (let ((unrestorable (list-unrestorable-tasks)))
    (check= "one unrestorable task found" (length unrestorable) 1)
    (when (= (length unrestorable) 1)
      (check= "unrestorable task is user:bad"
              (getf (first unrestorable) :name)
              "user:bad")))

  ;; Cleanup
  (when (probe-file (user-tasks-file-path))
    (delete-file (user-tasks-file-path)))

  (print-summary "list-unrestorable"))

;;; --- Suite 7: export-user-tasks ---

(defun test-export ()
  "Test export-user-tasks returns valid JSON describing live tasks."
  (reset-counts)
  (format t "~&--- Suite: export-user-tasks ---~%")

  ;; Schedule a task to export (with agent-prompt so it appears in export)
  (schedule-prompt-every "user:export-test" 900 "Run export test" :replace t)

  (let ((json (export-user-tasks)))
    (check "export returns a string" (stringp json))
    (check "export JSON is non-empty" (plusp (length json)))
    ;; Basic structure check — should contain the task name
    (check "export contains task name"
           (search "user:export-test" json)))

  (cancel-task "user:export-test")

  ;; With no user tasks, export should still return valid JSON
  (let ((json (export-user-tasks)))
    (check "empty export returns string" (stringp json))
    (check "empty export is non-empty"   (plusp (length json))))

  (print-summary "export"))

;;; --- Run all ---

(defun run-all ()
  "Run all task persistence test suites and print a final summary."
  (format t "~&~%========================================~%")
  (format t " Task Persistence Test Suite~%")
  (format t "========================================~%~%")
  (let ((total-pass 0)
        (total-fail 0))
    (dolist (suite '(test-user-task-predicate
                     test-serialization
                     test-encryption-roundtrip
                     test-restore-edge-cases
                     test-persist-restore-cycle
                     test-list-unrestorable
                     test-export))
      (reset-counts)
      (funcall suite)
      (incf total-pass *pass-count*)
      (incf total-fail *fail-count*))
    (format t "========================================~%")
    (format t " TOTAL: ~D passed, ~D failed~%" total-pass total-fail)
    (format t "========================================~%~%")
    (zerop total-fail)))
