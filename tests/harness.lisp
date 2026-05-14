;;;; tests/harness.lisp
;;;;
;;;; Shared test harness for all Crichton test suites.
;;;; Extracted from test-kv-store.lisp to avoid per-file duplication.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/harness.lisp")
;;;;   ;; then load individual test files

(defpackage #:crichton/tests/harness
  (:use #:cl)
  (:export #:*pass-count* #:*fail-count*
           #:reset-counts
           #:pass #:fail
           #:check #:check= #:check-signals #:check-error #:check-no-error
           #:print-summary))

(in-package #:crichton/tests/harness)

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

(defmacro check-signals (label condition-type &body body)
  `(let ((signaled nil))
     (handler-case (progn ,@body)
       (,condition-type () (setf signaled t)))
     (if signaled
         (pass ,label)
         (fail ,label (format nil "expected ~S to be signaled" ',condition-type)))))

(defmacro check-error (label &body body)
  "Pass if BODY signals any ERROR subtype."
  `(let ((signaled nil))
     (handler-case (progn ,@body)
       (error () (setf signaled t)))
     (if signaled
         (pass ,label)
         (fail ,label "expected an ERROR to be signaled"))))

(defmacro check-no-error (label &body body)
  "Pass if BODY completes without signalling any ERROR."
  `(block check-no-error
     (handler-case (progn ,@body)
       (error (c)
         (fail ,label (format nil "unexpected error: ~A" c))
         (return-from check-no-error nil)))
     (pass ,label)))

(defun print-summary (suite-name)
  (format t "  → ~A: ~D passed, ~D failed~%~%" suite-name *pass-count* *fail-count*))
