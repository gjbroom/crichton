;;;; tests/run-all-tests.lisp
;;;;
;;;; Top-level runner: loads all unit test suites and runs them.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/run-all-tests.lisp")
;;;;   (crichton/tests:run-all)
;;;;
;;;; From the shell:
;;;;   make test

(let ((here (make-pathname :name nil :type nil
                           :defaults (or *load-truename*
                                         *default-pathname-defaults*))))
  (load (merge-pathnames "harness.lisp" here))
  (load (merge-pathnames "test-sanitize.lisp" here))
  (load (merge-pathnames "test-config.lisp" here))
  (load (merge-pathnames "test-llm-anthropic.lisp" here))
  (load (merge-pathnames "test-tools.lisp" here))
  (load (merge-pathnames "test-agent-loop.lisp" here)))

(defpackage #:crichton/tests
  (:use #:cl)
  (:export #:run-all))

(in-package #:crichton/tests)

(defun run-all ()
  "Run all unit test suites. Returns T if all passed, NIL if any failed."
  (format t "~%~%╔══════════════════════════════════════╗~%")
  (format t   "║        Crichton Unit Tests           ║~%")
  (format t   "╚══════════════════════════════════════╝~%")
  (let ((results
         (list (crichton/tests/sanitize:run-all)
               (crichton/tests/config:run-all)
               (crichton/tests/llm:run-all)
               (crichton/tests/tools:run-all)
               (crichton/tests/agent-loop:run-all))))
    (let ((all-passed (every #'identity results))
          (suite-count (length results))
          (pass-count  (count t results)))
      (format t "~%════════════════════════════════════════~%")
      (format t "Suites: ~D/~D passed~%" pass-count suite-count)
      (if all-passed
          (format t "Result: ALL TESTS PASSED~%")
          (format t "Result: FAILURES DETECTED~%"))
      (format t "════════════════════════════════════════~%~%")
      all-passed)))
