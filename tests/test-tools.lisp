;;;; tests/test-tools.lisp
;;;;
;;;; Unit tests for pure helper functions in agent/tools.lisp.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/harness.lisp")
;;;;   (load "tests/test-tools.lisp")
;;;;   (crichton/tests/tools:run-all)
;;;;
;;;; Individual suites:
;;;;   (crichton/tests/tools:test-hget)
;;;;   (crichton/tests/tools:test-symbol-to-underscored)
;;;;   (crichton/tests/tools:test-make-json-schema)

(defpackage #:crichton/tests/tools
  (:use #:cl #:crichton/tests/harness)
  (:import-from #:crichton/agent
                #:hget
                #:make-json-schema
                #:symbol-to-underscored)
  (:export #:run-all
           #:test-hget
           #:test-symbol-to-underscored
           #:test-make-json-schema))

(in-package #:crichton/tests/tools)

;;; --- Suite 1: hget ---

(defun test-hget ()
  (reset-counts)
  (format t "~&--- Suite: hget ---~%")

  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) "alice"
          (gethash "count" ht) 42)

    (check= "present string key"
            (hget ht "name")
            "alice")

    (check= "present integer value"
            (hget ht "count")
            42)

    (check= "missing key → nil"
            (hget ht "missing")
            nil)

    (check= "missing key → supplied default"
            (hget ht "missing" "fallback")
            "fallback"))

  ;; nil table → default
  (check= "nil table → nil"
          (hget nil "key")
          nil)

  (check= "nil table → supplied default"
          (hget nil "key" 99)
          99)

  ;; non-hash-table → default
  (check= "non-ht → default"
          (hget "not-a-table" "key" "default")
          "default")

  (print-summary "hget"))

;;; --- Suite 2: symbol-to-underscored ---

(defun test-symbol-to-underscored ()
  (reset-counts)
  (format t "~&--- Suite: symbol-to-underscored ---~%")

  (check= "hyphen to underscore"
          (symbol-to-underscored 'system-info)
          "system_info")

  (check= "no hyphen unchanged"
          (symbol-to-underscored 'foo)
          "foo")

  (check= "multiple hyphens"
          (symbol-to-underscored 'rss-monitor-start)
          "rss_monitor_start")

  (check= "uppercase symbol → lowercase"
          (symbol-to-underscored 'SYSTEM-INFO)
          "system_info")

  (check= "single char"
          (symbol-to-underscored 'a)
          "a")

  (print-summary "symbol-to-underscored"))

;;; --- Suite 3: make-json-schema ---

(defun test-make-json-schema ()
  (reset-counts)
  (format t "~&--- Suite: make-json-schema ---~%")

  (let ((schema (make-json-schema :type "object" :description "A test schema")))
    (check "returns hash-table"
           (hash-table-p schema))
    (check= "type key"
            (gethash "type" schema)
            "object")
    (check= "description key"
            (gethash "description" schema)
            "A test schema"))

  ;; hyphen in keyword → underscore in string key
  (let ((schema (make-json-schema :max-tokens 100)))
    (check= "hyphen → underscore in key"
            (gethash "max_tokens" schema)
            100))

  ;; empty → empty hash-table
  (let ((schema (make-json-schema)))
    (check "empty schema is hash-table"
           (hash-table-p schema))
    (check= "empty schema has 0 keys"
            (hash-table-count schema)
            0))

  (print-summary "make-json-schema"))

;;; --- Top-level runner ---

(defun run-all ()
  "Run all tools helper test suites. Returns T if all passed."
  (format t "~%=== Tools Helper Tests ===~%")
  (test-hget)
  (test-symbol-to-underscored)
  (test-make-json-schema)
  (let ((total-pass *pass-count*)
        (total-fail *fail-count*))
    (format t "=== Total: ~D passed, ~D failed ===~%~%" total-pass total-fail)
    (zerop total-fail)))
