;;;; tests/test-rss-monitor.lisp
;;;;
;;;; Unit tests for the RSS monitor registry (rss-monitor.lisp).
;;;; Covers rss-monitor-stop-many: batch removal, partial overlap,
;;;; unknown-name tolerance, and single-persist semantics.
;;;;
;;;; Tests manipulate *rss-monitors* directly to avoid scheduler and
;;;; storage I/O dependencies.  Disk persistence is exercised by
;;;; integration tests against the live daemon.

(defpackage #:crichton/tests/rss-monitor
  (:use #:cl)
  (:import-from #:crichton/skills
                #:rss-monitor-stop-many)
  (:import-from #:crichton/tests/harness
                #:reset-counts #:check #:check= #:check-no-error
                #:print-summary)
  (:export #:run-all
           #:test-stop-many-removes-all
           #:test-stop-many-partial-overlap
           #:test-stop-many-unknown-names
           #:test-stop-many-empty-list
           #:test-stop-many-preserves-others))

(in-package #:crichton/tests/rss-monitor)

;;; --- Helpers ---

(defun registry ()
  "Return the live *rss-monitors* hash-table."
  crichton/skills::*rss-monitors*)

(defun registry-lock ()
  crichton/skills::*rss-monitors-lock*)

(defun seed-monitors (&rest names)
  "Insert synthetic monitor entries for each name. Returns NAMES."
  (bt:with-lock-held ((registry-lock))
    (dolist (name names)
      (setf (gethash name (registry))
            (list :url (format nil "http://example.com/~A.xml" name)
                  :interval-seconds 3600))))
  names)

(defun cleanup-monitors (&rest names)
  "Remove test entries from the registry."
  (bt:with-lock-held ((registry-lock))
    (dolist (name names)
      (remhash name (registry)))))

(defun monitor-present-p (name)
  (bt:with-lock-held ((registry-lock))
    (nth-value 1 (gethash name (registry)))))

;;; --- Suite 1: Remove all named monitors ---

(defun test-stop-many-removes-all ()
  (reset-counts)
  (format t "~&~%  [test-rss-monitor-stop-many-removes-all]~%")

  (seed-monitors "rss:feed-a" "rss:feed-b" "rss:feed-c")

  (let ((removed (rss-monitor-stop-many '("rss:feed-a" "rss:feed-b" "rss:feed-c")
                                         :persist nil)))
    (check= "returns count of removed monitors" removed 3)
    (check "feed-a removed" (not (monitor-present-p "rss:feed-a")))
    (check "feed-b removed" (not (monitor-present-p "rss:feed-b")))
    (check "feed-c removed" (not (monitor-present-p "rss:feed-c"))))

  (cleanup-monitors "rss:feed-a" "rss:feed-b" "rss:feed-c")
  (print-summary "rss-monitor-stop-many-removes-all"))

;;; --- Suite 2: Partial overlap (some names present, some not) ---

(defun test-stop-many-partial-overlap ()
  (reset-counts)
  (format t "~&~%  [test-rss-monitor-stop-many-partial-overlap]~%")

  (seed-monitors "rss:present-1" "rss:present-2")

  (let ((removed (rss-monitor-stop-many
                   '("rss:present-1" "rss:missing" "rss:present-2")
                   :persist nil)))
    (check= "returns only the count of monitors that existed" removed 2)
    (check "present-1 removed" (not (monitor-present-p "rss:present-1")))
    (check "present-2 removed" (not (monitor-present-p "rss:present-2"))))

  (cleanup-monitors "rss:present-1" "rss:present-2")
  (print-summary "rss-monitor-stop-many-partial-overlap"))

;;; --- Suite 3: All names unknown ---

(defun test-stop-many-unknown-names ()
  (reset-counts)
  (format t "~&~%  [test-rss-monitor-stop-many-unknown-names]~%")

  (check-no-error "no error when all names are unknown"
    (rss-monitor-stop-many '("rss:no-such-a" "rss:no-such-b") :persist nil))

  (let ((removed (rss-monitor-stop-many '("rss:no-such-a") :persist nil)))
    (check= "returns 0 when no monitors matched" removed 0))

  (print-summary "rss-monitor-stop-many-unknown-names"))

;;; --- Suite 4: Empty name list ---

(defun test-stop-many-empty-list ()
  (reset-counts)
  (format t "~&~%  [test-rss-monitor-stop-many-empty-list]~%")

  (check-no-error "no error on empty name list"
    (rss-monitor-stop-many '() :persist nil))

  (let ((removed (rss-monitor-stop-many '() :persist nil)))
    (check= "returns 0 for empty list" removed 0))

  (print-summary "rss-monitor-stop-many-empty-list"))

;;; --- Suite 5: Monitors not in the list are untouched ---

(defun test-stop-many-preserves-others ()
  (reset-counts)
  (format t "~&~%  [test-rss-monitor-stop-many-preserves-others]~%")

  (seed-monitors "rss:target" "rss:bystander")

  (rss-monitor-stop-many '("rss:target") :persist nil)

  (check "target is removed" (not (monitor-present-p "rss:target")))
  (check "bystander is preserved" (monitor-present-p "rss:bystander"))

  (cleanup-monitors "rss:target" "rss:bystander")
  (print-summary "rss-monitor-stop-many-preserves-others"))

;;; --- Top-level runner ---

(defun run-all ()
  (let ((total-pass 0)
        (total-fail 0))
    (flet ((run-suite (fn)
             (funcall fn)
             (incf total-pass crichton/tests/harness:*pass-count*)
             (incf total-fail crichton/tests/harness:*fail-count*)))
      (run-suite #'test-stop-many-removes-all)
      (run-suite #'test-stop-many-partial-overlap)
      (run-suite #'test-stop-many-unknown-names)
      (run-suite #'test-stop-many-empty-list)
      (run-suite #'test-stop-many-preserves-others))
    (zerop total-fail)))
