;;;; tests/test-rss-filter.lisp
;;;;
;;;; Lisp-side integration test for the rss-filter WASM skill.
;;;; Verifies that run-rss-filter correctly invokes the skill and returns
;;;; matched items with the right matched_keywords field.
;;;;
;;;; Usage (from SLIME/SLY):
;;;;   (load "tests/test-rss-filter.lisp")
;;;;   (in-package #:crichton/tests)
;;;;   (test-rss-filter)

(defpackage #:crichton/tests
  (:use #:cl)
  (:import-from #:crichton/skills
                #:run-rss-filter)
  (:export #:test-rss-filter))

(in-package #:crichton/tests)

(defun test-rss-filter ()
  "Integration test for the rss-filter WASM skill.

Creates 3 test items (Carney news, weather, interest rates), runs the
rss-filter WASM skill with keywords [\"Carney\" \"Bank of Canada\"], and
verifies that exactly 2 items match with the correct matched_keywords.

Returns T on success, NIL on failure."
  (format t "~&=== RSS Filter Integration Test ===~%~%")

  (let* ((items (list
                 (list :id "item-1"
                       :title "Mark Carney wins Liberal leadership race"
                       :description "Carney takes over as Liberal party leader."
                       :link "https://example.com/1"
                       :published "Sat, 08 Mar 2025 12:00:00 +0000"
                       :feed-name "CBC News")
                 (list :id "item-2"
                       :title "Weekend weather: sunny skies ahead"
                       :description "Temperatures expected to rise this weekend."
                       :link "https://example.com/2"
                       :published "Sat, 08 Mar 2025 08:00:00 +0000"
                       :feed-name "Weather Network")
                 (list :id "item-3"
                       :title "Bank of Canada holds interest rates steady"
                       :description "The central bank kept the overnight rate unchanged."
                       :link "https://example.com/3"
                       :published "Wed, 05 Mar 2025 15:00:00 +0000"
                       :feed-name "Financial Post")))
         (keywords '("Carney" "Bank of Canada"))
         (result (run-rss-filter items keywords :match-mode "any"))
         (ok t))

    (format t "1. Checking skill invocation...~%")
    (unless result
      (format t "   FAIL: run-rss-filter returned NIL (skill not loaded?)~%")
      (return-from test-rss-filter nil))
    (format t "   OK: got result hash-table~%~%")

    (let ((matches (coerce (gethash "matches" result) 'list))
          (stats (gethash "statistics" result)))

      (format t "2. Checking statistics...~%")
      (let ((scanned (gethash "items_scanned" stats))
            (matched (gethash "items_matched" stats)))
        (format t "   items_scanned=~A, items_matched=~A~%" scanned matched)
        (unless (= scanned 3)
          (format t "   FAIL: expected items_scanned=3, got ~A~%" scanned)
          (setf ok nil))
        (unless (= matched 2)
          (format t "   FAIL: expected items_matched=2, got ~A~%" matched)
          (setf ok nil))
        (when (and (= scanned 3) (= matched 2))
          (format t "   OK~%")))
      (format t "~%")

      (format t "3. Checking matched items...~%")
      (unless (= (length matches) 2)
        (format t "   FAIL: expected 2 matches, got ~D~%" (length matches))
        (setf ok nil))

      ;; Verify item-1 (Carney) matched with keyword "Carney"
      (let ((m1 (find "item-1" matches :key (lambda (m) (gethash "id" m)) :test #'string=)))
        (cond
          ((null m1)
           (format t "   FAIL: item-1 (Carney news) not in matches~%")
           (setf ok nil))
          (t
           (let ((kws (coerce (gethash "matched_keywords" m1) 'list)))
             (format t "   item-1 matched_keywords: ~{~S~^, ~}~%" kws)
             (unless (member "Carney" kws :test #'string=)
               (format t "   FAIL: expected \"Carney\" in matched_keywords~%")
               (setf ok nil))))))

      ;; Verify item-3 (Bank of Canada) matched with keyword "Bank of Canada"
      (let ((m3 (find "item-3" matches :key (lambda (m) (gethash "id" m)) :test #'string=)))
        (cond
          ((null m3)
           (format t "   FAIL: item-3 (Bank of Canada) not in matches~%")
           (setf ok nil))
          (t
           (let ((kws (coerce (gethash "matched_keywords" m3) 'list)))
             (format t "   item-3 matched_keywords: ~{~S~^, ~}~%" kws)
             (unless (member "Bank of Canada" kws :test #'string=)
               (format t "   FAIL: expected \"Bank of Canada\" in matched_keywords~%")
               (setf ok nil))))))

      ;; Verify item-2 (weather) did NOT match
      (let ((m2 (find "item-2" matches :key (lambda (m) (gethash "id" m)) :test #'string=)))
        (when m2
          (format t "   FAIL: item-2 (weather) should not have matched~%")
          (setf ok nil)))

      (when ok
        (format t "   OK: both matches correct~%"))
      (format t "~%"))

    (if ok
        (format t "=== ALL TESTS PASSED ===~%")
        (format t "=== SOME TESTS FAILED ===~%"))
    ok))
