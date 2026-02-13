;;;; tests/test-rss-persistence.lisp
;;;;
;;;; Manual test for RSS persistence functionality.
;;;; This script verifies that RSS seen-item state persists correctly.
;;;;
;;;; Usage (from SLIME/SLY):
;;;;   (load "tests/test-rss-persistence.lisp")
;;;;   (in-package #:crichton/tests)
;;;;   (test-rss-persistence)

(defpackage #:crichton/tests
  (:use #:cl)
  (:import-from #:crichton/skills
                #:clear-all-rss-state
                #:rss-state-stats
                #:persist-rss-state
                #:mark-seen
                #:filter-new-items
                #:%make-feed-item)
  (:import-from #:crichton/storage
                #:store-get
                #:flush-all-storage)
  (:export #:test-rss-persistence))

(in-package #:crichton/tests)

(defun test-rss-persistence ()
  "Test RSS persistence functionality."
  (format t "~&=== RSS Persistence Test ===~%~%")

  ;; Step 1: Clear any existing state
  (format t "1. Clearing existing RSS state...~%")
  (clear-all-rss-state)
  (let ((stats (rss-state-stats)))
    (format t "   Stats: ~D feeds, ~D items~%~%"
            (getf stats :feed-count)
            (getf stats :total-seen-items)))

  ;; Step 2: Create some fake feed items
  (format t "2. Creating test feed items...~%")
  (let* ((test-url "https://example.com/feed.xml")
         (items (list
                 (%make-feed-item :title "Item 1" :guid "guid-1" :link "http://example.com/1")
                 (%make-feed-item :title "Item 2" :guid "guid-2" :link "http://example.com/2")
                 (%make-feed-item :title "Item 3" :guid "guid-3" :link "http://example.com/3"))))

    ;; Step 3: Mark items as seen
    (format t "3. Marking items as seen...~%")
    (mark-seen test-url items)
    (let ((stats (rss-state-stats)))
      (format t "   Stats: ~D feeds, ~D items~%"
              (getf stats :feed-count)
              (getf stats :total-seen-items)))

    ;; Step 4: Verify filtering works
    (format t "4. Testing filter (should show 0 new items)...~%")
    (let ((new-items (filter-new-items test-url items)))
      (format t "   New items: ~D (expected: 0)~%"
              (length new-items)))

    ;; Step 5: Test with one new item
    (format t "5. Testing with one new item...~%")
    (let* ((new-item (%make-feed-item :title "Item 4" :guid "guid-4" :link "http://example.com/4"))
           (all-items (append items (list new-item)))
           (new-items (filter-new-items test-url all-items)))
      (format t "   New items: ~D (expected: 1)~%"
              (length new-items))
      (when (= 1 (length new-items))
        (format t "   New item title: ~A~%"
                (crichton/skills::feed-item-title (first new-items)))))

    ;; Step 6: Explicitly persist
    (format t "6. Persisting state to storage...~%")
    (persist-rss-state)
    (flush-all-storage)

    ;; Step 7: Check raw storage
    (format t "7. Checking raw storage...~%")
    (let ((stored-data (store-get "rss" "seen-feeds")))
      (if stored-data
          (progn
            (format t "   Storage contains data: YES~%")
            (format t "   Type: ~A~%" (type-of stored-data))
            (when (hash-table-p stored-data)
              (format t "   Feeds in storage: ~D~%" (hash-table-count stored-data))))
          (format t "   Storage contains data: NO~%")))

    (format t "~%=== Test Complete ===~%")
    (format t "To fully test persistence, restart the daemon and verify~%")
    (format t "that rss-state-stats shows the same counts.~%")
    t))
