;;;; tests/test-storage.lisp
;;;;
;;;; Unit tests for storage/store.lisp.
;;;; Covers the in-memory cache API (CRUD, list, batch, dirty tracking,
;;;; namespace isolation, JSON serialization, usage reporting, clear).
;;;;
;;;; Disk persistence (flush/load roundtrip) requires encryption to be
;;;; configured; that path is exercised by integration tests that run
;;;; against a live daemon with an age key.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/test-storage.lisp")
;;;;   (crichton/tests/storage:run-all)

(defpackage #:crichton/tests/storage
  (:use #:cl)
  (:import-from #:crichton/storage
                #:store-get
                #:store-set
                #:store-delete
                #:store-exists-p
                #:store-list-keys
                #:store-list-namespaces
                #:store-get-all
                #:store-set-all
                #:store-clear-namespace
                #:clear-storage-cache
                #:storage-usage)
  (:import-from #:crichton/tests/harness
                #:reset-counts #:check #:check= #:check-no-error
                #:print-summary)
  (:export #:run-all
           #:test-basic-crud
           #:test-list-keys
           #:test-batch-ops
           #:test-dirty-tracking
           #:test-namespace-isolation
           #:test-json-serialization
           #:test-usage-reporting
           #:test-clear-namespace))

(in-package #:crichton/tests/storage)

(defparameter +ns+ "crichton.test.storage.primary"
  "Test namespace, isolated from production data.")

(defparameter +ns-b+ "crichton.test.storage.secondary")

(defun cleanup ()
  "Remove test namespaces from cache before/after each suite."
  (ignore-errors (store-clear-namespace +ns+))
  (ignore-errors (store-clear-namespace +ns-b+)))

;;; --- Suite 1: Basic CRUD ---

(defun test-basic-crud ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-basic-crud]~%")

  (check= "get missing key returns nil"
          (store-get +ns+ "no-such-key") nil)

  (check= "get missing key returns custom default"
          (store-get +ns+ "no-such-key" :missing) :missing)

  (check "exists-p missing key is false"
         (not (store-exists-p +ns+ "no-such-key")))

  (store-set +ns+ "greeting" "hello")
  (check= "get after set" (store-get +ns+ "greeting") "hello")
  (check "exists-p after set" (store-exists-p +ns+ "greeting"))

  (store-set +ns+ "greeting" "world")
  (check= "overwrite returns new value" (store-get +ns+ "greeting") "world")

  (check= "delete existing returns T" (store-delete +ns+ "greeting") t)
  (check= "get after delete returns nil" (store-get +ns+ "greeting") nil)
  (check "exists-p after delete is false" (not (store-exists-p +ns+ "greeting")))

  (check= "delete non-existent returns nil" (store-delete +ns+ "greeting") nil)

  (store-set +ns+ "unicode" "日本語テスト")
  (check= "unicode value roundtrip" (store-get +ns+ "unicode") "日本語テスト")

  (store-set +ns+ "num-val" 42)
  (check= "integer value roundtrip" (store-get +ns+ "num-val") 42)

  (cleanup)
  (print-summary "storage-basic-crud"))

;;; --- Suite 2: List keys with prefix filtering ---

(defun test-list-keys ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-list-keys]~%")

  (check= "list-keys on empty namespace" (store-list-keys +ns+) nil)

  (store-set +ns+ "alpha:a" "1")
  (store-set +ns+ "alpha:b" "2")
  (store-set +ns+ "beta:x"  "3")
  (store-set +ns+ "beta:y"  "4")
  (store-set +ns+ "gamma"   "5")

  (check= "list-keys no prefix returns all 5 (sorted)"
          (store-list-keys +ns+)
          '("alpha:a" "alpha:b" "beta:x" "beta:y" "gamma"))

  (check= "list-keys alpha: prefix"
          (store-list-keys +ns+ "alpha:")
          '("alpha:a" "alpha:b"))

  (check= "list-keys beta: prefix"
          (store-list-keys +ns+ "beta:")
          '("beta:x" "beta:y"))

  (check= "list-keys gamma prefix (exact match)"
          (store-list-keys +ns+ "gamma")
          '("gamma"))

  (check= "list-keys no-match prefix returns nil"
          (store-list-keys +ns+ "zzz:")
          nil)

  (cleanup)
  (print-summary "storage-list-keys"))

;;; --- Suite 3: Batch operations ---

(defun test-batch-ops ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-batch-ops]~%")

  (check= "get-all on empty namespace" (store-get-all +ns+) nil)

  (store-set +ns+ "k1" "v1")
  (store-set +ns+ "k2" "v2")
  (store-set +ns+ "k3" "v3")

  (let ((pairs (store-get-all +ns+)))
    (check= "get-all returns 3 pairs" (length pairs) 3)
    (check= "get-all sorted by key" (mapcar #'car pairs) '("k1" "k2" "k3"))
    (check= "get-all values match" (mapcar #'cdr pairs) '("v1" "v2" "v3")))

  (store-set-all +ns+ '(("a" . 10) ("b" . 20)))
  (check= "set-all replaces all data — count" (length (store-get-all +ns+)) 2)
  (check= "set-all a value" (store-get +ns+ "a") 10)
  (check= "set-all b value" (store-get +ns+ "b") 20)
  (check= "set-all old key k1 is gone" (store-get +ns+ "k1") nil)

  (store-set-all +ns+ nil)
  (check= "set-all nil clears namespace" (store-get-all +ns+) nil)

  (cleanup)
  (print-summary "storage-batch-ops"))

;;; --- Suite 4: Dirty tracking ---

(defun test-dirty-tracking ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-dirty-tracking]~%")

  ;; Accessing a namespace loads it (empty) — not dirty
  (store-get +ns+ "any")
  (let ((usage (storage-usage +ns+)))
    (check "freshly loaded namespace is not dirty"
           (not (getf usage :dirty-p))))

  ;; After a write, namespace becomes dirty
  (store-set +ns+ "x" "y")
  (let ((usage (storage-usage +ns+)))
    (check "namespace is dirty after store-set" (getf usage :dirty-p)))

  ;; After clear-storage-cache, dirty flag is also cleared
  (clear-storage-cache)
  (store-get +ns+ "any")
  (let ((usage (storage-usage +ns+)))
    (check "dirty flag cleared after cache wipe" (not (getf usage :dirty-p))))

  (cleanup)
  (print-summary "storage-dirty-tracking"))

;;; --- Suite 5: Namespace isolation ---

(defun test-namespace-isolation ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-namespace-isolation]~%")

  (store-set +ns+   "shared-key" "from-primary")
  (store-set +ns-b+ "shared-key" "from-secondary")

  (check= "primary ns has its own value"
          (store-get +ns+ "shared-key") "from-primary")
  (check= "secondary ns has its own value"
          (store-get +ns-b+ "shared-key") "from-secondary")

  (store-delete +ns+ "shared-key")
  (check= "delete in primary does not affect secondary"
          (store-get +ns-b+ "shared-key") "from-secondary")

  (let ((nss (store-list-namespaces)))
    (check "both namespaces visible in list-namespaces"
           (and (member +ns+ nss :test #'equal)
                (member +ns-b+ nss :test #'equal))))

  (cleanup)
  (print-summary "storage-namespace-isolation"))

;;; --- Suite 6: JSON serialization roundtrip ---

(defun test-json-serialization ()
  (reset-counts)
  (format t "~&~%  [test-storage-json-serialization]~%")

  (let* ((data-ht (make-hash-table :test #'equal)))
    (setf (gethash "alpha" data-ht) "one"
          (gethash "beta"  data-ht) 2
          (gethash "gamma" data-ht) t)

    (let* ((json (crichton/storage::storage-to-json-string "test-ns" data-ht)))
      (check "json-string is a non-empty string"
             (and (stringp json) (plusp (length json))))
      (check "json contains version field"
             (search "\"version\"" json))
      (check "json contains namespace field"
             (search "\"test-ns\"" json))

      (multiple-value-bind (parsed-ns parsed-ht)
          (crichton/storage::json-string-to-storage json)
        (check= "roundtrip: namespace preserved" parsed-ns "test-ns")
        (check= "roundtrip: string value" (gethash "alpha" parsed-ht) "one")
        (check= "roundtrip: integer value" (gethash "beta" parsed-ht) 2)
        (check "roundtrip: boolean value" (gethash "gamma" parsed-ht)))))

  ;; Corrupt JSON signals an error
  (check-no-error "empty data ht deserializes cleanly"
    (crichton/storage::json-string-to-storage
     "{\"version\":1,\"namespace\":\"x\",\"updated_at\":\"now\",\"data\":{}}"))

  (print-summary "storage-json-serialization"))

;;; --- Suite 7: Usage reporting ---

(defun test-usage-reporting ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-usage-reporting]~%")

  ;; Touch namespace to load it
  (store-get +ns+ "any")
  (let ((usage (storage-usage +ns+)))
    (check "usage plist has :namespace" (getf usage :namespace))
    (check= "usage namespace value" (getf usage :namespace) +ns+)
    (check= "usage key-count starts at 0" (getf usage :key-count) 0))

  (store-set +ns+ "a" 1)
  (store-set +ns+ "b" 2)
  (let ((usage (storage-usage +ns+)))
    (check= "usage key-count reflects writes" (getf usage :key-count) 2))

  (store-delete +ns+ "a")
  (let ((usage (storage-usage +ns+)))
    (check= "usage key-count decrements after delete" (getf usage :key-count) 1))

  (let ((usage (storage-usage +ns+)))
    (check "usage size-bytes is a positive integer"
           (and (integerp (getf usage :size-bytes))
                (plusp (getf usage :size-bytes)))))

  (cleanup)
  (print-summary "storage-usage-reporting"))

;;; --- Suite 8: Clear namespace ---

(defun test-clear-namespace ()
  (reset-counts)
  (cleanup)
  (format t "~&~%  [test-storage-clear-namespace]~%")

  (store-set +ns+ "x" "y")
  (store-set +ns+ "p" "q")
  (check= "namespace has 2 keys before clear"
          (length (store-list-keys +ns+)) 2)

  (check-no-error "clear-namespace does not error"
    (store-clear-namespace +ns+))

  ;; After clear, namespace is gone from cache; re-accessing produces empty ns
  (check= "get after clear returns nil" (store-get +ns+ "x") nil)
  (check= "list-keys after clear is empty" (store-list-keys +ns+) nil)

  (cleanup)
  (print-summary "storage-clear-namespace"))

;;; --- Top-level runner ---

(defun run-all ()
  (let ((total-pass 0)
        (total-fail 0))
    (flet ((run-suite (fn)
             (funcall fn)
             (incf total-pass crichton/tests/harness:*pass-count*)
             (incf total-fail crichton/tests/harness:*fail-count*)))
      (run-suite #'test-basic-crud)
      (run-suite #'test-list-keys)
      (run-suite #'test-batch-ops)
      (run-suite #'test-dirty-tracking)
      (run-suite #'test-namespace-isolation)
      (run-suite #'test-json-serialization)
      (run-suite #'test-usage-reporting)
      (run-suite #'test-clear-namespace))
    (zerop total-fail)))
