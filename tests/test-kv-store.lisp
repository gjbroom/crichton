;;;; tests/test-kv-store.lisp
;;;;
;;;; Validation tests for the age-encrypted KV store backend.
;;;; Covers CRUD, quota enforcement, cache management, admin API,
;;;; backup/restore, integrity check, repair, and health check.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/test-kv-store.lisp")
;;;;   (crichton/tests/kv:run-all)
;;;;
;;;; Individual suites:
;;;;   (crichton/tests/kv:test-basic-crud)
;;;;   (crichton/tests/kv:test-prefix-list)
;;;;   (crichton/tests/kv:test-quota-enforcement)
;;;;   (crichton/tests/kv:test-cache-management)
;;;;   (crichton/tests/kv:test-admin-api)
;;;;   (crichton/tests/kv:test-backup-restore)
;;;;   (crichton/tests/kv:test-integrity-check)
;;;;   (crichton/tests/kv:test-repair)
;;;;   (crichton/tests/kv:test-health-check)

(defpackage #:crichton/tests/kv
  (:use #:cl)
  (:import-from #:crichton/skills
                #:kv-get
                #:kv-set
                #:kv-delete
                #:kv-exists-p
                #:kv-list
                #:kv-clear-skill
                #:kv-skill-usage
                #:kv-global-usage
                #:preload-kv-cache
                #:flush-all-kv
                #:clear-kv-cache
                #:kv-usage-report
                #:kv-quota-exceeded
                #:kv-dir-path
                #:backup-kv-store
                #:restore-kv-backup
                #:check-kv-integrity
                #:repair-corrupt-kv
                #:kv-health-check)
  (:export #:run-all
           #:test-basic-crud
           #:test-prefix-list
           #:test-quota-enforcement
           #:test-cache-management
           #:test-admin-api
           #:test-backup-restore
           #:test-integrity-check
           #:test-repair
           #:test-health-check))

(in-package #:crichton/tests/kv)

;;; --- Test harness ---

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun %reset-counts ()
  (setf *pass-count* 0 *fail-count* 0))

(defun %pass (label)
  (incf *pass-count*)
  (format t "  PASS  ~A~%" label))

(defun %fail (label message)
  (incf *fail-count*)
  (format t "  FAIL  ~A — ~A~%" label message))

(defmacro check (label condition &optional message)
  `(if ,condition
       (%pass ,label)
       (%fail ,label (or ,message (format nil "expected truthy, got ~S" ,condition)))))

(defmacro check= (label got expected)
  `(let ((g ,got) (e ,expected))
     (if (equal g e)
         (%pass ,label)
         (%fail ,label (format nil "expected ~S, got ~S" e g)))))

(defmacro check-signals (label condition-type &body body)
  `(let ((signaled nil))
     (handler-case (progn ,@body)
       (,condition-type () (setf signaled t)))
     (if signaled
         (%pass ,label)
         (%fail ,label (format nil "expected ~S to be signaled" ',condition-type)))))

(defun %print-summary (suite-name)
  (format t "  → ~A: ~D passed, ~D failed~%~%" suite-name *pass-count* *fail-count*))

;;; --- Test skill IDs (isolated from real skills) ---

(defparameter +test-skill+ "crichton.test.kv.primary")
(defparameter +test-skill-b+ "crichton.test.kv.secondary")

(defun %cleanup ()
  "Remove test skill data and clear cache between suites."
  (ignore-errors (kv-clear-skill +test-skill+))
  (ignore-errors (kv-clear-skill +test-skill-b+)))

;;; --- Suite 1: Basic CRUD ---

(defun test-basic-crud ()
  "Test set/get/delete/exists-p."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: basic CRUD ---~%")

  ;; get on missing key returns nil
  (check= "get missing key" (kv-get +test-skill+ "nope") nil)

  ;; exists-p on missing key returns nil/false
  (check "exists-p missing key" (not (kv-exists-p +test-skill+ "nope")))

  ;; set and get roundtrip
  (kv-set +test-skill+ "greeting" "hello")
  (check= "get after set" (kv-get +test-skill+ "greeting") "hello")

  ;; exists-p after set
  (check "exists-p after set" (kv-exists-p +test-skill+ "greeting"))

  ;; overwrite value
  (kv-set +test-skill+ "greeting" "world")
  (check= "get after overwrite" (kv-get +test-skill+ "greeting") "world")

  ;; delete existing key returns T
  (check= "delete returns T" (kv-delete +test-skill+ "greeting") t)
  (check= "get after delete" (kv-get +test-skill+ "greeting") nil)
  (check "exists-p after delete" (not (kv-exists-p +test-skill+ "greeting")))

  ;; delete non-existent key returns NIL
  (check= "delete missing returns nil" (kv-delete +test-skill+ "greeting") nil)

  ;; unicode values round-trip
  (kv-set +test-skill+ "unicode" "日本語テスト")
  (check= "unicode roundtrip" (kv-get +test-skill+ "unicode") "日本語テスト")

  ;; skill isolation: key in one skill not visible in another
  (kv-set +test-skill+ "shared-key" "from-primary")
  (check= "skill isolation" (kv-get +test-skill-b+ "shared-key") nil)

  (%cleanup)
  (%print-summary "basic CRUD"))

;;; --- Suite 2: List and prefix filtering ---

(defun test-prefix-list ()
  "Test kv-list with and without prefix."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: prefix list ---~%")

  (kv-set +test-skill+ "alpha:a" "1")
  (kv-set +test-skill+ "alpha:b" "2")
  (kv-set +test-skill+ "beta:x"  "3")
  (kv-set +test-skill+ "beta:y"  "4")
  (kv-set +test-skill+ "gamma"   "5")

  ;; list all keys, sorted
  (let ((all (kv-list +test-skill+)))
    (check= "all keys count" (length all) 5)
    (check= "all keys sorted" all
            (list "alpha:a" "alpha:b" "beta:x" "beta:y" "gamma")))

  ;; list with prefix
  (let ((alpha (kv-list +test-skill+ "alpha:")))
    (check= "alpha: prefix count" (length alpha) 2)
    (check= "alpha: prefix keys" alpha (list "alpha:a" "alpha:b")))

  (let ((beta (kv-list +test-skill+ "beta:")))
    (check= "beta: prefix count" (length beta) 2))

  ;; exact match prefix
  (let ((exact (kv-list +test-skill+ "gamma")))
    (check= "exact prefix" exact (list "gamma")))

  ;; prefix with no matches
  (let ((none (kv-list +test-skill+ "zzz:")))
    (check= "no-match prefix" none nil))

  ;; empty skill returns empty list
  (let ((empty (kv-list +test-skill-b+)))
    (check= "empty skill list" empty nil))

  (%cleanup)
  (%print-summary "prefix list"))

;;; --- Suite 3: Quota enforcement ---

(defun test-quota-enforcement ()
  "Test that quota violations signal KV-QUOTA-EXCEEDED."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: quota enforcement ---~%")

  ;; max-value-bytes: default 10240. A value of 10241 bytes should fail.
  (let ((big-value (make-string 10241 :initial-element #\x)))
    (check-signals "max-value-bytes exceeded" kv-quota-exceeded
      (kv-set +test-skill+ "big" big-value)))

  ;; Value at exactly max-value-bytes boundary should succeed.
  (let ((ok-value (make-string 10240 :initial-element #\x)))
    (handler-case
        (progn
          (kv-set +test-skill+ "maxval" ok-value)
          (%pass "max-value-bytes boundary accepted"))
      (kv-quota-exceeded (c)
        (%fail "max-value-bytes boundary accepted"
               (format nil "unexpected quota error: ~A" c)))))

  ;; After the boundary write, the store has data. Clean for next tests.
  (%cleanup)

  ;; max-keys: default 100. Fill 100 keys, then adding a 101st should fail.
  (dotimes (i 100)
    (kv-set +test-skill+ (format nil "k~D" i) "v"))
  (check-signals "max-keys exceeded" kv-quota-exceeded
    (kv-set +test-skill+ "overflow-key" "v"))
  ;; Overwriting an existing key must not be blocked by max-keys.
  (handler-case
      (progn
        (kv-set +test-skill+ "k0" "new-value")
        (%pass "overwrite existing key at max-keys boundary"))
    (kv-quota-exceeded (c)
      (%fail "overwrite existing key at max-keys boundary"
             (format nil "unexpected quota error: ~A" c))))

  (%cleanup)
  (%print-summary "quota enforcement"))

;;; --- Suite 4: Cache management ---

(defun test-cache-management ()
  "Test that clear-kv-cache forces a disk reload."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: cache management ---~%")

  (kv-set +test-skill+ "persistent" "yes")

  ;; Clear only the in-memory cache — file still on disk.
  (clear-kv-cache)

  ;; After cache clear, get must reload from disk and still find the value.
  (check= "reload from disk after clear-kv-cache"
          (kv-get +test-skill+ "persistent") "yes")

  ;; flush-all-kv: ensure we can flush without error when cache is populated.
  (kv-set +test-skill+ "another" "value")
  (let ((flushed (flush-all-kv)))
    (check "flush-all-kv returns positive count" (>= flushed 1)))

  ;; preload-kv-cache: repopulate cache from disk files.
  (clear-kv-cache)
  (let ((loaded (preload-kv-cache)))
    (check "preload-kv-cache returns non-negative count" (>= loaded 0)))

  ;; After preload, value is still accessible.
  (check= "value accessible after preload"
          (kv-get +test-skill+ "persistent") "yes")

  (%cleanup)
  (%print-summary "cache management"))

;;; --- Suite 5: Admin API ---

(defun test-admin-api ()
  "Test kv-skill-usage, kv-global-usage, kv-clear-skill."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: admin API ---~%")

  (kv-set +test-skill+ "a" "hello")
  (kv-set +test-skill+ "b" "world")
  (kv-set +test-skill-b+ "x" "foo")

  ;; kv-skill-usage structure
  (let ((usage (kv-skill-usage +test-skill+)))
    (check= "skill-usage key-count" (getf usage :key-count) 2)
    (check "skill-usage total-bytes positive" (> (getf usage :total-bytes) 0))
    (check= "skill-usage skill-id" (getf usage :skill-id) +test-skill+))

  ;; kv-global-usage lists all loaded skills
  (let ((global (kv-global-usage)))
    (check "global-usage non-empty" (>= (length global) 2))
    (let ((ids (mapcar (lambda (u) (getf u :skill-id)) global)))
      (check "global-usage includes primary" (member +test-skill+ ids :test #'string=))
      (check "global-usage includes secondary" (member +test-skill-b+ ids :test #'string=))))

  ;; kv-clear-skill removes data
  (kv-clear-skill +test-skill+)
  (check= "cleared skill key gone" (kv-get +test-skill+ "a") nil)
  (let ((usage-after (kv-skill-usage +test-skill+)))
    (check= "cleared skill key-count" (getf usage-after :key-count) 0))

  ;; secondary skill unaffected by clearing primary
  (check= "secondary skill intact" (kv-get +test-skill-b+ "x") "foo")

  (%cleanup)
  (%print-summary "admin API"))

;;; --- Suite 6: Backup and restore ---

(defun %backup-dir ()
  (merge-pathnames "kv-test-backup/" crichton/config:*agent-home*))

(defun %cleanup-backup ()
  (let ((bdir (%backup-dir)))
    (when (probe-file bdir)
      (dolist (f (directory (merge-pathnames (make-pathname :name :wild :type "age")
                                             bdir)))
        (delete-file f))
      (ignore-errors (uiop:delete-empty-directory bdir)))))

(defun test-backup-restore ()
  "Test backup-kv-store / restore-kv-backup roundtrip."
  (%reset-counts)
  (%cleanup)
  (%cleanup-backup)
  (format t "~&--- Suite: backup/restore ---~%")

  ;; Populate test skill.
  (kv-set +test-skill+ "key1" "value1")
  (kv-set +test-skill+ "key2" "value2")

  ;; Backup.
  (let ((backed-up (backup-kv-store (%backup-dir))))
    (check "backup-kv-store returns non-negative count" (>= backed-up 0))
    ;; At least the primary test skill should be among backed-up files.
    (let ((backup-file (merge-pathnames
                        (make-pathname :name +test-skill+ :type "age")
                        (%backup-dir))))
      (check "backup file exists on disk" (probe-file backup-file))))

  ;; Wipe the live skill.
  (kv-clear-skill +test-skill+)
  (check= "data gone before restore" (kv-get +test-skill+ "key1") nil)

  ;; Restore from backup.
  (let ((restored (restore-kv-backup (%backup-dir))))
    (check "restore-kv-backup returns positive count" (>= restored 1)))

  ;; Data should be accessible again.
  (check= "key1 restored" (kv-get +test-skill+ "key1") "value1")
  (check= "key2 restored" (kv-get +test-skill+ "key2") "value2")

  (%cleanup)
  (%cleanup-backup)
  (%print-summary "backup/restore"))

;;; --- Suite 7: Integrity check ---

(defun test-integrity-check ()
  "Test check-kv-integrity reports :ok for good stores and :corrupt for bad ones."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: integrity check ---~%")

  ;; Write a good store.
  (kv-set +test-skill+ "integrity-key" "ok")

  ;; check-kv-integrity should return at least one :ok entry for our skill.
  (let* ((results (check-kv-integrity))
         (our-result (find +test-skill+ results :key (lambda (r) (getf r :skill-id))
                           :test #'string=)))
    (check "integrity: found our skill in results" our-result)
    (when our-result
      (check= "integrity: our skill is :ok" (getf our-result :status) :ok)
      (check= "integrity: no error on :ok" (getf our-result :error) nil)))

  ;; Write a garbage .age file to simulate corruption.
  (let* ((corrupt-skill "crichton.test.kv.corrupt")
         (corrupt-path (crichton/skills::kv-file-path corrupt-skill)))
    (ensure-directories-exist corrupt-path)
    (with-open-file (out corrupt-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (let ((garbage (sb-ext:string-to-octets "this is not a valid age file" :external-format :utf-8)))
        (write-sequence garbage out)))

    (let* ((results (check-kv-integrity))
           (bad-result (find corrupt-skill results
                             :key (lambda (r) (getf r :skill-id))
                             :test #'string=)))
      (check "integrity: corrupt skill found" bad-result)
      (when bad-result
        (check= "integrity: corrupt skill status" (getf bad-result :status) :corrupt)
        (check "integrity: corrupt skill has error string" (getf bad-result :error))))

    ;; Clean up the garbage file.
    (ignore-errors (crichton/config:delete-file-if-exists corrupt-path)))

  (%cleanup)
  (%print-summary "integrity check"))

;;; --- Suite 8: Repair corrupt KV ---

(defun test-repair ()
  "Test repair-corrupt-kv renames the corrupt file and evicts the cache entry."
  (%reset-counts)
  (format t "~&--- Suite: repair corrupt KV ---~%")

  (let* ((repair-skill "crichton.test.kv.repair")
         (live-path (crichton/skills::kv-file-path repair-skill))
         (corrupt-ext-path (make-pathname :name (format nil "~A.corrupt" repair-skill)
                                          :type "age"
                                          :defaults live-path)))
    ;; Create a garbage .age file.
    (ensure-directories-exist live-path)
    (with-open-file (out live-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (write-sequence (sb-ext:string-to-octets "garbage" :external-format :utf-8) out))

    ;; Set a cache entry so we can verify it gets evicted.
    (setf (gethash repair-skill crichton/skills::*kv-cache*)
          (make-hash-table :test #'equal))

    ;; Repair should rename and return the new path.
    (let ((result (repair-corrupt-kv repair-skill)))
      (check "repair returns a path" result)
      (when result
        (check= "repair: live file gone" (probe-file live-path) nil)
        (check "repair: renamed file exists"
               (probe-file corrupt-ext-path))))

    ;; Cache entry should be evicted.
    (check= "repair: cache evicted"
            (gethash repair-skill crichton/skills::*kv-cache*) nil)

    ;; Repair on non-existent file returns NIL.
    (check= "repair: missing file returns nil"
            (repair-corrupt-kv repair-skill) nil)

    ;; Clean up.
    (ignore-errors (delete-file corrupt-ext-path)))

  (%print-summary "repair corrupt KV"))

;;; --- Suite 9: Health check ---

(defun test-health-check ()
  "Test that kv-health-check runs without error and returns a boolean."
  (%reset-counts)
  (%cleanup)
  (format t "~&--- Suite: health check ---~%")

  (kv-set +test-skill+ "hc-key" "hc-value")

  (let* ((output (make-string-output-stream))
         (result (kv-health-check :stream output))
         (text   (get-output-stream-string output)))
    (check "health-check returns T (no corruption)" (eq result t))
    (check "health-check output non-empty" (> (length text) 0))
    (check "health-check mentions directory"
           (search "Directory" text))
    (check "health-check mentions quotas"
           (search "Quota" text)))

  (%cleanup)
  (%print-summary "health check"))

;;; --- Top-level runner ---

(defun run-all ()
  "Run all KV store test suites and print a summary."
  (let ((total-pass 0)
        (total-fail 0))
    (flet ((run-suite (fn)
             (%reset-counts)
             (funcall fn)
             (incf total-pass *pass-count*)
             (incf total-fail *fail-count*)))
      (format t "~&=== KV Store Tests ===~%~%")
      (run-suite #'test-basic-crud)
      (run-suite #'test-prefix-list)
      (run-suite #'test-quota-enforcement)
      (run-suite #'test-cache-management)
      (run-suite #'test-admin-api)
      (run-suite #'test-backup-restore)
      (run-suite #'test-integrity-check)
      (run-suite #'test-repair)
      (run-suite #'test-health-check)
      (format t "=== TOTAL: ~D passed, ~D failed ===~%" total-pass total-fail)
      (zerop total-fail))))
