;;;; tests/test-config.lisp
;;;;
;;;; Unit tests for config/loader.lisp and config/util.lisp pure functions.
;;;; Tests deep-merge-plist, TOML key conversion, and JSON codec roundtrip.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/harness.lisp")
;;;;   (load "tests/test-config.lisp")
;;;;   (crichton/tests/config:run-all)
;;;;
;;;; Individual suites:
;;;;   (crichton/tests/config:test-deep-merge)
;;;;   (crichton/tests/config:test-toml-keys)
;;;;   (crichton/tests/config:test-json-codec)
;;;;   (crichton/tests/config:test-safe-intern)
;;;;   (crichton/tests/config:test-plistp)

(defpackage #:crichton/tests/config
  (:use #:cl #:crichton/tests/harness)
  (:import-from #:crichton/config
                #:deep-merge-plist
                #:plistp
                #:toml-key-to-keyword
                #:toml-value-to-lisp
                #:safe-intern-keyword
                #:plist-to-json-bytes
                #:json-bytes-to-plist)
  (:export #:run-all
           #:test-deep-merge
           #:test-toml-keys
           #:test-json-codec
           #:test-safe-intern
           #:test-plistp))

(in-package #:crichton/tests/config)

;;; --- Suite 1: deep-merge-plist ---

(defun test-deep-merge ()
  (reset-counts)
  (format t "~&--- Suite: deep-merge-plist ---~%")

  ;; nested key survives when override has a partial nested plist
  (let* ((defaults '(:daemon (:port 4005 :pid "d.pid")))
         (overrides '(:daemon (:port 9000)))
         (result   (deep-merge-plist defaults overrides))
         (daemon   (getf result :daemon)))
    (check= "nested override: port updated"
            (getf daemon :port)
            9000)
    (check= "nested override: pid preserved"
            (getf daemon :pid)
            "d.pid"))

  ;; non-plist value replaces entirely
  (let* ((defaults '(:name "alice"))
         (overrides '(:name "bob"))
         (result   (deep-merge-plist defaults overrides)))
    (check= "scalar value replaced"
            (getf result :name)
            "bob"))

  ;; empty overrides leaves defaults unchanged
  (let* ((defaults '(:port 4005 :host "localhost"))
         (result   (deep-merge-plist defaults nil)))
    (check= "empty override preserves port"
            (getf result :port)
            4005)
    (check= "empty override preserves host"
            (getf result :host)
            "localhost"))

  ;; new key in overrides is added
  (let* ((defaults  '(:port 4005))
         (overrides '(:port 4005 :debug t))
         (result    (deep-merge-plist defaults overrides)))
    (check= "new key added from override"
            (getf result :debug)
            t))

  ;; deeply nested merge
  (let* ((defaults  '(:a (:b (:c 1 :d 2))))
         (overrides '(:a (:b (:c 99))))
         (result    (deep-merge-plist defaults overrides))
         (b         (getf (getf result :a) :b)))
    (check= "deep nested override: c updated"
            (getf b :c)
            99)
    (check= "deep nested override: d preserved"
            (getf b :d)
            2))

  (print-summary "deep-merge-plist"))

;;; --- Suite 2: toml-key-to-keyword ---

(defun test-toml-keys ()
  (reset-counts)
  (format t "~&--- Suite: toml-key-to-keyword ---~%")

  (check= "underscore to hyphen"
          (toml-key-to-keyword "foo_bar")
          :foo-bar)

  (check= "already hyphenated"
          (toml-key-to-keyword "already-hyphenated")
          :already-hyphenated)

  (check= "simple lowercase"
          (toml-key-to-keyword "port")
          :port)

  (check= "uppercase input"
          (toml-key-to-keyword "MAX_TOKENS")
          :max-tokens)

  (check= "mixed underscores"
          (toml-key-to-keyword "api_key_file")
          :api-key-file)

  ;; toml-value-to-lisp: boolean normalization
  (check= "cl-toml:true → T"
          (toml-value-to-lisp 'cl-toml:true)
          t)

  (check= "cl-toml:false → NIL"
          (toml-value-to-lisp 'cl-toml:false)
          nil)

  (check= "integer pass-through"
          (toml-value-to-lisp 42)
          42)

  (check= "string pass-through"
          (toml-value-to-lisp "hello")
          "hello")

  (print-summary "toml-key-to-keyword"))

;;; --- Suite 3: JSON codec roundtrip ---

(defun test-json-codec ()
  (reset-counts)
  (format t "~&--- Suite: plist-to-json-bytes / json-bytes-to-plist ---~%")

  (let* ((original '(:name "alice" :count 42))
         (encoded  (plist-to-json-bytes original))
         (decoded  (json-bytes-to-plist encoded)))
    (check "bytes returned"
           (typep encoded '(vector (unsigned-byte 8))))
    (check= "name roundtrip"
            (getf decoded :name)
            "alice")
    (check= "count roundtrip"
            (getf decoded :count)
            42))

  ;; empty plist
  (let* ((encoded (plist-to-json-bytes nil))
         (decoded (json-bytes-to-plist encoded)))
    (check "empty plist encodes to bytes"
           (typep encoded '(vector (unsigned-byte 8))))
    (check "empty plist decodes to nil or empty"
           (null decoded)))

  ;; unicode values survive the roundtrip
  (let* ((original '(:text "日本語"))
         (encoded  (plist-to-json-bytes original))
         (decoded  (json-bytes-to-plist encoded)))
    (check= "unicode roundtrip"
            (getf decoded :text)
            "日本語"))

  (print-summary "plist-to-json-bytes / json-bytes-to-plist"))

;;; --- Suite 4: safe-intern-keyword ---

(defun test-safe-intern ()
  (reset-counts)
  (format t "~&--- Suite: safe-intern-keyword ---~%")

  (check= "normal key"
          (safe-intern-keyword "foo")
          :foo)

  (check= "hyphen allowed"
          (safe-intern-keyword "foo-bar")
          :foo-bar)

  (check= "underscore allowed (preserved, no translation)"
          (safe-intern-keyword "foo_bar")
          :foo_bar)

  ;; 64-char key should pass
  (let ((key64 (make-string 64 :initial-element #\a)))
    (check-no-error "64-char key accepted"
      (safe-intern-keyword key64)))

  ;; 65-char key should error
  (let ((key65 (make-string 65 :initial-element #\a)))
    (check-error "65-char key rejected"
      (safe-intern-keyword key65)))

  (check-error "empty string rejected"
    (safe-intern-keyword ""))

  (check-error "exclamation mark rejected"
    (safe-intern-keyword "foo!bar"))

  (check-error "slash rejected"
    (safe-intern-keyword "foo/bar"))

  (check-error "space rejected"
    (safe-intern-keyword "foo bar"))

  (print-summary "safe-intern-keyword"))

;;; --- Suite 5: plistp predicate ---

(defun test-plistp ()
  (reset-counts)
  (format t "~&--- Suite: plistp ---~%")

  (check "valid plist"
         (plistp '(:a 1 :b 2)))

  (check "empty list is valid plist"
         (plistp nil))

  (check "single pair"
         (plistp '(:x "hello")))

  (check "not a list → nil"
         (not (plistp 42)))

  (check "odd-length list → nil"
         (not (plistp '(:a 1 :b))))

  (check "non-keyword key → nil"
         (not (plistp '("a" 1))))

  (check "regular list → nil"
         (not (plistp '(1 2 3 4))))

  (print-summary "plistp"))

;;; --- Top-level runner ---

(defun run-all ()
  "Run all config test suites. Returns T if all passed."
  (format t "~%=== Config Tests ===~%")
  (test-deep-merge)
  (test-toml-keys)
  (test-json-codec)
  (test-safe-intern)
  (test-plistp)
  (let ((total-pass *pass-count*)
        (total-fail *fail-count*))
    (format t "=== Total: ~D passed, ~D failed ===~%~%" total-pass total-fail)
    (zerop total-fail)))
