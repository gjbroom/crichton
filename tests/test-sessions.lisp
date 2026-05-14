;;;; tests/test-sessions.lisp
;;;;
;;;; Unit tests for sessions/store.lisp.
;;;; Covers in-memory session lifecycle (create, add-message, structure),
;;;; utility functions (generate-session-id, ensure-adjustable-vector,
;;;; unix-timestamp), and the JSON serialization roundtrip.
;;;;
;;;; Disk persistence (save-session/load-session) requires an age key and
;;;; is exercised separately against a live daemon.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/test-sessions.lisp")
;;;;   (crichton/tests/sessions:run-all)

(defpackage #:crichton/tests/sessions
  (:use #:cl)
  (:import-from #:crichton/sessions
                #:create-session
                #:add-message)
  (:import-from #:crichton/tests/harness
                #:reset-counts #:check #:check= #:check-no-error #:check-error
                #:print-summary)
  (:export #:run-all
           #:test-create-session
           #:test-add-message
           #:test-ensure-adjustable-vector
           #:test-generate-session-id
           #:test-unix-timestamp
           #:test-serialization-roundtrip))

(in-package #:crichton/tests/sessions)

;;; --- Suite 1: create-session structure ---

(defun test-create-session ()
  (reset-counts)
  (format t "~&~%  [test-sessions-create-session]~%")

  (let ((s (create-session)))
    (check "session is a list" (listp s))
    (check "session has :id" (stringp (getf s :id)))
    (check ":id is non-empty" (plusp (length (getf s :id))))
    (check "session has :created-at" (stringp (getf s :created-at)))
    (check ":created-at is non-empty" (plusp (length (getf s :created-at))))
    (check "session has :messages vector" (vectorp (getf s :messages)))
    (check ":messages starts empty" (zerop (length (getf s :messages))))
    (check ":messages is adjustable" (adjustable-array-p (getf s :messages)))
    (check "session has :metadata" (hash-table-p (getf s :metadata))))

  (let ((s1 (create-session))
        (s2 (create-session)))
    (check "two sessions get distinct IDs"
           (not (equal (getf s1 :id) (getf s2 :id)))))

  (let ((meta (make-hash-table :test #'equal)))
    (setf (gethash "channel" meta) "discord")
    (let ((s (create-session :metadata meta)))
      (check= "custom metadata is stored"
              (gethash "channel" (getf s :metadata)) "discord")))

  (print-summary "sessions-create-session"))

;;; --- Suite 2: add-message ---

(defun test-add-message ()
  (reset-counts)
  (format t "~&~%  [test-sessions-add-message]~%")

  (let ((s (create-session)))
    (add-message s :user "Hello, world.")
    (check= "one message after first add" (length (getf s :messages)) 1)

    (let ((msg (aref (getf s :messages) 0)))
      (check "message is a hash-table" (hash-table-p msg))
      (check= "message role is user" (gethash "role" msg) "user")
      (check= "message content" (gethash "content" msg) "Hello, world.")
      (check "message has timestamp" (stringp (gethash "timestamp" msg))))

    (add-message s :assistant "Hi there.")
    (check= "two messages after second add" (length (getf s :messages)) 2)
    (check= "second message role is assistant"
            (gethash "role" (aref (getf s :messages) 1)) "assistant")

    ;; add-message returns the session (for chaining)
    (check "add-message returns the session" (eq (add-message s :user "ping") s)))

  ;; Role keyword is downcased to string
  (let ((s (create-session)))
    (add-message s :TOOL "tool result")
    (check= "role keyword is lowercased"
            (gethash "role" (aref (getf s :messages) 0)) "tool"))

  (print-summary "sessions-add-message"))

;;; --- Suite 3: ensure-adjustable-vector ---

(defun test-ensure-adjustable-vector ()
  (reset-counts)
  (format t "~&~%  [test-sessions-ensure-adjustable-vector]~%")

  (let ((v (crichton/sessions::ensure-adjustable-vector nil)))
    (check "nil → adjustable empty vector" (adjustable-array-p v))
    (check= "nil → zero length" (length v) 0))

  (let ((v (crichton/sessions::ensure-adjustable-vector '(1 2 3))))
    (check "list → adjustable vector" (adjustable-array-p v))
    (check= "list → correct length" (length v) 3)
    (check= "list → element 0" (aref v 0) 1)
    (check= "list → element 2" (aref v 2) 3))

  (let ((v (crichton/sessions::ensure-adjustable-vector #(10 20))))
    (check "simple-vector → adjustable vector" (adjustable-array-p v))
    (check= "simple-vector → correct length" (length v) 2)
    (check= "simple-vector → element 0" (aref v 0) 10))

  (let* ((orig (make-array 2 :adjustable t :fill-pointer 2
                             :initial-contents '(:a :b)))
         (v (crichton/sessions::ensure-adjustable-vector orig)))
    (check "adjustable-vector → still adjustable" (adjustable-array-p v))
    (check= "adjustable-vector → correct length" (length v) 2)
    (check= "adjustable-vector → element 1" (aref v 1) :b))

  (check-error "non-sequence type signals error"
    (crichton/sessions::ensure-adjustable-vector 42))

  (print-summary "sessions-ensure-adjustable-vector"))

;;; --- Suite 4: generate-session-id ---

(defun hex-string-p (s)
  "Return T if S is a non-empty string of hex digits."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (digit-char-p c)
                    (member c '(#\a #\b #\c #\d #\e #\f) :test #'char=)))
              s)))

(defun test-generate-session-id ()
  (reset-counts)
  (format t "~&~%  [test-sessions-generate-session-id]~%")

  (let ((id (crichton/sessions::generate-session-id)))
    (check "id is a string" (stringp id))
    (check= "id is 32 hex chars (16 bytes)" (length id) 32)
    (check "id contains only hex digits" (hex-string-p id)))

  (let ((ids (loop repeat 10 collect (crichton/sessions::generate-session-id))))
    (check= "10 IDs are all unique" (length (remove-duplicates ids :test #'equal)) 10))

  (print-summary "sessions-generate-session-id"))

;;; --- Suite 5: unix-timestamp ---

(defun test-unix-timestamp ()
  (reset-counts)
  (format t "~&~%  [test-sessions-unix-timestamp]~%")

  (let ((ts (crichton/sessions::unix-timestamp)))
    (check "unix-timestamp returns an integer" (integerp ts))
    ;; Sanity: after 2020-01-01 (1577836800) and before 2060-01-01 (2840140800)
    (check "timestamp is in plausible range"
           (and (> ts 1577836800) (< ts 2840140800)))
    ;; Two calls a moment apart should be close
    (let ((ts2 (crichton/sessions::unix-timestamp)))
      (check "two timestamps within 1 second" (< (abs (- ts2 ts)) 2))))

  (print-summary "sessions-unix-timestamp"))

;;; --- Suite 6: JSON serialization roundtrip ---

(defun test-serialization-roundtrip ()
  (reset-counts)
  (format t "~&~%  [test-sessions-serialization-roundtrip]~%")

  (let* ((s (create-session)))
    (add-message s :user "What time is it?")
    (add-message s :assistant "It's noon.")
    (let* ((original-id (getf s :id))
           (original-created (getf s :created-at))
           (bytes (crichton/sessions::session-to-json-bytes s)))

      (check "session serializes to bytes"
             (and bytes (plusp (length bytes))))

      (let* ((raw (crichton/sessions::json-bytes-to-session bytes))
             ;; messages come back as a list from JSON — run through ensure-adjustable-vector
             (restored (progn
                         (setf (getf raw :messages)
                               (crichton/sessions::ensure-adjustable-vector
                                (getf raw :messages)))
                         raw)))

        (check= "roundtrip: :id preserved" (getf restored :id) original-id)
        (check= "roundtrip: :created-at preserved"
                (getf restored :created-at) original-created)
        (check= "roundtrip: message count" (length (getf restored :messages)) 2)

        (let ((m0 (aref (getf restored :messages) 0))
              (m1 (aref (getf restored :messages) 1)))
          (check= "roundtrip: first message role" (gethash "role" m0) "user")
          (check= "roundtrip: first message content"
                  (gethash "content" m0) "What time is it?")
          (check= "roundtrip: second message role" (gethash "role" m1) "assistant")
          (check= "roundtrip: second message content"
                  (gethash "content" m1) "It's noon.")))))

  (print-summary "sessions-serialization-roundtrip"))

;;; --- Top-level runner ---

(defun run-all ()
  (let ((total-pass 0)
        (total-fail 0))
    (flet ((run-suite (fn)
             (funcall fn)
             (incf total-pass crichton/tests/harness:*pass-count*)
             (incf total-fail crichton/tests/harness:*fail-count*)))
      (run-suite #'test-create-session)
      (run-suite #'test-add-message)
      (run-suite #'test-ensure-adjustable-vector)
      (run-suite #'test-generate-session-id)
      (run-suite #'test-unix-timestamp)
      (run-suite #'test-serialization-roundtrip))
    (zerop total-fail)))
