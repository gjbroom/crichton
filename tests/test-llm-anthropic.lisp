;;;; tests/test-llm-anthropic.lisp
;;;;
;;;; Unit tests for pure serialization functions in llm/anthropic.lisp.
;;;; No network calls — tests only the plist↔hash-table conversion layer.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/harness.lisp")
;;;;   (load "tests/test-llm-anthropic.lisp")
;;;;   (crichton/tests/llm:run-all)
;;;;
;;;; Individual suites:
;;;;   (crichton/tests/llm:test-content-blocks)
;;;;   (crichton/tests/llm:test-messages)
;;;;   (crichton/tests/llm:test-tool-choice)
;;;;   (crichton/tests/llm:test-response-parsing)

(defpackage #:crichton/tests/llm
  (:use #:cl #:crichton/tests/harness)
  (:import-from #:crichton/llm
                #:content-block-to-anthropic
                #:messages-to-anthropic
                #:parse-anthropic-response
                #:tool-choice-to-anthropic
                #:build-anthropic-request)
  (:export #:run-all
           #:test-content-blocks
           #:test-messages
           #:test-tool-choice
           #:test-response-parsing))

(in-package #:crichton/tests/llm)

;;; --- helpers ---

(defun ht-get (ht key)
  (gethash key ht))

(defun make-response-block (type &rest pairs)
  "Build a hash-table simulating an Anthropic response content block."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "type" ht) type)
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun make-response-json (&key (stop-reason "end_turn") blocks
                                (input-tokens 10) (output-tokens 5))
  "Build a hash-table simulating an Anthropic Messages API response."
  (let ((ht (make-hash-table :test #'equal))
        (usage (make-hash-table :test #'equal)))
    (setf (gethash "input_tokens" usage) input-tokens
          (gethash "output_tokens" usage) output-tokens)
    (setf (gethash "stop_reason" ht) stop-reason
          (gethash "content" ht) (coerce (or blocks #()) 'vector)
          (gethash "usage" ht) usage
          (gethash "id" ht) "msg_test123"
          (gethash "model" ht) "claude-test")
    ht))

;;; --- Suite 1: content-block-to-anthropic ---

(defun test-content-blocks ()
  (reset-counts)
  (format t "~&--- Suite: content-block-to-anthropic ---~%")

  ;; text block
  (let ((result (content-block-to-anthropic '(:type :text :text "Hello!"))))
    (check= "text: type key"  (ht-get result "type") "text")
    (check= "text: text key"  (ht-get result "text") "Hello!"))

  ;; tool-use block
  (let ((result (content-block-to-anthropic
                 '(:type :tool-use :id "tu_abc" :name "get_weather"
                   :input (:location "Tokyo")))))
    (check= "tool-use: type"   (ht-get result "type") "tool_use")
    (check= "tool-use: id"     (ht-get result "id") "tu_abc")
    (check= "tool-use: name"   (ht-get result "name") "get_weather"))

  ;; tool-result block without error
  (let ((result (content-block-to-anthropic
                 '(:type :tool-result :tool-use-id "tu_abc" :content "sunny"))))
    (check= "tool-result: type"       (ht-get result "type") "tool_result")
    (check= "tool-result: tool_use_id" (ht-get result "tool_use_id") "tu_abc")
    (check= "tool-result: content"    (ht-get result "content") "sunny")
    (check "tool-result: no is_error" (null (gethash "is_error" result))))

  ;; tool-result block with is-error
  (let ((result (content-block-to-anthropic
                 '(:type :tool-result :tool-use-id "tu_abc"
                   :content "oops" :is-error t))))
    (check= "tool-result-error: is_error" (ht-get result "is_error") t))

  (print-summary "content-block-to-anthropic"))

;;; --- Suite 2: messages-to-anthropic ---

(defun test-messages ()
  (reset-counts)
  (format t "~&--- Suite: messages-to-anthropic ---~%")

  ;; simple string content
  (let* ((messages '((:role :user :content "Hi")))
         (result   (messages-to-anthropic messages))
         (msg      (first result)))
    (check= "single message: role"    (ht-get msg "role") "user")
    (check= "single message: content" (ht-get msg "content") "Hi"))

  ;; multi-message conversation
  (let* ((messages '((:role :user      :content "Hello")
                     (:role :assistant :content "World")))
         (result   (messages-to-anthropic messages)))
    (check= "two messages count" (length result) 2)
    (check= "first role"  (ht-get (first result)  "role") "user")
    (check= "second role" (ht-get (second result) "role") "assistant"))

  ;; block list content → vector of hash-tables
  (let* ((messages '((:role :assistant
                      :content ((:type :text :text "Okay")
                                (:type :tool-use :id "tu1" :name "search"
                                 :input (:query "test"))))))
         (result   (messages-to-anthropic messages))
         (content  (ht-get (first result) "content")))
    (check "block content is vector" (vectorp content))
    (check= "block count" (length content) 2)
    (check= "first block type" (ht-get (aref content 0) "type") "text"))

  (print-summary "messages-to-anthropic"))

;;; --- Suite 3: tool-choice-to-anthropic ---

(defun test-tool-choice ()
  (reset-counts)
  (format t "~&--- Suite: tool-choice-to-anthropic ---~%")

  (check= ":auto → type=auto"
          (ht-get (tool-choice-to-anthropic :auto) "type")
          "auto")

  (check= ":any → type=any"
          (ht-get (tool-choice-to-anthropic :any) "type")
          "any")

  (check= ":none → type=none"
          (ht-get (tool-choice-to-anthropic :none) "type")
          "none")

  (let ((result (tool-choice-to-anthropic '(:tool "get_weather"))))
    (check= "(:tool name) → type=tool"  (ht-get result "type") "tool")
    (check= "(:tool name) → name field" (ht-get result "name") "get_weather"))

  ;; unknown value falls back to auto
  (check= "unknown → auto"
          (ht-get (tool-choice-to-anthropic :bogus) "type")
          "auto")

  (print-summary "tool-choice-to-anthropic"))

;;; --- Suite 4: parse-anthropic-response ---

(defun test-response-parsing ()
  (reset-counts)
  (format t "~&--- Suite: parse-anthropic-response ---~%")

  ;; basic text response
  (let* ((json   (make-response-json
                  :stop-reason "end_turn"
                  :blocks (list (make-response-block "text" "text" "Hello"))))
         (result (parse-anthropic-response json)))
    (check= "stop-reason end_turn"   (getf result :stop-reason) :end-turn)
    (check= "content block count"    (length (getf result :content)) 1)
    (check= "first block type"       (getf (first (getf result :content)) :type) :text)
    (check= "first block text"       (getf (first (getf result :content)) :text) "Hello")
    (check= "usage input-tokens"     (getf (getf result :usage) :input-tokens) 10)
    (check= "usage output-tokens"    (getf (getf result :usage) :output-tokens) 5)
    (check= "id"                     (getf result :id) "msg_test123")
    (check= "model"                  (getf result :model) "claude-test"))

  ;; stop reasons map correctly
  (dolist (pair '(("tool_use"       . :tool-use)
                  ("max_tokens"     . :max-tokens)
                  ("stop_sequence"  . :stop-sequence)
                  ("unknown_reason" . :unknown)))
    (let* ((json   (make-response-json :stop-reason (car pair) :blocks nil))
           (result (parse-anthropic-response json)))
      (check= (format nil "stop-reason ~A" (car pair))
              (getf result :stop-reason)
              (cdr pair))))

  ;; unknown block type degrades gracefully
  (let* ((json   (make-response-json
                  :blocks (list (make-response-block "thinking" "thinking" "hmm"))))
         (result (parse-anthropic-response json))
         (block  (first (getf result :content))))
    (check= "unknown block type falls back to :text" (getf block :type) :text)
    (check "unknown block text contains marker"
           (search "[unknown block type:" (getf block :text))))

  ;; empty content blocks
  (let* ((json   (make-response-json :blocks nil))
         (result (parse-anthropic-response json)))
    (check "empty content → nil" (null (getf result :content))))

  (print-summary "parse-anthropic-response"))

;;; --- Top-level runner ---

(defun run-all ()
  "Run all LLM serialization test suites. Returns T if all passed."
  (format t "~%=== LLM Anthropic Tests ===~%")
  (test-content-blocks)
  (test-messages)
  (test-tool-choice)
  (test-response-parsing)
  (let ((total-pass *pass-count*)
        (total-fail *fail-count*))
    (format t "=== Total: ~D passed, ~D failed ===~%~%" total-pass total-fail)
    (zerop total-fail)))
