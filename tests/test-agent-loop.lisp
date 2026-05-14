;;;; tests/test-agent-loop.lisp
;;;;
;;;; Unit tests for agent/loop.lisp — specifically run-agent-loop.
;;;; All tests use a mock send-fn so no LLM network calls are made.
;;;;
;;;; Usage (from SLIME/SLY or crichton-client --eval):
;;;;   (load "tests/harness.lisp")
;;;;   (load "tests/test-agent-loop.lisp")
;;;;   (crichton/tests/agent-loop:run-all)

(defpackage #:crichton/tests/agent-loop
  (:use #:cl #:crichton/tests/harness)
  (:import-from #:crichton/agent #:run-agent-loop)
  (:export #:run-all
           #:test-normal-flow
           #:test-max-tokens-with-tool-use))

(in-package #:crichton/tests/agent-loop)

;;; --- helpers ---

(defun make-text-response (text)
  "Simulate a successful text response from the LLM."
  (list :content (list (list :type :text :text text))
        :stop-reason :end-turn))

(defun make-tool-use-response (tool-id tool-name)
  "Simulate an LLM response requesting a tool call."
  (list :content (list (list :type :tool-use :id tool-id :name tool-name :input nil))
        :stop-reason :tool-use))

(defun make-max-tokens-with-tool-use-response (tool-id tool-name)
  "Simulate an LLM response cut off by max_tokens mid-tool-use."
  (list :content (list (list :type :tool-use :id tool-id :name tool-name :input nil))
        :stop-reason :max-tokens))

(defun initial-msgs (text)
  (list (list :role :user :content text)))

(defun last-role (msgs)
  (getf (car (last msgs)) :role))

(defun has-unmatched-tool-use-p (msgs)
  "Return T if msgs ends with an assistant turn containing tool_use but no
   following user turn with tool_result — exactly the session-poisoning state."
  (let ((last-msg (car (last msgs))))
    (and (eq :assistant (getf last-msg :role))
         (crichton/llm:blocks-tool-uses (getf last-msg :content)))))

;;; --- test suites ---

(defun test-normal-flow ()
  "run-agent-loop with a single tool call followed by a text response."
  (format t "~%  [test-normal-flow]~%")
  (crichton/tests/harness:reset-counts)

  ;; send-fn returns: first a tool-use response, then a text response
  (let* ((calls 0)
         (send-fn (lambda (msgs)
                    (declare (ignore msgs))
                    (incf calls)
                    (if (= calls 1)
                        (make-tool-use-response "tu-1" "ping")
                        (make-text-response "done"))))
         (msgs (initial-msgs "hello")))

    (multiple-value-bind (text all-messages)
        (run-agent-loop msgs send-fn "test" 10)

      (check= "returns expected text" text "done")
      (check= "made 2 LLM calls" calls 2)
      (check "last message is assistant text"
             (eq :assistant (last-role all-messages)))
      (check "session ends clean (no unmatched tool_use)"
             (not (has-unmatched-tool-use-p all-messages)))
      (check= "history length: user + assistant(tool) + user(result) + assistant(text)"
              (length all-messages) 4)))

  (print-summary "normal-flow")
  (zerop crichton/tests/harness:*fail-count*))

(defun test-max-tokens-with-tool-use ()
  "run-agent-loop when max_tokens cuts off the LLM mid-tool-use.
   Regression test for the session-poisoning bug: the partial assistant
   turn must be stripped so the session stays in a valid state."
  (format t "~%  [test-max-tokens-with-tool-use]~%")
  (crichton/tests/harness:reset-counts)

  (let* ((send-fn (lambda (msgs)
                    (declare (ignore msgs))
                    (make-max-tokens-with-tool-use-response "tu-cutoff" "rss")))
         (msgs (initial-msgs "remove duplicates")))

    (multiple-value-bind (text all-messages)
        (run-agent-loop msgs send-fn "test" 10)

      (check "returns a non-nil message" (and text (plusp (length text))))
      (check "message mentions token limit"
             (search "cut off" text))
      (check "session does NOT end with unmatched tool_use"
             (not (has-unmatched-tool-use-p all-messages)))
      (check "history is just the original user message (partial turn stripped)"
             (= (length all-messages) 1))
      (check "remaining message is the user turn"
             (eq :user (last-role all-messages)))))

  (print-summary "max-tokens-with-tool-use")
  (zerop crichton/tests/harness:*fail-count*))

(defun test-max-tokens-text-only ()
  "max_tokens on a pure text response is fine — no stripping needed."
  (format t "~%  [test-max-tokens-text-only]~%")
  (crichton/tests/harness:reset-counts)

  (let* ((send-fn (lambda (msgs)
                    (declare (ignore msgs))
                    (list :content (list (list :type :text :text "partial answer"))
                          :stop-reason :max-tokens)))
         (msgs (initial-msgs "question")))

    (multiple-value-bind (text all-messages)
        (run-agent-loop msgs send-fn "test" 10)

      (check= "returns the partial text" text "partial answer")
      (check "session does not have unmatched tool_use"
             (not (has-unmatched-tool-use-p all-messages)))
      (check= "history: user + assistant(text)" (length all-messages) 2)))

  (print-summary "max-tokens-text-only")
  (zerop crichton/tests/harness:*fail-count*))

;;; --- runner ---

(defun run-all ()
  (format t "~%Agent Loop tests:~%")
  (let ((results (list (test-normal-flow)
                       (test-max-tokens-with-tool-use)
                       (test-max-tokens-text-only))))
    (every #'identity results)))
