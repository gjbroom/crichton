;;;; agent/loop.lisp
;;;;
;;;; The core agent loop: think → act → observe.
;;;; Sends messages to the LLM with tool definitions, executes tool calls,
;;;; feeds results back, and iterates until the LLM returns text or we
;;;; hit the max-iterations guard.

(in-package #:crichton/agent)

(defparameter *default-system-prompt*
  "You are Crichton, a secure background AI agent daemon running on a Linux system.
You have access to tools for checking weather, system health, RSS feeds, and scheduled tasks.
Be concise and helpful. Use tools when they would help answer the user's question.
When reporting tool results, summarize the key information clearly."
  "Default system prompt for the agent loop.")

(defparameter *default-max-iterations* 10
  "Maximum number of think→act→observe iterations before forced stop.")

(defparameter *default-max-tokens* 4096
  "Default max tokens for agent LLM calls.")

;;; --- Helpers ---

(defun truncate-for-log (text &optional (limit 200))
  "Return at most LIMIT characters of TEXT for log output."
  (let ((s (or text "")))
    (subseq s 0 (min limit (length s)))))

(defun execute-tool-calls (content)
  "Dispatch all tool-use blocks in CONTENT, return tool-result blocks."
  (let ((tool-uses (crichton/llm:blocks-tool-uses content)))
    (log:info "Tool calls: ~{~A~^, ~}"
              (mapcar (lambda (tu) (getf tu :name)) tool-uses))
    (mapcar (lambda (tu)
              (let* ((name   (getf tu :name))
                     (result (dispatch-tool name (getf tu :input))))
                (log:debug "Tool ~A result: ~A..." name
                           (truncate-for-log result))
                (crichton/llm:make-tool-result-block (getf tu :id) result)))
            tool-uses)))

(defun %initialize-messages (user-input messages)
  "Build the initial message list for an agent loop invocation.
   Destructively appends USER-INPUT to MESSAGES when both are provided."
  (if messages
      (progn
        (when user-input
          (nconc messages (list (list :role :user :content user-input))))
        messages)
      (when user-input
        (list (list :role :user :content user-input)))))

(defun %run-agent-loop (msgs send-fn label max-iterations)
  "Core agent loop: call SEND-FN to get an LLM response, dispatch tool calls,
   and iterate until the LLM returns text or MAX-ITERATIONS is reached.
   SEND-FN is (lambda (msgs) → response-plist).
   LABEL is a string for log messages (e.g. \"Agent\" or \"Agent/stream\").
   Returns (values response-text all-messages last-response)."
  (dotimes (i max-iterations
            (progn
              (log:warn "~A hit max iterations (~D)" label max-iterations)
              (values (format nil "[Agent stopped after ~D iterations]"
                              max-iterations)
                      msgs nil)))
    (log:info "~A iteration ~D/~D" label (1+ i) max-iterations)
    (let* ((response    (funcall send-fn msgs))
           (content     (getf response :content))
           (stop-reason (getf response :stop-reason)))
      (setf msgs (nconc msgs (list (list :role :assistant :content content))))
      (log:info "Stop reason: ~A" stop-reason)
      (unless (eq stop-reason :tool-use)
        (let ((text (crichton/llm:response-text response)))
          (log:info "~A done: ~A" label (truncate-for-log text))
          (return (values text msgs response))))
      (setf msgs (nconc msgs (list (list :role :user
                                         :content (execute-tool-calls content))))))))

;;; --- Agent loop ---

(defun %resolve-system-prompt (system session-type)
  "Resolve the system prompt: use SYSTEM if provided, otherwise build
from bootstrap files for SESSION-TYPE."
  (or system
      (if (crichton/config:config-section-get :state :enabled t)
          (crichton/state:bootstrap-system-prompt :session-type session-type)
          *default-system-prompt*)))

(defun run-agent (user-input &key provider system tools messages
                                  (session-type :main)
                                  (max-iterations *default-max-iterations*)
                                  (max-tokens *default-max-tokens*)
                                  temperature)
  "Run the agent loop on USER-INPUT.
Returns (values response-text all-messages last-response).

PROVIDER defaults to the configured LLM provider.
SYSTEM defaults to bootstrap-assembled prompt for SESSION-TYPE.
TOOLS defaults to all registered tools.
MESSAGES is the conversation history (extended destructively via NCONC).
SESSION-TYPE controls which bootstrap files are included (:main, :channel,
:subagent).
MAX-ITERATIONS prevents runaway tool loops."
  (let ((provider (or provider (crichton/llm:ensure-llm-provider)))
        (system   (%resolve-system-prompt system session-type))
        (tools    (or tools (all-tool-defs)))
        (msgs     (%initialize-messages user-input messages)))
    (log:info "Agent start: ~A" (truncate-for-log user-input))
    (%run-agent-loop
     msgs
     (lambda (msgs)
       (crichton/llm:send-message provider msgs
                                  :system system :max-tokens max-tokens
                                  :temperature temperature :tools tools))
     "Agent" max-iterations)))

(defun run-agent/stream (user-input on-delta &key provider system tools messages
                                                      (session-type :main)
                                                      (max-iterations *default-max-iterations*)
                                                      (max-tokens *default-max-tokens*)
                                                      temperature)
  "Run the agent loop on USER-INPUT, streaming the final response.
ON-DELTA is called with each text delta string during the final streaming response.
SESSION-TYPE controls which bootstrap files are included (:main, :channel,
:subagent).
Returns (values response-text all-messages last-response), same as run-agent."
  (let ((provider (or provider (crichton/llm:ensure-llm-provider)))
        (system   (%resolve-system-prompt system session-type))
        (tools    (or tools (all-tool-defs)))
        (msgs     (%initialize-messages user-input messages)))
    (log:info "Agent/stream start: ~A" (truncate-for-log user-input))
    (%run-agent-loop
     msgs
     (lambda (msgs)
       (handler-case
           (crichton/llm:stream-message
            provider msgs
            (lambda (event)
              (when (eq :delta (getf event :type))
                (funcall on-delta (getf event :text))))
            :system system :max-tokens max-tokens
            :temperature temperature :tools tools)
         (crichton/llm:llm-feature-not-supported ()
           (log:warn "Streaming not supported, falling back to send-message")
           (crichton/llm:send-message
            provider msgs
            :system system :max-tokens max-tokens
            :temperature temperature :tools tools))))
     "Agent/stream" max-iterations)))

;;; --- Convenience wrappers ---

(defun ask (text &key system)
  "One-shot agent interaction. Returns the response text.
   The simplest way to use Crichton from the REPL:
     (crichton/agent:ask \"What's the weather like?\")
     (crichton/agent:ask \"How's the system doing?\")"
  (register-all-tools)
  (run-agent text :system system))

(defun chat-session (&key system)
  "Start an interactive chat session in the current REPL.
   Type your messages, get responses with tool use.
   Type :quit or :exit to stop."
  (register-all-tools)
  (let ((msgs nil)
        (provider (crichton/llm:ensure-llm-provider))
        (system-prompt (or system *default-system-prompt*)))
    (format t "~&Crichton agent ready. Type :quit to exit.~%~%")
    (loop
      (format t "> ")
      (finish-output)
      (let ((line (read-line *standard-input* nil nil)))
        (unless line (return))
        (let ((trimmed (string-trim '(#\Space #\Tab) line)))
          (when (or (string-equal trimmed ":quit")
                    (string-equal trimmed ":exit")
                    (string-equal trimmed ":q"))
            (format t "~&Goodbye.~%")
            (return))
          (unless (zerop (length trimmed))
            (if msgs
                (nconc msgs (list (list :role :user :content trimmed)))
                (setf msgs (list (list :role :user :content trimmed))))
            (let ((response-text
                    (run-agent nil :provider provider
                                   :system system-prompt
                                   :messages msgs)))
              (format t "~&~A~%~%" response-text))))))))

