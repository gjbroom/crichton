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

(defparameter *max-tool-result-chars* 100000
  "Maximum characters to store for a single tool result in message history.
   Larger results are truncated before being stored, preventing them from
   accumulating and eventually overflowing the context window on replay.")

(defparameter *default-max-history-chars* 300000
  "Default character budget for message history before truncation.
   Older user/assistant pairs are dropped when this is exceeded.
   300K chars leaves comfortable headroom under the 200K-token context window
   after accounting for system prompt and tool schemas.")

(defparameter *llm-api-timeout* 300
  "Default timeout in seconds for a single LLM API call.
Override with [llm] api-timeout = N in config.toml.")

(defparameter *tool-execution-timeout* 60
  "Default timeout in seconds for executing a single tool call batch.
Override with [llm] tool-timeout = N in config.toml.")

;;; --- Helpers ---

(defun estimate-content-chars (content)
  "Rough character count for message content (string or block list)."
  (cond
    ((stringp content) (length content))
    ((listp content)
     (loop for block in content
           sum (let ((type (getf block :type)))
                 (case type
                   (:text    (length (or (getf block :text) "")))
                   (:tool-result (length (format nil "~A" (or (getf block :content) ""))))
                   (:tool-use    (length (format nil "~A" (or (getf block :input) ""))))
                   (otherwise 64)))))
    (t 0)))

(defun truncate-history (msgs &optional (max-chars *default-max-history-chars*))
  "Drop oldest user/assistant pairs from MSGS until total chars are under MAX-CHARS.
   Drops in pairs so the history always starts with a user message.
   Will drop all messages if necessary — there is no guaranteed minimum."
  (when (null msgs)
    (return-from truncate-history msgs))
  (let ((total (loop for m in msgs
                     sum (estimate-content-chars (getf m :content)))))
    (when (> total max-chars)
      (log:info "History truncation: ~D chars across ~D messages, budget ~D"
                total (length msgs) max-chars))
    (loop while (and msgs (> total max-chars))
          do (decf total (estimate-content-chars (getf (first msgs) :content)))
             (setf msgs (rest msgs))
             ;; Drop in pairs to keep history starting on a user message
             (when msgs
               (decf total (estimate-content-chars (getf (first msgs) :content)))
               (setf msgs (rest msgs))))
    msgs))

(defun truncate-for-log (text &optional (limit 200))
  "Return at most LIMIT characters of TEXT for log output."
  (let ((s (or text "")))
    (subseq s 0 (min limit (length s)))))

(defun cap-tool-result (result name)
  "Truncate RESULT to *max-tool-result-chars* if necessary, appending a notice."
  (if (> (length result) *max-tool-result-chars*)
      (let ((notice (format nil "~%[Result truncated at ~D chars — ~A returned ~D total]"
                            *max-tool-result-chars* name (length result))))
        (log:info "Tool ~A result truncated: ~D → ~D chars"
                  name (length result) *max-tool-result-chars*)
        (concatenate 'string
                     (subseq result 0 (- *max-tool-result-chars* (length notice)))
                     notice))
      result))

(defun execute-tool-calls (content)
  "Dispatch all tool-use blocks in CONTENT in parallel, return tool-result blocks
   in the same order.  Each tool runs in its own thread; we join all threads
   before returning so the caller sees a complete, ordered result list."
  (let* ((tool-uses (crichton/llm:blocks-tool-uses content))
         (n         (length tool-uses))
         (results   (make-array n)))
    (log:info "Tool calls: ~{~A~^, ~}"
              (mapcar (lambda (tu) (getf tu :name)) tool-uses))
    (let ((threads
            (loop for tu in tool-uses
                  for i from 0
                  collect (let ((tu tu) (i i))
                            (bt:make-thread
                             (lambda ()
                               (let* ((name   (getf tu :name))
                                      (result (cap-tool-result
                                               (dispatch-tool name (getf tu :input))
                                               name)))
                                 (log:debug "Tool ~A result: ~A..." name
                                            (truncate-for-log result))
                                 (setf (aref results i)
                                       (crichton/llm:make-tool-result-block
                                        (getf tu :id) result))))
                             :name (format nil "tool/~A" (getf tu :name)))))))
      (mapc #'bt:join-thread threads))
    (coerce results 'list)))

(defun initialize-messages (user-input messages)
  "Build the initial message list for an agent loop invocation."
  (cond
    ((and messages user-input)
     (append messages (list (list :role :user :content user-input))))
    (messages messages)
    (user-input (list (list :role :user :content user-input)))))

(defun run-agent-loop (msgs send-fn label max-iterations)
  "Core agent loop: call SEND-FN to get an LLM response, dispatch tool calls,
   and iterate until the LLM returns text or MAX-ITERATIONS is reached.
   SEND-FN is (lambda (msgs) → response-plist).
   LABEL is a string for log messages (e.g. \"Agent\" or \"Agent/stream\").
   Returns (values response-text all-messages last-response).

   MSGS is copied on entry so callers retain an unmodified reference. This
   matters for handle-streaming-chat-request's snapshot/rollback: the snapshot
   it holds for error recovery is never mutated by this function."
  (let ((msgs (copy-list msgs)))
  (dotimes (i max-iterations
            (progn
              (log:warn "~A hit max iterations (~D)" label max-iterations)
              (values (format nil "[Agent stopped after ~D iterations]"
                              max-iterations)
                      msgs nil)))
    (log:info "~A iteration ~D/~D" label (1+ i) max-iterations)
    (let* ((api-timeout (or (crichton/config:config-section-get :llm :api-timeout)
                            *llm-api-timeout*))
           (response    (crichton/skills:with-timeout
                            (api-timeout :error-message "LLM API call timed out")
                          (funcall send-fn msgs)))
           (content     (getf response :content))
           (stop-reason (getf response :stop-reason)))
      (setf msgs (nconc msgs (list (list :role :assistant :content content))))
      (log:info "Stop reason: ~A" stop-reason)
      (unless (eq stop-reason :tool-use)
        (let ((text (crichton/llm:response-text response)))
          (log:info "~A done: ~A" label (truncate-for-log text))
          (return (values text msgs response))))
      (let ((tool-timeout (or (crichton/config:config-section-get :llm :tool-timeout)
                               *tool-execution-timeout*)))
        (setf msgs (nconc msgs (list (list :role :user
                                           :content (crichton/skills:with-timeout
                                                        (tool-timeout
                                                         :error-message "Tool execution timed out")
                                                      (execute-tool-calls content)))))))))))

;;; --- Agent loop ---

(defun resolve-system-prompt (system session-type)
  "Resolve the system prompt: use SYSTEM if provided, otherwise build
from bootstrap files for SESSION-TYPE."
  (or system
      (if (crichton/config:config-section-get :state :enabled t)
          (crichton/state:bootstrap-system-prompt :session-type session-type)
          *default-system-prompt*)))

(defun sanitize-input-for-session (user-input session-type)
  "Sanitize USER-INPUT according to SESSION-TYPE trust level.
   Channel sessions (external sources) use a tighter length limit.
   Returns the sanitised string or NIL."
  (when user-input
    (if (eq session-type :channel)
        (sanitize-user-input user-input
                             :max-length +max-channel-input-length+
                             :source "channel")
        (sanitize-user-input user-input
                             :max-length +max-direct-input-length+
                             :source (symbol-name session-type)))))

(defun resolve-tools-for-session (tools session-type)
  "Return the effective tool list for SESSION-TYPE.
   Channel sessions use the restricted channel-safe allowlist unless
   an explicit TOOLS list was supplied by the caller."
  (cond
    (tools tools)
    ((eq session-type :channel) (channel-safe-tool-defs))
    (t (all-tool-defs))))

(defun run-agent (user-input &key provider system tools messages
                                  (session-type :main)
                                  (max-iterations *default-max-iterations*)
                                  (max-tokens *default-max-tokens*)
                                  temperature)
  "Run the agent loop on USER-INPUT.
Returns (values response-text all-messages last-response).

PROVIDER defaults to the configured LLM provider.
SYSTEM defaults to bootstrap-assembled prompt for SESSION-TYPE.
TOOLS defaults to all registered tools (or channel-safe subset for :channel).
MESSAGES is the conversation history (extended destructively via NCONC).
SESSION-TYPE controls bootstrap files and tool access:
  :main     — full tool access, local clients
  :channel  — restricted tool allowlist, sanitised input (Discord etc.)
  :subagent — full tool access, sub-agent calls
MAX-ITERATIONS prevents runaway tool loops."
  (let* ((provider (or provider (crichton/llm:ensure-llm-provider)))
         (system   (resolve-system-prompt system session-type))
         (tools    (resolve-tools-for-session tools session-type))
         (safe-input (sanitize-input-for-session user-input session-type))
         (trimmed  (truncate-history messages))
         (msgs     (initialize-messages safe-input trimmed)))
    (log:info "Agent start [~A]: ~A" session-type (truncate-for-log user-input))
    (run-agent-loop
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
SESSION-TYPE controls bootstrap files and tool access (same as run-agent).
Returns (values response-text all-messages last-response), same as run-agent."
  (let* ((provider (or provider (crichton/llm:ensure-llm-provider)))
         (system   (resolve-system-prompt system session-type))
         (tools    (resolve-tools-for-session tools session-type))
         (safe-input (sanitize-input-for-session user-input session-type))
         (trimmed  (truncate-history messages))
         (msgs     (initialize-messages safe-input trimmed)))
    (log:info "Agent/stream start [~A]: ~A" session-type (truncate-for-log user-input))
    (run-agent-loop
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

