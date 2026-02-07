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

;;; --- Agent loop ---

(defun run-agent (user-input &key (provider nil)
                                  (system nil)
                                  (tools nil)
                                  (messages nil)
                                  (max-iterations *default-max-iterations*)
                                  (max-tokens *default-max-tokens*)
                                  (temperature nil)
                                  (verbose nil))
  "Run the agent loop on USER-INPUT.
   Returns (values response-text all-messages last-response).

   PROVIDER defaults to the configured LLM provider.
   SYSTEM defaults to *default-system-prompt*.
   TOOLS defaults to all registered tools.
   MESSAGES is the conversation history (mutated by appending).
   MAX-ITERATIONS prevents runaway tool loops."
  (let* ((provider (or provider (crichton/llm:ensure-llm-provider)))
         (system-prompt (or system *default-system-prompt*))
         (tool-defs (or tools (all-tool-defs)))
         (msgs (or messages
                   (list (list :role :user :content user-input)))))
    (when (and user-input (not messages))
      nil)
    (when (and user-input messages)
      (nconc msgs (list (list :role :user :content user-input))))
    (loop for iteration from 1 to max-iterations
          do (when verbose
               (log:info "Agent iteration ~D/~D" iteration max-iterations))
             (let ((response
                     (crichton/llm:send-message
                      provider msgs
                      :system system-prompt
                      :max-tokens max-tokens
                      :temperature temperature
                      :tools tool-defs)))
               (let ((content (getf response :content))
                     (stop-reason (getf response :stop-reason)))
                 (nconc msgs (list (list :role :assistant :content content)))
                 (when verbose
                   (log:info "Stop reason: ~A" stop-reason))
                 (unless (eq stop-reason :tool-use)
                   (return-from run-agent
                     (values (crichton/llm:response-text response)
                             msgs
                             response)))
                 (let ((tool-uses (crichton/llm:blocks-tool-uses content)))
                   (when verbose
                     (log:info "Tool calls: ~{~A~^, ~}"
                               (mapcar (lambda (tu) (getf tu :name)) tool-uses)))
                   (let ((result-blocks
                           (mapcar (lambda (tu)
                                     (let* ((name (getf tu :name))
                                            (input (getf tu :input))
                                            (id (getf tu :id))
                                            (result (dispatch-tool name input)))
                                       (when verbose
                                         (log:info "Tool ~A result: ~A..."
                                                   name (subseq result 0
                                                                (min 200 (length result)))))
                                       (crichton/llm:make-tool-result-block id result)))
                                   tool-uses)))
                     (nconc msgs (list (list :role :user
                                            :content result-blocks))))))))
    (log:warn "Agent hit max iterations (~D)" max-iterations)
    (values (format nil "[Agent stopped after ~D iterations]" max-iterations)
            msgs
            nil)))

;;; --- Convenience wrappers ---

(defun ask (text &key (system nil) (verbose nil))
  "One-shot agent interaction. Returns the response text.
   The simplest way to use Crichton from the REPL:
     (crichton/agent:ask \"What's the weather like?\")
     (crichton/agent:ask \"How's the system doing?\")"
  (register-all-tools)
  (run-agent text :system system :verbose verbose))

(defun chat-session (&key (system nil) (verbose nil))
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
                                   :messages msgs
                                   :verbose verbose)))
              (format t "~&~A~%~%" response-text))))))))

