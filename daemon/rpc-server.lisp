;;;; daemon/rpc-server.lisp
;;;;
;;;; NDJSON-over-Unix-socket RPC server for the Crichton daemon.
;;;; Listens on ~/.crichton/daemon.sock, dispatches requests using
;;;; the rpc/protocol.lisp wire format.

(in-package #:crichton/daemon)

(defvar *rpc-server-socket* nil "The listening Unix domain socket.")
(defvar *rpc-server-thread* nil "The accept loop thread.")
(defvar *rpc-running* nil "T while the RPC accept loop is active.")
(defvar *current-rpc-stream* nil "Stream of the currently-handled RPC connection.")
(defvar *current-rpc-stream-lock* nil "Write lock for the currently-handled RPC connection.")
(defvar *chat-sessions* (make-hash-table :test #'equal)
  "In-memory message lists keyed by session-id.")
(defvar *chat-session-locks* (make-hash-table :test #'equal)
  "Per-session locks keyed by session-id.")

(defun daemon-socket-path ()
  "Return the path to the daemon RPC socket."
  (namestring (merge-pathnames "daemon.sock" crichton/config:*agent-home*)))

;;; --- Per-session locking ---

(defvar *chat-session-locks-lock* (bt:make-lock "chat-session-locks-lock"))

(defun get-session-lock (session-id)
  "Return the lock for SESSION-ID, creating one if needed."
  (bt:with-lock-held (*chat-session-locks-lock*)
    (or (gethash session-id *chat-session-locks*)
        (setf (gethash session-id *chat-session-locks*)
              (bt:make-lock (format nil "chat-session-~A" session-id))))))

;;; --- Push helper ---

(defun rpc-push (stream lock msg)
  "Push a message to a specific client connection."
  (handler-case
      (bt:with-lock-held (lock)
        (crichton/rpc:write-message stream msg))
    (error (c)
      (log:warn "RPC push failed: ~A" c))))

;;; --- Chat handler ---

(defun handle-streaming-chat-request (id msg stream lock)
  "Handle a streaming 'chat' request. Pushes chat_delta/chat_done messages."
  (let ((text (crichton/rpc:msg-get msg "text"))
        (session-id (crichton/rpc:msg-get msg "session_id")))
    (unless text
      (return-from handle-streaming-chat-request
        (crichton/rpc:make-error-response id "bad_request" "Missing required field: text")))
    (let* ((session-id (or session-id
                           (getf (crichton/sessions:create-session) :id)))
           (session-lock (get-session-lock session-id)))
      (bt:with-lock-held (session-lock)
        (let ((msgs (gethash session-id *chat-sessions*)))
          (handler-case
              (multiple-value-bind (response-text all-messages)
                  (crichton/agent:run-agent/stream
                   text
                   (lambda (delta)
                     (let ((delta-msg (make-hash-table :test #'equal)))
                       (setf (gethash "op" delta-msg) "chat_delta"
                             (gethash "id" delta-msg) id
                             (gethash "text" delta-msg) delta)
                       (rpc-push stream lock delta-msg)))
                   :messages msgs)
                (setf (gethash session-id *chat-sessions*) all-messages)
                (let ((done-msg (make-hash-table :test #'equal)))
                  (setf (gethash "op" done-msg) "chat_done"
                        (gethash "id" done-msg) id
                        (gethash "text" done-msg) response-text
                        (gethash "session_id" done-msg) session-id)
                  (rpc-push stream lock done-msg))
                (let ((result (make-hash-table :test #'equal)))
                  (setf (gethash "text" result) response-text
                        (gethash "session_id" result) session-id)
                  (crichton/rpc:make-ok-response id result)))
            (crichton/llm:llm-rate-limit-error (c)
              (log:warn "Rate limited by ~A: ~A"
                        (crichton/llm:provider-id (crichton/llm:llm-error-provider c))
                        (crichton/llm:llm-error-message c))
              (crichton/rpc:make-error-response
               id "rate_limited"
               "I'm being rate limited by the API right now. Please try again in a minute or two."))
            (crichton/llm:llm-auth-error (c)
              (log:error "Authentication failed for ~A: ~A"
                         (crichton/llm:provider-id (crichton/llm:llm-error-provider c))
                         (crichton/llm:llm-error-message c))
              (crichton/rpc:make-error-response
               id "auth_error"
               "My API credentials appear to be invalid. Please check the configuration."))
            (crichton/llm:llm-api-error (c)
              (log:error "LLM API error (~A) HTTP ~D: ~A"
                         (crichton/llm:provider-id (crichton/llm:llm-error-provider c))
                         (crichton/llm:llm-api-error-status c)
                         (crichton/llm:llm-error-message c))
              (crichton/rpc:make-error-response
               id "llm_error"
               "I encountered an error communicating with my AI provider. Check the logs for details."))
            (crichton/llm:llm-error (c)
              (log:error "LLM error: ~A" (crichton/llm:llm-error-message c))
              (crichton/rpc:make-error-response
               id "llm_error"
               "I encountered an error communicating with my AI provider. Check the logs for details."))
            (error (c)
              (log:error "Agent error: ~A" c)
              (crichton/rpc:make-error-response
               id "agent_error"
               (format nil "An unexpected error occurred: ~A" c)))))))))

(defun handle-chat-request (id msg)
  "Handle a 'chat' request: run the agent loop with session tracking."
  (let ((stream-p (crichton/rpc:msg-get msg "stream")))
    (if (and stream-p *current-rpc-stream* *current-rpc-stream-lock*)
        (handle-streaming-chat-request id msg *current-rpc-stream* *current-rpc-stream-lock*)
        (let ((text (crichton/rpc:msg-get msg "text"))
              (session-id (crichton/rpc:msg-get msg "session_id")))
          (unless text
            (return-from handle-chat-request
              (crichton/rpc:make-error-response id "bad_request" "Missing required field: text")))
          (let* ((session-id (or session-id
                                 (getf (crichton/sessions:create-session) :id)))
                 (lock (get-session-lock session-id)))
            (bt:with-lock-held (lock)
              (let ((msgs (gethash session-id *chat-sessions*)))
                (handler-case
                    (multiple-value-bind (response-text all-messages)
                        (crichton/agent:run-agent text :messages msgs)
                      (setf (gethash session-id *chat-sessions*) all-messages)
                      (let ((result (make-hash-table :test #'equal)))
                        (setf (gethash "text" result) response-text
                              (gethash "session_id" result) session-id)
                        (crichton/rpc:make-ok-response id result)))
                  (crichton/llm:llm-rate-limit-error (c)
                    (log:warn "Rate limited by ~A: ~A"
                              (crichton/llm:provider-id (crichton/llm:llm-error-provider c))
                              (crichton/llm:llm-error-message c))
                    (crichton/rpc:make-error-response
                     id "rate_limited"
                     "I'm being rate limited by the API right now. Please try again in a minute or two."))
                  (crichton/llm:llm-auth-error (c)
                    (log:error "Authentication failed for ~A: ~A"
                               (crichton/llm:provider-id (crichton/llm:llm-error-provider c))
                               (crichton/llm:llm-error-message c))
                    (crichton/rpc:make-error-response
                     id "auth_error"
                     "My API credentials appear to be invalid. Please check the configuration."))
                  (crichton/llm:llm-api-error (c)
                    (log:error "LLM API error (~A) HTTP ~D: ~A"
                               (crichton/llm:provider-id (crichton/llm:llm-error-provider c))
                               (crichton/llm:llm-api-error-status c)
                               (crichton/llm:llm-error-message c))
                    (crichton/rpc:make-error-response
                     id "llm_error"
                     "I encountered an error communicating with my AI provider. Check the logs for details."))
                  (crichton/llm:llm-error (c)
                    (log:error "LLM error: ~A" (crichton/llm:llm-error-message c))
                    (crichton/rpc:make-error-response
                     id "llm_error"
                     "I encountered an error communicating with my AI provider. Check the logs for details."))
                  (error (c)
                    (log:error "Agent error: ~A" c)
                    (crichton/rpc:make-error-response
                     id "agent_error"
                     (format nil "An unexpected error occurred: ~A" c)))))))))))

;;; --- Request dispatch ---

(defun rpc-dispatch (msg)
  "Dispatch a parsed NDJSON request and return a response hash-table."
  (let ((id (crichton/rpc:msg-id msg))
        (op (crichton/rpc:msg-op msg)))
    (handler-case
        (crichton/config:string-case op
          ("ping"
           (crichton/rpc:make-ok-response id "pong"))

          ("chat"
           (handle-chat-request id msg))

          ("status"
           (crichton/rpc:make-ok-response id (daemon-status)))

          ("subscribe"
           (when (and *current-rpc-stream* *current-rpc-stream-lock*)
             (add-subscriber *current-rpc-stream* *current-rpc-stream-lock*))
           (crichton/rpc:make-ok-response id t))

          ("stop"
           (bt:make-thread (lambda () (stop-daemon))
                           :name "rpc-stop-daemon")
           (crichton/rpc:make-ok-response id t))

          (otherwise
           (crichton/rpc:make-error-response
            id "unknown_op"
            (format nil "Unknown operation: ~A" op))))
      (error (c)
        (crichton/rpc:make-error-response
         id "internal_error"
         (format nil "~A" c))))))

;;; --- Connection handler ---

(defun handle-rpc-connection (socket)
  "Service a single RPC client connection until EOF or error."
  (let* ((stream (sb-bsd-sockets:socket-make-stream
                  socket :input t :output t
                  :element-type 'character
                  :buffering :line
                  :external-format :utf-8))
         (stream-lock (bt:make-lock "rpc-stream-write")))
    (unwind-protect
         (let ((*current-rpc-stream* stream)
               (*current-rpc-stream-lock* stream-lock))
           (loop
             (let ((msg (handler-case
                            (crichton/rpc:read-message stream)
                          (error (c)
                            (log:error "RPC protocol error: ~A" c)
                            (return)))))
               (when (null msg)
                 (return))
               (let ((response (rpc-dispatch msg)))
                 (handler-case
                     (bt:with-lock-held (stream-lock)
                       (crichton/rpc:write-message stream response))
                   (error (c)
                     (log:error "RPC write error: ~A" c)
                     (return)))))))
      (remove-subscriber stream)
      (close stream)
      (sb-bsd-sockets:socket-close socket))))

;;; --- Server lifecycle ---

(defun start-rpc-server (&key socket-path)
  "Start the daemon RPC server on a Unix domain socket."
  (let ((path (or socket-path (daemon-socket-path))))
    (crichton/agent:register-all-tools)
    (crichton/config:delete-file-if-exists path)
    (let ((server (make-instance 'sb-bsd-sockets:local-socket
                                 :type :stream)))
      (sb-bsd-sockets:socket-bind server path)
      #+sbcl (sb-posix:chmod path #o600)
      (sb-bsd-sockets:socket-listen server 5)
      (setf *rpc-server-socket* server
            *rpc-running* t)
      (setf *rpc-server-thread*
            (bt:make-thread
             (lambda ()
               (log:info "RPC server listening on ~A" path)
               (loop while *rpc-running*
                     do (handler-case
                            (let ((client (sb-bsd-sockets:socket-accept server)))
                              (bt:make-thread
                               (lambda () (handle-rpc-connection client))
                               :name "rpc-client"))
                          (error (c)
                            (when *rpc-running*
                              (log:error "RPC accept error: ~A" c)
                              (sleep 0.1))))))
             :name "rpc-accept-loop"))
      (log:info "RPC server started"))))

(defun stop-rpc-server ()
  "Stop the daemon RPC server."
  (setf *rpc-running* nil)
  (when *rpc-server-socket*
    (handler-case
        (sb-bsd-sockets:socket-close *rpc-server-socket*)
      (error (c)
        (log:warn "Error closing RPC server socket: ~A" c)))
    (setf *rpc-server-socket* nil))
  (crichton/config:delete-file-if-exists (daemon-socket-path))
  (log:info "RPC server stopped"))
