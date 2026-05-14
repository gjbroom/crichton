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
(defvar *chat-session-active* (make-hash-table :test #'equal)
  "Sessions currently being processed: session-id → (token . claimed-universal-time).
   Prevents a second client from blocking on the same session while an
   LLM call is in flight.  Guarded by *chat-session-active-lock*.
   Claims older than *chat-session-claim-timeout* are force-released on the
   next claim attempt so a reconnecting client is never permanently locked out.")
(defvar *chat-session-active-lock* (bt:make-lock "chat-session-active"))

(defparameter *chat-session-claim-timeout* 60
  "Seconds before a stale session claim is force-released when a new claim arrives.
   Covers the case where the client disconnected (C-c) before the LLM finished,
   so rpc-push write errors never triggered the unwind-protect release.")

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

(defun session-claim (session-id)
  "Atomically claim SESSION-ID for processing.
Returns a claim token (non-NIL) on success, NIL if the session is actively busy.
Force-releases claims older than *chat-session-claim-timeout* seconds so a
reconnecting client is never permanently locked out by a stale in-flight request."
  (bt:with-lock-held (*chat-session-active-lock*)
    (let ((entry (gethash session-id *chat-session-active*)))
      (when (and entry
                 (> (- (get-universal-time) (cdr entry))
                    *chat-session-claim-timeout*))
        (log:warn "Session ~A claim timed out after ~Ds; force-releasing"
                  session-id *chat-session-claim-timeout*)
        (setf entry nil))
      (unless entry
        (let ((token (gensym "CLAIM")))
          (setf (gethash session-id *chat-session-active*)
                (cons token (get-universal-time)))
          token)))))

(defun session-release (session-id token)
  "Release the active claim on SESSION-ID, but only if TOKEN matches.
If the claim was force-released and re-claimed by another thread, the
token won't match and this is a deliberate no-op."
  (bt:with-lock-held (*chat-session-active-lock*)
    (let ((entry (gethash session-id *chat-session-active*)))
      (when (and entry (eq (car entry) token))
        (remhash session-id *chat-session-active*)))))

;;; --- Local helpers ---

(defun ht (&rest pairs)
  "Build an equal-test hash-table from alternating string-key / value PAIRS."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

;;; --- Push helper ---

(defun rpc-push (stream lock msg)
  "Push a message to a specific client connection."
  (handler-case
      (bt:with-lock-held (lock)
        (crichton/rpc:write-message stream msg))
    (error (c)
      (log:warn "RPC push failed: ~A" c))))

;;; --- Chat handler ---

(defun push-chat-done (stream lock id error-text session-id)
  "Push a chat_done message for error cases during streaming."
  (let ((done-msg (ht "op" "chat_done" "id" id "text" error-text "error" t)))
    (when session-id
      (setf (gethash "session_id" done-msg) session-id))
    (rpc-push stream lock done-msg)))

(defmacro with-llm-error-handling ((id &key stream lock session-id) &body body)
  "Execute BODY with unified error handling for LLM/agent errors.
When STREAM and LOCK are provided, also pushes chat_done messages for
streaming clients before returning the error response.

Automatically retries rate-limited requests up to 3 times with backoff
when a :RETRY restart is available (established by the LLM provider)."
  (let ((c (gensym "C"))
        (err-msg (gensym "ERR-MSG"))
        (attempts (gensym "ATTEMPTS"))
        (max-retries (gensym "MAX-RETRIES"))
        (retry (gensym "RETRY"))
        (delay (gensym "DELAY")))
    `(let ((,attempts 0)
           (,max-retries 3))
       (handler-case
           (handler-bind
               ((crichton/llm:llm-rate-limit-error
                  (lambda (,c)
                    (let ((,retry (find-restart :retry ,c)))
                      (when (and ,retry (< ,attempts ,max-retries))
                        (incf ,attempts)
                        (let ((,delay (or (crichton/llm:llm-rate-limit-retry-after ,c)
                                          (* 15 ,attempts))))
                          (log:warn "Rate limited by ~A (attempt ~D/~D), retrying in ~Ds"
                                    (crichton/llm:provider-id (crichton/llm:llm-error-provider ,c))
                                    ,attempts ,max-retries ,delay)
                          (sleep ,delay)
                          (invoke-restart ,retry)))))))
             (progn ,@body))
         (crichton/llm:llm-rate-limit-error (,c)
           (let ((,err-msg (format nil "Rate limited by the API after ~D retry attempt~:P. ~
                                       Please try again in a few minutes."
                                   ,max-retries)))
             (log:warn "Rate limited by ~A (retries exhausted): ~A"
                       (crichton/llm:provider-id (crichton/llm:llm-error-provider ,c))
                       (crichton/llm:llm-error-message ,c))
             ,@(when stream `((push-chat-done ,stream ,lock ,id ,err-msg ,session-id)))
             (crichton/rpc:make-error-response ,id "rate_limited" ,err-msg)))
         (crichton/llm:llm-auth-error (,c)
           (let ((,err-msg "My API credentials appear to be invalid. Please check the configuration."))
             (log:error "Authentication failed for ~A: ~A"
                        (crichton/llm:provider-id (crichton/llm:llm-error-provider ,c))
                        (crichton/llm:llm-error-message ,c))
             ,@(when stream `((push-chat-done ,stream ,lock ,id ,err-msg ,session-id)))
             (crichton/rpc:make-error-response ,id "auth_error" ,err-msg)))
         (crichton/llm:llm-api-error (,c)
           (let ((,err-msg (format nil "~A HTTP ~D: ~A"
                                   (crichton/llm:provider-id (crichton/llm:llm-error-provider ,c))
                                   (crichton/llm:llm-api-error-status ,c)
                                   (crichton/llm:llm-error-message ,c))))
             (log:error "LLM API error ~A" ,err-msg)
             ,@(when stream `((push-chat-done ,stream ,lock ,id ,err-msg ,session-id)))
             (crichton/rpc:make-error-response ,id "llm_error" ,err-msg)))
         (crichton/llm:llm-error (,c)
           (let ((,err-msg (format nil "~A: ~A"
                                   (crichton/llm:provider-id (crichton/llm:llm-error-provider ,c))
                                   (crichton/llm:llm-error-message ,c))))
             (log:error "LLM error ~A" ,err-msg)
             ,@(when stream `((push-chat-done ,stream ,lock ,id ,err-msg ,session-id)))
             (crichton/rpc:make-error-response ,id "llm_error" ,err-msg)))
         (error (,c)
           (let ((,err-msg (format nil "An unexpected error occurred: ~A" ,c)))
             (log:error "Agent error: ~A" ,c)
             ,@(when stream `((push-chat-done ,stream ,lock ,id ,err-msg ,session-id)))
             (crichton/rpc:make-error-response ,id "agent_error" ,err-msg)))))))

(defun rpc-session-type (msg)
  "Derive the :session-type keyword from the RPC message.
   Returns :channel when channel_type is 'channel', :main otherwise."
  (let ((ct (crichton/rpc:msg-get msg "channel_type")))
    (if (and ct (string-equal ct "channel"))
        :channel
        :main)))

(defun session-messages-snapshot (session-id)
  "Return a shallow copy of the current message list for SESSION-ID.
The copy is the rollback point: if the LLM call errors or is interrupted,
the unwind-protect in handle-streaming-chat-request restores this snapshot,
preventing torn session state (e.g. tool_use appended but no tool_result).
Safety relies on run-agent-loop copying its input on entry (so nconc never
reaches back to this list) and initialize-messages using append (so the
user turn is also decoupled)."
  (let ((session-lock (get-session-lock session-id)))
    (bt:with-lock-held (session-lock)
      (copy-list (gethash session-id *chat-sessions*)))))

(defun session-messages-commit (session-id messages)
  "Atomically store MESSAGES as the current history for SESSION-ID."
  (let ((session-lock (get-session-lock session-id)))
    (bt:with-lock-held (session-lock)
      (setf (gethash session-id *chat-sessions*) messages))))

(defun handle-streaming-chat-request (id msg stream lock)
  "Handle a streaming 'chat' request. Pushes chat_delta/chat_done messages.
The session lock is held only briefly for snapshot/commit — never across the
LLM network call.  session-claim prevents a second client from hanging on the
same session; it gets an immediate error instead.  If the thread is killed or
errors, unwind-protect restores the pre-request snapshot so the session
remains consistent."
  (let ((text (crichton/rpc:msg-get msg "text"))
        (session-id (crichton/rpc:msg-get msg "session_id"))
        (session-type (rpc-session-type msg)))
    (unless text
      (return-from handle-streaming-chat-request
        (crichton/rpc:make-error-response id "bad_request" "Missing required field: text")))
    (let ((session-id (or session-id (getf (crichton/sessions:create-session) :id))))
      (let ((token (session-claim session-id)))
        (unless token
          (let ((err "Session is busy — a previous request is still processing."))
            (push-chat-done stream lock id err session-id)
            (return-from handle-streaming-chat-request
              (crichton/rpc:make-error-response id "session_busy" err))))
        (let ((snapshot (session-messages-snapshot session-id))
              (committed nil))
          (unwind-protect
              (with-llm-error-handling (id :stream stream :lock lock :session-id session-id)
                (multiple-value-bind (response-text all-messages)
                    (crichton/agent:run-agent/stream
                     text
                     ;; Write directly (not via rpc-push) so that a closed-socket
                     ;; error propagates out of run-agent/stream and triggers the
                     ;; unwind-protect below, releasing the session immediately on
                     ;; client disconnect rather than waiting for the LLM to finish.
                     (lambda (delta)
                       (bt:with-lock-held (lock)
                         (crichton/rpc:write-message stream
                                                     (ht "op" "chat_delta"
                                                         "id" id
                                                         "text" delta))))
                     :session-type session-type
                     :messages snapshot)
                  (session-messages-commit session-id all-messages)
                  (setf committed t)
                  (rpc-push stream lock
                            (ht "op" "chat_done" "id" id
                                "text" response-text "session_id" session-id))
                  (crichton/rpc:make-ok-response id
                    (ht "text" response-text "session_id" session-id))))
            (unless committed
              (session-messages-commit session-id snapshot))
            (session-release session-id token)))))))

(defun handle-blocking-chat-request (id msg)
  "Handle a non-streaming 'chat' request.
Same snapshot/commit/restore pattern as the streaming handler."
  (let ((text (crichton/rpc:msg-get msg "text"))
        (session-id (crichton/rpc:msg-get msg "session_id"))
        (session-type (rpc-session-type msg)))
    (unless text
      (return-from handle-blocking-chat-request
        (crichton/rpc:make-error-response id "bad_request" "Missing required field: text")))
    (let ((session-id (or session-id (getf (crichton/sessions:create-session) :id))))
      (let ((token (session-claim session-id)))
        (unless token
          (return-from handle-blocking-chat-request
            (crichton/rpc:make-error-response id "session_busy"
                                              "Session is busy — a previous request is still processing.")))
        (let ((snapshot (session-messages-snapshot session-id))
              (committed nil))
          (unwind-protect
              (with-llm-error-handling (id)
                (multiple-value-bind (response-text all-messages)
                    (crichton/agent:run-agent text :session-type session-type :messages snapshot)
                  (session-messages-commit session-id all-messages)
                  (setf committed t)
                  (crichton/rpc:make-ok-response id
                    (ht "text" response-text "session_id" session-id))))
            (unless committed
              (session-messages-commit session-id snapshot))
            (session-release session-id token)))))))

(defun handle-chat-request (id msg)
  "Handle a 'chat' request: dispatch to streaming or blocking handler."
  (if (and (crichton/rpc:msg-get msg "stream")
           *current-rpc-stream* *current-rpc-stream-lock*)
      (handle-streaming-chat-request id msg *current-rpc-stream* *current-rpc-stream-lock*)
      (handle-blocking-chat-request id msg)))

;;; --- Rate limiting ---

(defvar *rpc-rate-limiter*
  (make-instance 'crichton/skills:rate-limiter
                 :requests-per-window 600
                 :window-seconds 60)
  "Rate limiter for RPC requests (600 requests per 60 seconds).")

;;; --- Request dispatch ---

(defun rpc-dispatch (msg)
  "Dispatch a parsed NDJSON request and return a response hash-table."
  (let ((id (crichton/rpc:msg-id msg))
        (op (crichton/rpc:msg-op msg)))
    (multiple-value-bind (allowed-p retry-after)
        (crichton/skills:check-rate-limit *rpc-rate-limiter* "local")
      (unless allowed-p
        (return-from rpc-dispatch
          (crichton/rpc:make-error-response
           id "rate_limited"
           (format nil "Rate limited; retry after ~Ds" retry-after)))))
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

          ("rss-interests-get"
           (crichton/rpc:make-ok-response
            id (or (crichton/skills:get-interests-profile) "")))

          ("rss-interests-set"
           (let ((text (crichton/rpc:msg-get msg "text")))
             (unless (and text (plusp (length text)))
               (return-from rpc-dispatch
                 (crichton/rpc:make-error-response
                  id "bad_request" "Missing required field: text")))
             (crichton/skills:set-interests-profile text)
             (crichton/rpc:make-ok-response id t)))

          ("rss-inbox-query"
           (let* ((feed   (crichton/rpc:msg-get msg "feed_name"))
                  (items  (crichton/skills:rss-inbox-query :feed-name feed))
                  (result (make-hash-table :test #'equal)))
             (setf (gethash "count" result) (length items)
                   (gethash "items" result)
                   (coerce
                    (mapcar (lambda (a)
                              (let ((ht (make-hash-table :test #'equal)))
                                (setf (gethash "guid"           ht) (getf a :guid "")
                                      (gethash "feed_name"      ht) (getf a :feed-name "")
                                      (gethash "title"          ht) (getf a :title "")
                                      (gethash "link"           ht) (getf a :link "")
                                      (gethash "pub_date"       ht) (getf a :pub-date "")
                                      (gethash "interest_score" ht) (or (getf a :interest-score) 0.0)
                                      (gethash "reason"         ht) (or (getf a :reason) ""))
                                ht))
                            items)
                    'vector))
             (crichton/rpc:make-ok-response id result)))

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
