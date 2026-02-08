;;;; channels/manager.lisp
;;;;
;;;; Channel manager: reads config, starts/stops adapters, bridges
;;;; incoming messages to the daemon RPC socket, sends responses back.

(in-package #:crichton/channels)

(defvar *channels* nil
  "List of active channel adapter instances.")

(defvar *conversation-sessions* (make-hash-table :test #'equal)
  "Per-conversation session IDs, keyed by (adapter-type channel-id).")

(defvar *conversation-locks* (make-hash-table :test #'equal)
  "Per-conversation locks to prevent interleaving agent calls.")

(defvar *rpc-id-counter* 0
  "Monotonically increasing correlation ID for RPC requests.")

(defun conversation-key (channel msg)
  "Compute a conversation key from the channel adapter and message."
  (list (channel-name channel) (channel-message-channel-id msg)))

(defun ensure-conversation-lock (key)
  "Get or create a lock for conversation KEY."
  (or (gethash key *conversation-locks*)
      (setf (gethash key *conversation-locks*)
            (bt:make-lock (format nil "conv-~A" key)))))

(defun rpc-chat (text session-id)
  "Send a chat request to the daemon RPC socket.
   Returns (values response-text new-session-id)."
  (let* ((path (namestring (merge-pathnames "daemon.sock"
                                            crichton/config:*agent-home*)))
         (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket path)
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t
                          :element-type 'character
                          :buffering :line
                          :external-format :utf-8)))
             (unwind-protect
                  (let ((req (make-hash-table :test #'equal))
                        (id (incf *rpc-id-counter*)))
                    (setf (gethash "id" req) id
                          (gethash "op" req) "chat"
                          (gethash "text" req) text)
                    (when session-id
                      (setf (gethash "session_id" req) session-id))
                    (crichton/rpc:write-message stream req)
                    (let ((resp (crichton/rpc:read-message stream)))
                      (unless resp
                        (error "No response from daemon RPC"))
                      (unless (crichton/rpc:msg-ok-p resp)
                        (let ((err (crichton/rpc:msg-error resp)))
                          (error "RPC error: ~A"
                                 (if (hash-table-p err)
                                     (gethash "message" err)
                                     err))))
                      (let ((result (crichton/rpc:msg-result resp)))
                        (values (gethash "text" result)
                                (gethash "session_id" result)))))
               (close stream))))
      (handler-case (sb-bsd-sockets:socket-close socket)
        (error () nil)))))

(defun handle-incoming (channel msg)
  "Process an incoming message via the daemon RPC socket and send the response."
  (let* ((key (conversation-key channel msg))
         (lock (ensure-conversation-lock key)))
    (bt:with-lock-held (lock)
      (handler-case
          (let ((session-id (gethash key *conversation-sessions*)))
            (multiple-value-bind (response-text new-session-id)
                (rpc-chat (channel-message-text msg) session-id)
              (when new-session-id
                (setf (gethash key *conversation-sessions*) new-session-id))
              (when (and response-text (plusp (length response-text)))
                (channel-send channel
                              (channel-message-channel-id msg)
                              response-text))))
        (error (c)
          (log:error "Error handling message from ~A: ~A"
                     (channel-name channel) c))))))

(defun discord-enabled-p ()
  "Check if Discord channel is enabled in config."
  (let ((discord-cfg (crichton/config:config-section-get :channels :discord)))
    (cond
      ((null discord-cfg) nil)
      ((listp discord-cfg) (getf discord-cfg :enabled))
      (t discord-cfg))))

(defun start-channels ()
  "Start all configured channel adapters."
  (when (discord-enabled-p)
    (handler-case
        (let ((adapter (crichton/channels/discord:make-discord-channel)))
          (channel-set-handler adapter #'handle-incoming)
          (channel-connect adapter)
          (push adapter *channels*)
          (log:info "Discord channel adapter started"))
      (error (c)
        (log:error "Failed to start Discord adapter: ~A" c)))))

(defun stop-channels ()
  "Stop all active channel adapters."
  (dolist (ch *channels*)
    (handler-case
        (progn
          (channel-disconnect ch)
          (log:info "Stopped channel adapter: ~A" (channel-name ch)))
      (error (c)
        (log:error "Error stopping ~A: ~A" (channel-name ch) c))))
  (setf *channels* nil)
  (clrhash *conversation-sessions*)
  (clrhash *conversation-locks*))
