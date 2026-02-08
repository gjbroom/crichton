;;;; channels/manager.lisp
;;;;
;;;; Channel manager: reads config, starts/stops adapters, bridges
;;;; incoming messages to the agent loop, sends responses back.

(in-package #:crichton/channels)

(defvar *channels* nil
  "List of active channel adapter instances.")

(defvar *conversation-state* (make-hash-table :test #'equal)
  "Per-conversation message history, keyed by (adapter-type channel-id).")

(defvar *conversation-locks* (make-hash-table :test #'equal)
  "Per-conversation locks to prevent interleaving agent calls.")

(defvar *tools-registered* nil
  "Whether agent tools have been registered.")

(defun conversation-key (channel msg)
  "Compute a conversation key from the channel adapter and message."
  (list (channel-name channel) (channel-message-channel-id msg)))

(defun ensure-conversation-lock (key)
  "Get or create a lock for conversation KEY."
  (or (gethash key *conversation-locks*)
      (setf (gethash key *conversation-locks*)
            (bt:make-lock (format nil "conv-~A" key)))))

(defun handle-incoming (channel msg)
  "Process an incoming message through the agent loop and send the response."
  (unless *tools-registered*
    (crichton/agent:register-all-tools)
    (setf *tools-registered* t))
  (let* ((key (conversation-key channel msg))
         (lock (ensure-conversation-lock key)))
    (bt:with-lock-held (lock)
      (handler-case
          (let* ((history (gethash key *conversation-state*))
                 (response-text
                   (crichton/agent:run-agent
                    (channel-message-text msg)
                    :messages history)))
            (when (and response-text (plusp (length response-text)))
              (setf (gethash key *conversation-state*) history)
              (channel-send channel
                            (channel-message-channel-id msg)
                            response-text)))
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
  (clrhash *conversation-state*)
  (clrhash *conversation-locks*)
  (setf *tools-registered* nil))
