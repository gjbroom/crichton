;;;; channels/discord.lisp
;;;;
;;;; Discord bot adapter via Gateway WebSocket + REST API.
;;;; Connects to the Discord Gateway (v10), receives MESSAGE_CREATE events,
;;;; and sends responses via the REST API.
;;;;
;;;; The bot responds to:
;;;;   - Direct messages
;;;;   - Messages that @mention the bot in guild channels
;;;;
;;;; Bot token is stored in the credential store as "discord-bot-token".

(in-package #:crichton/channels/discord)

(defparameter *gateway-url* "wss://gateway.discord.gg/?v=10&encoding=json"
  "Discord Gateway WebSocket URL.")

(defparameter *api-base* "https://discord.com/api/v10"
  "Discord REST API base URL.")

(defparameter *gateway-intents* (logior (ash 1 0)    ; GUILDS
                                        (ash 1 9)    ; GUILD_MESSAGES
                                        (ash 1 12)   ; DIRECT_MESSAGES
                                        (ash 1 15))  ; MESSAGE_CONTENT
  "Gateway intents bitmask: guilds + guild messages + DMs + message content.")

(defparameter *max-message-length* 2000
  "Discord maximum message length.")

;;; --- Discord channel class ---

(defclass discord-channel ()
  ((token :initarg :token :accessor discord-token)
   (handler :initform nil :accessor discord-handler)
   (bot-user-id :initform nil :accessor bot-user-id)
   (seq :initform nil :accessor discord-seq)
   (heartbeat-interval-ms :initform nil :accessor heartbeat-interval-ms)
   (heartbeat-thread :initform nil :accessor heartbeat-thread)
   (gateway-thread :initform nil :accessor gateway-thread)
   (ws :initform nil :accessor discord-ws)
   (stop-flag :initform nil :accessor stop-flag)
   (stop-cv :initform (bt:make-condition-variable) :accessor stop-cv)
   (lock :initform (bt:make-lock) :accessor stop-lock)
   (session-id :initform nil :accessor discord-session-id)
   (resume-url :initform nil :accessor discord-resume-url))
  (:documentation "Discord bot channel adapter."))

(defun make-discord-channel ()
  "Create a Discord channel adapter. Reads bot token from credential store."
  (let* ((store (crichton/credentials:ensure-credential-store))
         (cred (crichton/credentials:cred-get store "discord-bot-token"))
         (token (getf cred :token)))
    (unless token
      (error "Discord bot token not found in credential store.
Store it with: (crichton/credentials:store-credential \"discord-bot-token\" '(:token \"your-bot-token\"))"))
    (make-instance 'discord-channel :token token)))

;;; --- JSON helpers ---

(defun json-string (value)
  "Convert VALUE to a JSON string."
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (shasht:write-json value s))))

(defun parse-json (string)
  "Parse a JSON string into a hash-table."
  (shasht:read-json string))

(defun hg (ht &rest keys)
  "Nested hash-table get. (hg ht \"d\" \"user\" \"id\") traverses into nested tables."
  (let ((v ht))
    (dolist (k keys v)
      (if (and v (hash-table-p v))
          (setf v (gethash k v))
          (return nil)))))

;;; --- REST API ---

(defun discord-rest-post (token endpoint body)
  "POST to Discord REST API. Returns parsed JSON response."
  (multiple-value-bind (response-body status-code)
      (dex:post (format nil "~A~A" *api-base* endpoint)
                :headers `(("Authorization" . ,(format nil "Bot ~A" token))
                           ("Content-Type" . "application/json")
                           ("User-Agent" . "Crichton/0.1.0"))
                :content (json-string body))
    (when (= status-code 429)
      (let* ((parsed (parse-json response-body))
             (retry-after (or (hg parsed "retry_after") 5)))
        (log:warn "Discord rate limited, waiting ~As" retry-after)
        (sleep (ceiling retry-after))
        (return-from discord-rest-post
          (discord-rest-post token endpoint body))))
    (when (>= status-code 400)
      (log:error "Discord REST error ~D: ~A" status-code response-body))
    (values (when (plusp (length response-body))
              (parse-json response-body))
            status-code)))

(defun split-message (text)
  "Split TEXT into chunks of at most *max-message-length* characters."
  (if (<= (length text) *max-message-length*)
      (list text)
      (loop for start from 0 by *max-message-length*
            while (< start (length text))
            collect (subseq text start
                            (min (+ start *max-message-length*)
                                 (length text))))))

;;; --- Gateway operations ---

(defun send-gateway-message (ws op data)
  "Send a JSON payload to the Discord Gateway."
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "op" payload) op
          (gethash "d" payload) data)
    (wsd:send-text ws (json-string payload))))

(defun send-identify (channel)
  "Send IDENTIFY (op 2) to the Discord Gateway."
  (let ((data (make-hash-table :test #'equal))
        (props (make-hash-table :test #'equal)))
    (setf (gethash "os" props) "linux"
          (gethash "browser" props) "crichton"
          (gethash "device" props) "crichton")
    (setf (gethash "token" data) (discord-token channel)
          (gethash "intents" data) *gateway-intents*
          (gethash "properties" data) props)
    (send-gateway-message (discord-ws channel) 2 data)
    (log:info "Discord: IDENTIFY sent")))

(defun send-heartbeat (channel)
  "Send heartbeat (op 1) to the Discord Gateway."
  (send-gateway-message (discord-ws channel) 1 (discord-seq channel)))

(defun start-heartbeat (channel)
  "Start the heartbeat thread."
  (when (heartbeat-thread channel)
    (bt:destroy-thread (heartbeat-thread channel))
    (setf (heartbeat-thread channel) nil))
  (let ((interval-sec (/ (heartbeat-interval-ms channel) 1000.0)))
    (setf (heartbeat-thread channel)
          (bt:make-thread
           (lambda ()
             (loop while (not (stop-flag channel))
                   do (handler-case
                          (send-heartbeat channel)
                        (error (c)
                          (log:warn "Discord heartbeat error: ~A" c)
                          (return)))
                      (sleep interval-sec))
             (bt:with-lock-held ((stop-lock channel))
               (bt:condition-notify (stop-cv channel))))
           :name "discord-heartbeat"))))

;;; --- Gateway event dispatch ---

(defun strip-mention (text bot-id)
  "Remove @mention tokens for BOT-ID from TEXT."
  (let ((patterns (list (format nil "<@~A>" bot-id)
                        (format nil "<@!~A>" bot-id))))
    (dolist (pat patterns)
      (setf text (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars pat)
                                              text "")))
    (string-trim '(#\Space #\Tab) text)))

(defun bot-mentioned-p (data bot-id)
  "Return T if the bot is mentioned in the MESSAGE_CREATE event data."
  (let ((mentions (gethash "mentions" data)))
    (when (and mentions (vectorp mentions))
      (loop for m across mentions
            when (and (hash-table-p m)
                      (equal (gethash "id" m) bot-id))
              return t))))

;;; --- MESSAGE_CREATE predicates ---

(defun discord-user-authorized-p (author-id)
  "Return T if AUTHOR-ID is authorized to use the bot.
   Reads [discord] allowed_user_ids from config.  An empty or absent list
   means all users are allowed (open access, default)."
  (let ((allowed (crichton/config:config-section-get :discord :allowed-user-ids)))
    (or (null allowed)
        (zerop (length allowed))
        (member author-id allowed :test #'string=))))

(defun ignore-message-p (data channel)
  "Return T if the message should be silently ignored (bot, self, or unauthorized)."
  (let* ((author (hg data "author"))
         (is-bot (when author (gethash "bot" author)))
         (author-id (when author (gethash "id" author))))
    (or is-bot
        (equal author-id (bot-user-id channel))
        (and author-id (not (discord-user-authorized-p author-id))))))

(defun should-respond-p (data channel)
  "Return T if the bot should respond: DM or @mention in a guild channel."
  (or (null (gethash "guild_id" data))
      (bot-mentioned-p data (bot-user-id channel))))

(defun extract-user-text (data channel)
  "Return message text, stripping the bot @mention if present."
  (let ((content (gethash "content" data)))
    (if (and (gethash "guild_id" data)
             (bot-mentioned-p data (bot-user-id channel)))
        (strip-mention content (bot-user-id channel))
        content)))

(defun handle-message-create (channel data)
  "Handle a MESSAGE_CREATE dispatch event."
  (when (and (not (ignore-message-p data channel))
             (should-respond-p data channel))
    (let ((text (extract-user-text data channel))
          (author (hg data "author")))
      (when (and text (plusp (length text)) (discord-handler channel))
        (let ((msg (crichton/channels:make-channel-message
                    :id (gethash "id" data)
                    :text text
                    :author-id (when author (gethash "id" author))
                    :author-name (when author (gethash "username" author))
                    :channel-id (gethash "channel_id" data)
                    :guild-id (gethash "guild_id" data)
                    :raw data)))
          (bt:make-thread
           (lambda ()
             (handler-case
                 (funcall (discord-handler channel) channel msg)
               (error (c)
                 (log:error "Discord message handler error: ~A" c))))
           :name "discord-msg-handler"))))))

;;; --- Gateway op handlers ---

(defun handle-hello (channel data)
  "Handle HELLO (op 10): configure heartbeat interval and identify."
  (setf (heartbeat-interval-ms channel) (gethash "heartbeat_interval" data))
  (log:info "Discord: HELLO received (heartbeat ~Dms)" (heartbeat-interval-ms channel))
  (start-heartbeat channel)
  (send-identify channel))

(defun handle-dispatch-ready (channel data)
  "Handle DISPATCH READY: record bot identity and session info."
  (let ((user-id (hg data "user" "id")))
    (setf (bot-user-id channel) user-id
          (discord-session-id channel) (gethash "session_id" data)
          (discord-resume-url channel) (gethash "resume_gateway_url" data))
    (log:info "Discord: READY (bot user ~A)" user-id)))

(defun handle-dispatch-event (channel event-type data)
  "Dispatch a DISPATCH (op 0) event by type."
  (cond
    ((string= event-type "READY") (handle-dispatch-ready channel data))
    ((string= event-type "MESSAGE_CREATE") (handle-message-create channel data))))

(defun handle-gateway-event (channel payload-text)
  "Parse and dispatch a Gateway event."
  (handler-case
      (let* ((payload (parse-json payload-text))
             (op (gethash "op" payload))
             (data (gethash "d" payload))
             (seq (gethash "s" payload))
             (event-type (gethash "t" payload)))
        (when seq
          (setf (discord-seq channel) seq))
        (case op
          (10 (handle-hello channel data))
          (11 nil)                                    ; HEARTBEAT_ACK
          (0  (handle-dispatch-event channel event-type data))
          (7  (log:warn "Discord: RECONNECT requested")
              (wsd:close-connection (discord-ws channel)))
          (9  (log:warn "Discord: INVALID_SESSION")
              (sleep 3)
              (send-identify channel))
          (1  (send-heartbeat channel))))             ; server-requested heartbeat
    (error (c)
      (log:error "Discord gateway event error: ~A" c))))

;;; --- Connection lifecycle ---

(defun run-gateway (channel)
  "Main gateway connection loop with auto-reconnect."
  (loop while (not (stop-flag channel))
        do (handler-case
               (progn
                 (log:info "Discord: connecting to gateway...")
                 (let ((ws (wsd:make-client *gateway-url*)))
                   (setf (discord-ws channel) ws)
                   (wsd:on :message ws
                           (lambda (event)
                             (handle-gateway-event channel event)))
                   (wsd:on :close ws
                           (lambda (&key code reason)
                             (log:warn "Discord: connection closed (~A: ~A)"
                                       code reason)))
                   (wsd:on :error ws
                           (lambda (error)
                             (log:error "Discord: WebSocket error: ~A" error)))
                   (wsd:start-connection ws)))
             (error (c)
               (log:error "Discord gateway error: ~A" c)))
           (when (heartbeat-thread channel)
             (ignore-errors (bt:destroy-thread (heartbeat-thread channel)))
             (setf (heartbeat-thread channel) nil))
           (unless (stop-flag channel)
             (log:info "Discord: reconnecting in 5s...")
             (sleep 5)))
  (bt:with-lock-held ((stop-lock channel))
    (bt:condition-notify (stop-cv channel))))

;;; --- Protocol implementation ---

(defmethod crichton/channels:channel-connect ((ch discord-channel))
  (setf (stop-flag ch) nil)
  (setf (gateway-thread ch)
        (bt:make-thread (lambda () (run-gateway ch))
                        :name "discord-gateway"))
  t)

(defmethod crichton/channels:channel-disconnect ((ch discord-channel))
  (setf (stop-flag ch) t)
  (when (discord-ws ch)
    (ignore-errors (wsd:close-connection (discord-ws ch))))
  ;; Wait for threads to signal completion with timeout
  (when (or (heartbeat-thread ch) (gateway-thread ch))
    (bt:with-lock-held ((stop-lock ch))
      (bt:condition-wait (stop-cv ch) (stop-lock ch) :timeout 5)))
  ;; Cleanup threads
  (when (heartbeat-thread ch)
    (ignore-errors (bt:destroy-thread (heartbeat-thread ch)))
    (setf (heartbeat-thread ch) nil))
  (when (gateway-thread ch)
    (ignore-errors (bt:destroy-thread (gateway-thread ch)))
    (setf (gateway-thread ch) nil)))

(defmethod crichton/channels:channel-send ((ch discord-channel) destination message)
  (dolist (chunk (split-message message))
    (let ((body (make-hash-table :test #'equal)))
      (setf (gethash "content" body) chunk)
      (discord-rest-post (discord-token ch)
                         (format nil "/channels/~A/messages" destination)
                         body))))

(defmethod crichton/channels:channel-set-handler ((ch discord-channel) handler)
  (setf (discord-handler ch) handler))

(defmethod crichton/channels:channel-name ((ch discord-channel))
  "discord")
