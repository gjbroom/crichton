;;;; tui/client.lisp
;;;;
;;;; Main TUI client module for the Crichton daemon.
;;;; Wires together model, view, daemon I/O, and TEA update loop.

(in-package #:crichton-tui)
(defvar *swank-port* nil
  "Port the Swank server is running on, or NIL if not started.")
(defun start-swank (&key (port nil) (dont-close t))
  "Start a Swank server for SLIME/SLY connections.
   PORT defaults to config value or 4005.
   Idempotent — returns existing port if already running.
   Connect from Emacs with M-x slime-connect RET 127.0.0.1 RET 4005"
  (when *swank-port*
    (format *error-output* "Swank already running on port ~D~%" *swank-port*)
    (return-from start-swank *swank-port*))
  (let ((actual-port (or port 4005)))
    (handler-case
        (progn
          (asdf:load-system :swank)
          (let ((create-server (find-symbol "CREATE-SERVER" :swank)))
            (funcall create-server :port actual-port :dont-close dont-close)
            (setf *swank-port* actual-port)
            (format *error-output* "Swank server started on port ~D~%" actual-port)
            actual-port))
      (error (c)
        (format *error-output* "Could not start Swank: ~A~%" c)
        nil))))

;;; --- Helpers ---

(defun reset-status-cmd (model &key (delay 3))
  "Return a TEA command that resets the status bar to Ready after DELAY seconds."
  (declare (ignore model))
  (tui:tick delay (lambda () (make-instance 'reset-status-msg))))

(defun set-status-temporarily (model text &key (delay 3))
  "Set STATUS-TEXT on MODEL and return a command that resets it after DELAY."
  (setf (model-status-text model) text)
  (reset-status-cmd model :delay delay))

(defun handled (model &optional cmd)
  "Standard return shape for local commands: (values model cmd t)."
  (values model cmd t))

(defun append-chat-entry (model entry &key (scroll-p t))
  "Append ENTRY to MODEL's message list, re-render, and optionally scroll."
  (setf (model-messages model)
        (append (model-messages model) (list entry)))
  (render-chat-history model)
  (when scroll-p
    (tui.viewport:viewport-goto-bottom (model-viewport model)))
  model)

(defun refresh-chat (model &key (scroll-p t))
  "Re-render chat history and optionally scroll to bottom."
  (render-chat-history model)
  (when scroll-p
    (tui.viewport:viewport-goto-bottom (model-viewport model)))
  model)

;;; --- Daemon reader thread ---

(defun dispatch-daemon-message (program msg)
  "Translate a daemon NDJSON MSG hash-table into a typed TUI message and
inject it into PROGRAM via tui:send."
  (let ((op (gethash "op" msg)))
    (cond
      ((equal op "notify")
       (tui:send program
                 (make-instance 'daemon-notification-msg
                                :kind (gethash "kind" msg)
                                :text (gethash "text" msg)
                                :source (gethash "source" msg))))
      ((equal op "chat_delta")
       (tui:send program
                 (make-instance 'daemon-chat-delta-msg
                                :id (gethash "id" msg)
                                :text (gethash "text" msg))))
      ((equal op "chat_done")
       (tui:send program
                 (make-instance 'daemon-chat-done-msg
                                :id (gethash "id" msg)
                                :text (gethash "text" msg)
                                :session (gethash "session_id" msg)
                                :error-p (let ((err (gethash "error" msg)))
                                           (and err t)))))
      (t
       (let ((result (gethash "result" msg)))
         (when (hash-table-p result)
           (tui:send program
                     (make-instance 'daemon-response-msg
                                    :id (gethash "id" msg)
                                    :text (gethash "text" result)
                                    :session (gethash "session_id" result)
                                    :error-p (not (gethash "ok" msg))))))))))

(defun start-daemon-reader (stream program)
  "Spawn a background thread that reads NDJSON from STREAM and dispatches
each message into PROGRAM."
  (bt:make-thread
   (lambda ()
     (handler-case
         (loop for msg = (read-message stream)
               while msg
               do (dispatch-daemon-message program msg))
       (error ()
         (tui:send program (make-instance 'daemon-disconnected-msg)))))
   :name "daemon-reader"))

;;; --- Commands ---

(defun send-chat-cmd (stream text session-id)
  "Return a command that writes a chat request to STREAM."
  (lambda ()
    (let ((request (make-chat-request text :session-id session-id :stream t)))
      (write-message stream request))
    nil))

;;; --- Init ---

(defmethod tui:init ((model tui-model))
  (let ((stream (connect-daemon)))
    (setf (model-daemon-stream model) stream
          (model-input model) (tui.textinput:make-textinput
                               :prompt "❯ "
                               :placeholder "Type a message..."
                               :width 1000000)
          (model-viewport model) (tui.viewport:make-viewport
                                  :width (model-width model)
                                  :height (- (model-height model) 3))
          (model-status-text model) "Connected")
    (let ((program tui:*current-program*))
      (tui:batch
       (lambda ()
         (start-daemon-reader stream program)
         (write-message stream (make-subscribe-request))
         nil)
       (clock-tick-cmd)))))

;;; --- Local command handling ---

(defun parse-local-command (text)
  "Parse a colon-prefixed command. Returns (values keyword args raw-string)."
  (let* ((raw (string-trim '(#\Space #\Tab) (subseq text 1)))
         (space-pos (position #\Space raw))
         (cmd-str (if space-pos (subseq raw 0 space-pos) raw))
         (args (when space-pos
                 (string-trim '(#\Space #\Tab) (subseq raw (1+ space-pos))))))
    (values (intern (string-upcase cmd-str) :keyword)
            args
            raw)))

(defun format-notification-history (history)
  "Format notification history entries into a single display string."
  (with-output-to-string (s)
    (dolist (n history)
      (format s "~A [~A/~A] ~A~%"
              (format-time (notif-time n))
              (or (notif-source n) "?")
              (or (notif-kind n) "?")
              (notif-text n)))))

(defun handle-sixel-command (model args)
  "Handle :sixel on/off command."
  (let ((on-p (string-equal args "on")))
    (setf (model-sixel-enabled model) (if on-p :on :off))
    (set-status-temporarily model
      (if on-p "Sixel rendering enabled" "Sixel rendering disabled"))))

(defun handle-img-command (model args)
  "Handle :img <path> command."
  (let ((path (string-trim '(#\Space #\Tab) (or args ""))))
    (cond
      ((not (probe-file path))
       (set-status-temporarily model (format nil "File not found: ~A" path)))
      ((not (eq (model-sixel-enabled model) :on))
       (set-status-temporarily model "Sixel not enabled (use :sixel on)"))
      (t
       (append-chat-entry model
         (make-instance 'chat-entry
                        :role :notification
                        :text (format nil "[Image: ~A]" path)
                        :time (get-universal-time)))
       (reset-status-cmd model)))))

(defun handle-local-command (model text)
  "Handle colon-prefixed local commands. Returns (values model cmd handled-p)."
  (multiple-value-bind (cmd args) (parse-local-command text)
    (handled model
      (case cmd
        ((:quit :q)
         (tui:quit-cmd))

        (:clear
         (setf (model-messages model) nil)
         (render-chat-history model)
         nil)

        (:new
         (setf (model-session-id model) nil
               (model-messages model) nil)
         (render-chat-history model)
         (set-status-temporarily model "New session"))

        (:session
         (set-status-temporarily model
           (if (model-session-id model)
               (format nil "Session: ~A" (model-session-id model))
               "No active session")))

        (:status
         (when (model-daemon-stream model)
           (write-message (model-daemon-stream model) (make-status-request)))
         (set-status-temporarily model "Status requested"))

        (:notifications
         (if (model-notification-history model)
             (progn
               (append-chat-entry model
                 (make-instance 'chat-entry
                                :role :notification
                                :text (format-notification-history
                                       (model-notification-history model))
                                :time (get-universal-time)))
               (reset-status-cmd model))
             (set-status-temporarily model "No notifications yet")))

        (:sixel
         (handle-sixel-command model args))

        (:img
         (handle-img-command model args))

        (otherwise
         (set-status-temporarily model
           (format nil "Unknown command: :~A"
                   (string-downcase (symbol-name cmd)))))))))

;;; --- Enter-key chat submission ---

(defun handle-send-chat (model text)
  "Append user and placeholder assistant entries, start spinner, and return
a batched command that sends the chat request to the daemon."
  (append-chat-entry model
    (make-instance 'chat-entry :role :user :text text
                               :time (get-universal-time))
    :scroll-p nil)
  (let ((placeholder (make-instance 'chat-entry
                                     :role :assistant :text ""
                                     :time (get-universal-time))))
    (append-chat-entry model placeholder)
    (setf (model-streaming-entry model) placeholder))
  (tui.textinput:textinput-reset (model-input model))
  (setf (model-status model) :waiting
        (model-status-text model) "Thinking...")
  (let ((spinner (tui.spinner:make-spinner
                  :frames tui.spinner:*spinner-dot*
                  :fps 0.1)))
    (setf (model-spinner model) spinner)
    (values model (tui:batch
                   (send-chat-cmd (model-daemon-stream model)
                                  text
                                  (model-session-id model))
                   (tui.spinner:spinner-init spinner)))))

;;; --- Update: clock tick

(defun clock-tick-cmd ()
  (lambda ()
    (sleep 0.1)
    (make-instance 'clock-tick-msg)))

(defmethod tui:update-message ((model tui-model) (msg clock-tick-msg))
  (values model (clock-tick-cmd)))  ; schedule the next tick, view redraws automatically

;;; --- Update: key-msg (decomposed) ---

(defun handle-enter (model)
  "Handle Enter key: submit chat or dispatch a local command."
  (let ((text (string-trim '(#\Space #\Tab)
                           (tui.textinput:textinput-value (model-input model)))))
    (cond
      ((zerop (length text))
       (values model nil))
      ((char= (char text 0) #\:)
       (tui.textinput:textinput-reset (model-input model))
       (handle-local-command model text))
      ((eq (model-status model) :waiting)
       (values model nil))
      (t
       (handle-send-chat model text)))))

(defun handle-ctrl-c (model)
  "Handle Ctrl-C: quit on second press, arm confirmation on first."
  (if (model-confirm-quit model)
      (values model (tui:quit-cmd))
      (progn
        (setf (model-confirm-quit model) t
              (model-status-text model) "Press Ctrl-C again to quit")
        (values model (tui:tick 3 (lambda ()
                                    (make-instance 'reset-confirm-quit-msg)))))))

(defun move-input-cursor-vertical (model delta-rows)
  "Move the input cursor by DELTA-ROWS visual lines. Clamps to bounds."
  (let* ((input (model-input model))
         (cw (input-content-width model))
         (pos (tui.textinput:textinput-cursor-pos input))
         (len (length (tui.textinput:textinput-value input)))
         (new-pos (min len (max 0 (+ pos (* delta-rows cw))))))
    (unless (= new-pos pos)
      (setf (tui.textinput:textinput-cursor-pos input) new-pos)))
  (values model nil))

(defun handle-ctrl-key (model key)
  "Dispatch Ctrl+<key> chords."
  (cond
    ((and (characterp key) (char= key #\c))
     (handle-ctrl-c model))
    ((and (characterp key) (char= key #\d))
     (values model (tui:quit-cmd)))
    ((and (characterp key) (char= key #\l))
     (render-chat-history model)
     (values model nil))
    (t
     (values model nil))))

(defun handle-normal-key (model msg key)
  "Dispatch non-Ctrl keys: navigation, escape, multiline cursor, textinput."
  (case key
    (:page-up   (tui.viewport:viewport-page-up (model-viewport model))
                (values model nil))
    (:page-down (tui.viewport:viewport-page-down (model-viewport model))
                (values model nil))
    (:home      (tui.viewport:viewport-goto-top (model-viewport model))
                (values model nil))
    (:end       (tui.viewport:viewport-goto-bottom (model-viewport model))
                (values model nil))
    (:escape    (setf (model-show-notifications model) nil
                      (model-confirm-quit model) nil
                      (model-notifications model) nil)
                (values model nil))
    (:up        (if (> (input-line-count model) 1)
                    (move-input-cursor-vertical model -1)
                    (values model nil)))
    (:down      (if (> (input-line-count model) 1)
                    (move-input-cursor-vertical model 1)
                    (values model nil)))
    (otherwise  (when (model-confirm-quit model)
                  (setf (model-confirm-quit model) nil
                        (model-status-text model) "Ready"))
                (multiple-value-bind (new-input cmd)
                    (tui.textinput:textinput-update (model-input model) msg)
                  (setf (model-input model) new-input)
                  (values model cmd)))))

(defmethod tui:update-message ((model tui-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Enter comes through with :ctrl t (it's a control character),
      ;; so check for it before dispatching on the ctrl flag.
      ((eq key :enter)
       (handle-enter model))
      ((tui:key-msg-ctrl msg)
       (handle-ctrl-key model key))
      (t
       (handle-normal-key model msg key)))))

;;; --- Update: reset-status-msg ---

(defmethod tui:update-message ((model tui-model) (msg reset-status-msg))
  (setf (model-status-text model) "Ready")
  (values model nil))

;;; --- Update: reset-confirm-quit-msg ---

(defmethod tui:update-message ((model tui-model) (msg reset-confirm-quit-msg))
  (setf (model-confirm-quit model) nil
        (model-status-text model) "Ready")
  (values model nil))

;;; --- Update: daemon-chat-delta-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-chat-delta-msg))
  (let ((entry (model-streaming-entry model)))
    (when entry
      (setf (entry-text entry)
            (concatenate 'string (entry-text entry) (msg-delta-text msg))
            (entry-rendered entry) nil
            (model-status-text model) "Streaming...")
      (refresh-chat model)))
  (values model nil))

;;; --- Update: daemon-chat-done-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-chat-done-msg))
  (let ((entry (model-streaming-entry model)))
    (when entry
      ;; Only use done-text as fallback when streaming produced no text.
      ;; If streaming already built up entry-text, ignore done-text to
      ;; avoid the erase-and-rewrite visible glitch.
      (when (and (msg-done-text msg)
                 (string= (entry-text entry) ""))
        (setf (entry-text entry) (msg-done-text msg)))
      (when (msg-done-error-p msg)
        (setf (entry-role entry) :error))
      (setf (entry-rendered entry) nil)))
  (when (msg-done-session msg)
    (setf (model-session-id model) (msg-done-session msg)))
  (when (msg-done-id msg)
    (push (msg-done-id msg) (model-handled-stream-ids model)))
  (setf (model-streaming-entry model) nil
        (model-status model) :idle
        (model-spinner model) nil
        (model-status-text model) "Ready")
  (refresh-chat model)
  (values model nil))

;;; --- Update: daemon-response-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-response-msg))
  ;; Skip if this response was already handled by streaming
  (let ((id (msg-response-id msg)))
    (when (member id (model-handled-stream-ids model))
      (setf (model-handled-stream-ids model)
            (remove id (model-handled-stream-ids model)))
      (return-from tui:update-message (values model nil))))
  (let ((role (if (msg-response-error-p msg) :error :assistant)))
    (append-chat-entry model
      (make-instance 'chat-entry
                     :role role
                     :text (msg-response-text msg)
                     :time (get-universal-time)))
    (when (msg-response-session msg)
      (setf (model-session-id model) (msg-response-session msg)))
    (setf (model-status model) :idle
          (model-spinner model) nil
          (model-status-text model) "Ready")
    (values model nil)))

;;; --- Update: daemon-notification-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-notification-msg))
  (let ((entry (make-instance 'notification-entry
                               :kind (msg-notif-kind msg)
                               :text (msg-notif-text msg)
                               :source (msg-notif-source msg)
                               :time (get-universal-time))))
    (push entry (model-notification-history model))
    (when (> (length (model-notification-history model)) 100)
      (setf (model-notification-history model)
            (subseq (model-notification-history model) 0 100)))
    (push entry (model-notifications model))
    (setf (model-show-notifications model) t)
    (values model (tui:tick 10 (lambda ()
                                 (make-instance 'dismiss-notification-msg
                                                :entry entry))))))

;;; --- Update: dismiss-notification-msg ---

(defmethod tui:update-message ((model tui-model) (msg dismiss-notification-msg))
  (setf (model-notifications model)
        (remove (msg-dismiss-entry msg) (model-notifications model)))
  (when (null (model-notifications model))
    (setf (model-show-notifications model) nil))
  (values model nil))

;;; --- Update: daemon-disconnected-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-disconnected-msg))
  (setf (model-status model) :error
        (model-status-text model) "Disconnected from daemon"
        (model-daemon-stream model) nil)
  (values model nil))

;;; --- Update: window-size-msg ---

(defmethod tui:update-message ((model tui-model) (msg tui:window-size-msg))
  (setf (model-width model) (tui:window-size-msg-width msg)
        (model-height model) (tui:window-size-msg-height msg))
  (setf (tui.viewport:viewport-width (model-viewport model)) (model-width model)
        (tui.viewport:viewport-height (model-viewport model))
        (max 1 (- (model-height model) (+ 2 (input-line-count model)))))
  (dolist (e (model-messages model))
    (setf (entry-rendered e) nil))
  (render-chat-history model)
  (values model nil))

;;; --- Update: spinner tick ---

(defmethod tui:update-message ((model tui-model) (msg t))
  (cond
    ((and (tui.spinner:spinner-tick-msg-p msg)
          (model-spinner model))
     (multiple-value-bind (new-spinner cmd)
         (tui.spinner:spinner-update (model-spinner model) msg)
       (setf (model-spinner model) new-spinner)
       (values model cmd)))
    (t
     (values model nil))))

;;; --- Entry points ---

(defun tui-repl ()
  (let ((program (tui:make-program
                  (make-instance 'tui-model)
                  :alt-screen nil
                  :mouse nil)))
    (start-swank :port 4006)
    (tui:run program)))

(defun main ()
  (format *error-output* "Starting TUI")
  (handler-case
      (tui-repl)
    (error (c)
      (format *error-output* "~A~%" c)
      (sb-ext:exit :code 1))))
