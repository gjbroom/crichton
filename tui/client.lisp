;;;; tui/client.lisp
;;;;
;;;; Main TUI client module for the Crichton daemon.
;;;; Wires together model, view, daemon I/O, and TEA update loop.

(in-package #:crichton-tui)

;;; --- Daemon reader thread ---

(defun start-daemon-reader (stream program)
  "Spawn a background thread that reads NDJSON from STREAM and injects
messages into the tuition PROGRAM via tui:send."
  (bt:make-thread
   (lambda ()
     (handler-case
         (loop for msg = (read-message stream)
               while msg
               do (let ((op (gethash "op" msg)))
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
                         (when result
                           (tui:send program
                                     (make-instance 'daemon-response-msg
                                                    :id (gethash "id" msg)
                                                    :text (gethash "text" result)
                                                    :session (gethash "session_id" result)
                                                    :error-p (not (gethash "ok" msg))))))))))
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
                               :width (- (model-width model) 4))
          (model-viewport model) (tui.viewport:make-viewport
                                  :width (model-width model)
                                  :height (- (model-height model) 3))
          (model-status-text model) "Connected")
    (let ((program tui:*current-program*))
      (lambda ()
        (start-daemon-reader stream program)
        (write-message stream (make-subscribe-request))
        nil))))

;;; --- Local command handling ---

(defun handle-local-command (model text)
  "Handle colon-prefixed local commands. Returns (values model cmd handled-p)."
  (let ((cmd-text (string-trim '(#\Space #\Tab) (subseq text 1))))
    (cond
      ((or (string-equal cmd-text "quit")
           (string-equal cmd-text "q"))
       (values model (tui:quit-cmd) t))

      ((string-equal cmd-text "clear")
       (setf (model-messages model) nil)
       (render-chat-history model)
       (values model nil t))

      ((string-equal cmd-text "new")
       (setf (model-session-id model) nil
             (model-messages model) nil)
       (render-chat-history model)
       (setf (model-status-text model) "New session")
       (values model nil t))

      ((string-equal cmd-text "session")
       (setf (model-status-text model)
             (if (model-session-id model)
                 (format nil "Session: ~A" (model-session-id model))
                 "No active session"))
       (values model nil t))

      ((string-equal cmd-text "status")
       (when (model-daemon-stream model)
         (write-message (model-daemon-stream model) (make-status-request)))
       (setf (model-status-text model) "Status requested")
       (values model nil t))

      ((string-equal cmd-text "notifications")
       (if (model-notification-history model)
           (let ((history-text
                   (with-output-to-string (s)
                     (dolist (n (model-notification-history model))
                       (format s "~A [~A/~A] ~A~%"
                               (format-time (notif-time n))
                               (or (notif-source n) "?")
                               (or (notif-kind n) "?")
                               (notif-text n))))))
             (setf (model-messages model)
                   (nconc (model-messages model)
                          (list (make-instance 'chat-entry
                                               :role :notification
                                               :text history-text
                                               :time (get-universal-time)))))
             (render-chat-history model)
             (tui.viewport:viewport-goto-bottom (model-viewport model)))
           (setf (model-status-text model) "No notifications yet"))
       (values model nil t))

      ((or (string-equal cmd-text "sixel on")
           (string-equal cmd-text "sixel off"))
       (let ((on-p (string-equal cmd-text "sixel on")))
         (setf (model-sixel-enabled model) (if on-p :on :off))
         (setf (model-status-text model)
               (if on-p "Sixel rendering enabled" "Sixel rendering disabled")))
       (values model nil t))

      ((and (>= (length cmd-text) 4)
            (string-equal (subseq cmd-text 0 4) "img "))
       (let* ((path (string-trim '(#\Space #\Tab) (subseq cmd-text 4))))
         (cond
           ((not (probe-file path))
            (setf (model-status-text model)
                  (format nil "File not found: ~A" path)))
           ((not (eq (model-sixel-enabled model) :on))
            (setf (model-status-text model)
                  "Sixel not enabled (use :sixel on)"))
           (t
            (setf (model-messages model)
                  (nconc (model-messages model)
                         (list (make-instance 'chat-entry
                                              :role :notification
                                              :text (format nil "[Image: ~A]" path)
                                              :time (get-universal-time)))))
            (render-chat-history model)
            (tui.viewport:viewport-goto-bottom (model-viewport model)))))
       (values model nil t))

      (t
       (setf (model-status-text model)
             (format nil "Unknown command: :~A" cmd-text))
       (values model nil t)))))

;;; --- Update: key-msg ---

(defmethod tui:update-message ((model tui-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Enter — send chat or handle local command
      ((eq key :enter)
       (let ((text (string-trim '(#\Space #\Tab)
                                (tui.textinput:textinput-value (model-input model)))))
         (cond
           ((zerop (length text))
            (values model nil))
           ;; Local command
           ((char= (char text 0) #\:)
            (tui.textinput:textinput-reset (model-input model))
            (handle-local-command model text))
           ;; Waiting — ignore
           ((eq (model-status model) :waiting)
            (values model nil))
           ;; Send chat
           (t
            (setf (model-messages model)
                  (nconc (model-messages model)
                         (list (make-instance 'chat-entry
                                              :role :user
                                              :text text
                                              :time (get-universal-time)))))
            (let ((placeholder (make-instance 'chat-entry
                                               :role :assistant
                                               :text ""
                                               :time (get-universal-time))))
              (setf (model-messages model)
                    (nconc (model-messages model) (list placeholder)))
              (setf (model-streaming-entry model) placeholder))
            (tui.textinput:textinput-reset (model-input model))
            (setf (model-status model) :waiting
                  (model-status-text model) "Thinking...")
            (let ((spinner (tui.spinner:make-spinner
                            :frames tui.spinner:*spinner-dot*
                            :fps 0.1)))
              (setf (model-spinner model) spinner)
              (render-chat-history model)
              (tui.viewport:viewport-goto-bottom (model-viewport model))
              (values model (tui:batch
                             (send-chat-cmd (model-daemon-stream model)
                                            text
                                            (model-session-id model))
                             (tui.spinner:spinner-init spinner))))))))

      ;; Ctrl-C — quit with confirmation
      ((and (tui:key-msg-ctrl msg)
            (characterp key) (char= key #\c))
       (if (model-confirm-quit model)
           (values model (tui:quit-cmd))
           (progn
             (setf (model-confirm-quit model) t
                   (model-status-text model) "Press Ctrl-C again to quit")
             (values model (lambda ()
                           (sleep 3.0)
                           (setf (model-confirm-quit model) nil
                                 (model-status-text model) "Ready")
                           nil)))))

      ;; Ctrl-D — immediate quit
      ((and (tui:key-msg-ctrl msg)
            (characterp key) (char= key #\d))
       (values model (tui:quit-cmd)))

      ;; Ctrl-L — force redraw
      ((and (tui:key-msg-ctrl msg)
            (characterp key) (char= key #\l))
       (render-chat-history model)
       (values model nil))

      ;; Page Up / Page Down — delegate to viewport
      ((eq key :page-up)
       (tui.viewport:viewport-page-up (model-viewport model))
       (values model nil))

      ((eq key :page-down)
       (tui.viewport:viewport-page-down (model-viewport model))
       (values model nil))

      ;; Home / End — top / bottom of chat
      ((eq key :home)
       (tui.viewport:viewport-goto-top (model-viewport model))
       (values model nil))

      ((eq key :end)
       (tui.viewport:viewport-goto-bottom (model-viewport model))
       (values model nil))

      ;; Escape — dismiss notifications, clear confirm-quit
      ((eq key :escape)
       (setf (model-show-notifications model) nil
             (model-confirm-quit model) nil
             (model-notifications model) nil)
       (values model nil))

      ;; Otherwise — pass to textinput
      (t
       (when (model-confirm-quit model)
         (setf (model-confirm-quit model) nil
               (model-status-text model) "Ready"))
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update (model-input model) msg)
         (setf (model-input model) new-input)
         (values model cmd))))))

;;; --- Update: daemon-chat-delta-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-chat-delta-msg))
  (let ((entry (model-streaming-entry model)))
    (when entry
      (setf (entry-text entry)
            (concatenate 'string (entry-text entry) (msg-delta-text msg)))
      (setf (entry-rendered entry) nil)
      (setf (model-status-text model) "Streaming...")
      (render-chat-history model)
      (tui.viewport:viewport-goto-bottom (model-viewport model))))
  (values model nil))

;;; --- Update: daemon-chat-done-msg ---

(defmethod tui:update-message ((model tui-model) (msg daemon-chat-done-msg))
  (let ((entry (model-streaming-entry model)))
    (when entry
      (when (msg-done-text msg)
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
  (render-chat-history model)
  (tui.viewport:viewport-goto-bottom (model-viewport model))
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
    (setf (model-messages model)
          (nconc (model-messages model)
                 (list (make-instance 'chat-entry
                                       :role role
                                       :text (msg-response-text msg)
                                       :time (get-universal-time)))))
    (when (msg-response-session msg)
      (setf (model-session-id model) (msg-response-session msg)))
    (setf (model-status model) :idle
          (model-spinner model) nil
          (model-status-text model) "Ready")
    (render-chat-history model)
    (tui.viewport:viewport-goto-bottom (model-viewport model))
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
        (tui.viewport:viewport-height (model-viewport model)) (- (model-height model) 3))
  (setf (tui.textinput:textinput-width (model-input model)) (- (model-width model) 4))
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
                  :alt-screen t
                  :mouse nil)))
    (tui:run program)))

(defun one-shot-chat (text)
  (connect-daemon)
  (unwind-protect
       (multiple-value-bind (response-text)
           (send-chat-sync text)
         (format t "~A~%" response-text))
    (disconnect)))

(defun main ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (handler-case
        (if args
            (one-shot-chat (format nil "~{~A~^ ~}" args))
            (tui-repl))
      (error (c)
        (format *error-output* "~A~%" c)
        (sb-ext:exit :code 1)))))
