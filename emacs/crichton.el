;;; crichton.el --- Emacs client for the Crichton AI daemon -*- lexical-binding: t; -*-

;; Author: Gordon J. Broom <gjbroom>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, org
;; URL: https://github.com/gjbroom/crichton

;;; Commentary:

;; Emacs client for the Crichton AI daemon.  Connects via Unix domain
;; socket using NDJSON protocol, provides a chat buffer with streaming
;; responses, subscribes to push notifications, and exposes an
;; allowlisted set of Emacs functions that the daemon can invoke
;; (bidirectional RPC for org-roam integration, etc.).
;;
;; Usage:
;;   (require 'crichton)
;;   M-x crichton-connect
;;   M-x crichton-chat

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Customization

(defgroup crichton nil
  "Emacs client for the Crichton AI daemon."
  :group 'tools
  :prefix "crichton-")

(defcustom crichton-socket-path
  (expand-file-name "daemon.sock" (expand-file-name ".crichton" "~"))
  "Path to the Crichton daemon Unix socket."
  :type 'file
  :group 'crichton)

(defcustom crichton-allowed-functions
  '(org-roam-db-query
    org-roam-node-list
    org-roam-tag-completions
    org-agenda-list
    org-todo-list)
  "Functions the daemon may invoke via emacs_invoke.
Only allowlisted functions are executed — security boundary.
Functions are called with `apply', so they receive the args list
sent by the daemon."
  :type '(repeat symbol)
  :group 'crichton)

(defcustom crichton-emacs-invoke-timeout 10
  "Seconds the daemon waits for an emacs_invoke response."
  :type 'integer
  :group 'crichton)

(defcustom crichton-reconnect-delay 5
  "Seconds to wait before attempting reconnection."
  :type 'integer
  :group 'crichton)

;;;; Internal state

(defvar crichton--process nil
  "Network process connected to the Crichton daemon.")

(defvar crichton--next-id 0
  "Correlation ID counter for outgoing requests.")

(defvar crichton--pending-requests (make-hash-table :test 'equal)
  "Hash table mapping request ID to callback (lambda (response)).")

(defvar crichton--session-id nil
  "Current chat session ID, set by daemon responses.")

(defvar crichton--input-buffer ""
  "Accumulates partial NDJSON lines from the process filter.")

(defvar crichton--streaming-buffer nil
  "Buffer name currently receiving streaming chat deltas.")

(defvar-local crichton--chat-input-start nil
  "Marker for the start of the input area in the chat buffer.")

;;;; NDJSON protocol

(defun crichton--next-id ()
  "Return the next correlation ID."
  (cl-incf crichton--next-id))

(defun crichton--encode-message (msg)
  "Encode MSG (alist) as a JSON string with trailing newline."
  (concat (json-encode msg) "\n"))

(defun crichton--send (msg)
  "Send MSG (alist) to the daemon as NDJSON."
  (unless (and crichton--process (process-live-p crichton--process))
    (user-error "Not connected to Crichton daemon"))
  (process-send-string crichton--process (crichton--encode-message msg)))

(defun crichton--send-request (op &rest fields)
  "Send a request with OP and additional FIELDS (plist).
Returns the correlation ID."
  (let* ((id (crichton--next-id))
         (msg `((id . ,id) (op . ,op))))
    (while fields
      (push (cons (pop fields) (pop fields)) msg))
    (crichton--send (nreverse msg))
    id))

(defun crichton--send-request-with-callback (op callback &rest fields)
  "Send a request with OP and FIELDS, calling CALLBACK with the response.
Returns the correlation ID."
  (let ((id (apply #'crichton--send-request op fields)))
    (puthash id callback crichton--pending-requests)
    id))

;;;; Process filter — NDJSON line reassembly and dispatch

(defun crichton--process-filter (_proc output)
  "Accumulate OUTPUT and dispatch complete NDJSON lines."
  (setq crichton--input-buffer (concat crichton--input-buffer output))
  (let ((start 0))
    (while (string-match "\n" crichton--input-buffer start)
      (let* ((end (match-end 0))
             (line (substring crichton--input-buffer start (1- end))))
        (setq start end)
        (when (> (length line) 0)
          (condition-case err
              (let ((msg (json-read-from-string line)))
                (crichton--dispatch msg))
            (error
             (message "Crichton: JSON parse error: %s" (error-message-string err))))))
      (setq crichton--input-buffer (substring crichton--input-buffer start)))))

(defun crichton--dispatch (msg)
  "Route a parsed NDJSON message to the appropriate handler."
  (let ((id (alist-get 'id msg))
        (op (alist-get 'op msg))
        (ok (alist-get 'ok msg)))
    (cond
     ;; Response to a request we made (has 'ok' field)
     ((and id (not (eq ok :json-null)) (not (null (assq 'ok msg))))
      (let ((callback (gethash id crichton--pending-requests)))
        (when callback
          (remhash id crichton--pending-requests)
          (funcall callback msg))
        ;; Also route to streaming handlers
        (crichton--handle-response msg)))
     ;; Push: streaming chat delta
     ((equal op "chat_delta")
      (crichton--handle-chat-delta msg))
     ;; Push: streaming chat done
     ((equal op "chat_done")
      (crichton--handle-chat-done msg))
     ;; Push: notification
     ((equal op "notify")
      (crichton--handle-notification msg))
     ;; Push: emacs_invoke (daemon calling us)
     ((equal op "emacs_invoke")
      (crichton--handle-emacs-invoke msg))
     ;; Unknown push
     (op
      (message "Crichton: unknown push op: %s" op)))))

;;;; Process sentinel

(defun crichton--sentinel (proc event)
  "Handle process state changes."
  (unless (process-live-p proc)
    (setq crichton--process nil)
    (message "Crichton: disconnected (%s)" (string-trim event))))

;;;; Connection

;;;###autoload
(defun crichton-connect ()
  "Connect to the Crichton daemon."
  (interactive)
  (when (and crichton--process (process-live-p crichton--process))
    (message "Already connected to Crichton daemon")
    (cl-return-from crichton-connect))
  (unless (file-exists-p crichton-socket-path)
    (user-error "Daemon socket not found at %s — is the daemon running?"
                crichton-socket-path))
  (setq crichton--input-buffer "")
  (condition-case err
      (progn
        (setq crichton--process
              (make-network-process
               :name "crichton"
               :family 'local
               :service crichton-socket-path
               :filter #'crichton--process-filter
               :sentinel #'crichton--sentinel
               :coding 'utf-8-unix
               :noquery t))
        ;; Announce ourselves as an Emacs client
        (crichton--send `((id . ,(crichton--next-id))
                          (op . "hello")
                          (client_type . "emacs")
                          (capabilities . ("emacs_invoke"))))
        ;; Subscribe to push notifications
        (crichton--send-request "subscribe")
        (message "Connected to Crichton daemon"))
    (error
     (user-error "Cannot connect to Crichton: %s" (error-message-string err)))))

(defun crichton-disconnect ()
  "Disconnect from the Crichton daemon."
  (interactive)
  (when crichton--process
    (delete-process crichton--process)
    (setq crichton--process nil)
    (message "Disconnected from Crichton daemon")))

(defun crichton-connected-p ()
  "Return non-nil if connected to the daemon."
  (and crichton--process (process-live-p crichton--process)))

;;;; Ping

(defun crichton-ping ()
  "Ping the daemon and display the result."
  (interactive)
  (crichton--send-request-with-callback
   "ping"
   (lambda (resp)
     (if (eq (alist-get 'ok resp) t)
         (message "Crichton: pong")
       (message "Crichton: ping failed")))))

;;;; Status

(defun crichton-status ()
  "Request daemon status and display it."
  (interactive)
  (crichton--send-request-with-callback
   "status"
   (lambda (resp)
     (if (eq (alist-get 'ok resp) t)
         (message "Crichton status: %s" (alist-get 'result resp))
       (message "Crichton: status request failed")))))

;;;; Chat buffer

(defvar crichton-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'crichton-chat-send)
    (define-key map (kbd "C-c C-k") #'crichton-disconnect)
    map)
  "Keymap for `crichton-chat-mode'.")

(define-derived-mode crichton-chat-mode special-mode "Crichton Chat"
  "Major mode for chatting with the Crichton AI daemon."
  nil)

(defun crichton--chat-buffer ()
  "Return the chat buffer, creating it if needed."
  (let ((buf (get-buffer-create "*crichton-chat*")))
    (with-current-buffer buf
      (unless (eq major-mode 'crichton-chat-mode)
        (crichton-chat-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "— Crichton Chat —\n\n" 'face 'bold))
          (setq-local crichton--chat-input-start (point-marker))
          (set-marker-insertion-type crichton--chat-input-start nil))))
    buf))

(defun crichton--chat-insert (role text)
  "Insert a chat message with ROLE and TEXT into the chat buffer."
  (let ((buf (crichton--chat-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (marker-position crichton--chat-input-start))
          (let ((face (pcase role
                        ("user" 'font-lock-keyword-face)
                        ("assistant" 'font-lock-string-face)
                        ("error" 'error)
                        ("notification" 'font-lock-comment-face)
                        (_ 'default)))
                (prefix (pcase role
                          ("user" "You")
                          ("assistant" "Crichton")
                          ("error" "Error")
                          ("notification" "Notice")
                          (_ role))))
            (insert (propertize (format "%s: " prefix) 'face (list face 'bold))
                    text "\n\n")
            (set-marker crichton--chat-input-start (point))))
        (when at-end
          (goto-char (point-max)))))))

;;;###autoload
(defun crichton-chat ()
  "Open the Crichton chat buffer."
  (interactive)
  (unless (crichton-connected-p)
    (crichton-connect))
  (pop-to-buffer (crichton--chat-buffer)))

(defun crichton-chat-send ()
  "Send the current input line to the daemon."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties
                            (marker-position crichton--chat-input-start)
                            (point-max)))))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    ;; Clear input area
    (let ((inhibit-read-only t))
      (delete-region (marker-position crichton--chat-input-start) (point-max)))
    ;; Show user message
    (crichton--chat-insert "user" text)
    ;; Send to daemon with streaming
    (let ((id (crichton--send-request "chat"
                                      'text text
                                      'session_id crichton--session-id
                                      'stream t)))
      (setq crichton--streaming-buffer "*crichton-chat*")
      ;; Insert placeholder for streaming response
      (with-current-buffer (crichton--chat-buffer)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (marker-position crichton--chat-input-start))
            (insert (propertize "Crichton: " 'face '(font-lock-string-face bold))
                    (propertize "..." 'face 'font-lock-comment-face
                                'crichton-streaming-id id)
                    "\n\n")
            (set-marker crichton--chat-input-start (point))))))))

;;;; Streaming response handlers

(defun crichton--handle-chat-delta (msg)
  "Handle an incremental chat_delta push message."
  (let ((text (alist-get 'text msg))
        (id (alist-get 'id msg)))
    (when (and text crichton--streaming-buffer)
      (let ((buf (get-buffer crichton--streaming-buffer)))
        (when buf
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (save-excursion
                ;; Find the streaming placeholder and replace/extend
                (goto-char (point-min))
                (when (text-property-search-forward
                       'crichton-streaming-id id #'equal)
                  (let ((prop-end (point))
                        (prop-start (previous-single-property-change
                                     (point) 'crichton-streaming-id)))
                    (when prop-start
                      (delete-region prop-start prop-end)
                      (goto-char prop-start)
                      (insert (propertize text 'crichton-streaming-id id)))))))))))))

(defun crichton--handle-chat-done (msg)
  "Handle a chat_done push message — finalize streaming response."
  (let ((text (alist-get 'text msg))
        (id (alist-get 'id msg))
        (session (alist-get 'session_id msg)))
    (when session
      (setq crichton--session-id session))
    (when (and text crichton--streaming-buffer)
      (let ((buf (get-buffer crichton--streaming-buffer)))
        (when buf
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-min))
                (when (text-property-search-forward
                       'crichton-streaming-id id #'equal)
                  (let ((prop-end (point))
                        (prop-start (previous-single-property-change
                                     (point) 'crichton-streaming-id)))
                    (when prop-start
                      (delete-region prop-start prop-end)
                      (goto-char prop-start)
                      ;; Insert final text without the streaming property
                      (insert text))))))))))
    (setq crichton--streaming-buffer nil)))

(defun crichton--handle-response (_msg)
  "Handle a generic response. Currently a no-op; callbacks handle responses."
  nil)

;;;; Notification handler

(defun crichton--handle-notification (msg)
  "Handle a push notification from the daemon."
  (let ((kind (alist-get 'kind msg))
        (text (alist-get 'text msg))
        (source (alist-get 'source msg)))
    (message "Crichton [%s/%s]: %s" (or source "?") (or kind "?") text)
    (when (get-buffer "*crichton-chat*")
      (crichton--chat-insert "notification"
                             (format "[%s/%s] %s"
                                     (or source "?")
                                     (or kind "?")
                                     text)))))

;;;; Emacs invoke handler (daemon → Emacs RPC)

(defun crichton--handle-emacs-invoke (msg)
  "Handle an emacs_invoke message from the daemon.
Execute the requested function if it is in `crichton-allowed-functions',
then send the result back."
  (let* ((id (alist-get 'id msg))
         (func-name (intern (alist-get 'function msg)))
         (args (alist-get 'args msg)))
    (if (memq func-name crichton-allowed-functions)
        (condition-case err
            (let ((result (apply func-name (append args nil))))
              (crichton--send `((id . ,id)
                                (op . "emacs_result")
                                (result . ,result))))
          (error
           (crichton--send `((id . ,id)
                              (op . "emacs_result")
                              (error . ((code . "elisp_error")
                                        (message . ,(error-message-string err))))))))
      (crichton--send `((id . ,id)
                         (op . "emacs_result")
                         (error . ((code . "not_allowed")
                                   (message . ,(format "Function %s not in allowlist"
                                                       func-name)))))))))

;;;; Convenience commands

(defun crichton-send (text)
  "Send TEXT to the Crichton daemon and display the response in the chat buffer.
When called interactively, prompts for input."
  (interactive "sMessage: ")
  (unless (crichton-connected-p)
    (crichton-connect))
  (crichton--chat-insert "user" text)
  (crichton--send-request "chat"
                          'text text
                          'session_id crichton--session-id
                          'stream t)
  (setq crichton--streaming-buffer "*crichton-chat*")
  (pop-to-buffer (crichton--chat-buffer)))

(defun crichton-new-session ()
  "Start a new chat session."
  (interactive)
  (setq crichton--session-id nil)
  (message "Crichton: new session started"))

(provide 'crichton)
;;; crichton.el ends here
