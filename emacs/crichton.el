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

(defvar crichton--last-response nil
  "Text of the most recent complete assistant response.")

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
             (message "Crichton: JSON parse error: %s" (error-message-string err)))))))
    (setq crichton--input-buffer (substring crichton--input-buffer start))))

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
  (if (and crichton--process (process-live-p crichton--process))
      (message "Already connected to Crichton daemon")
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
       (user-error "Cannot connect to Crichton: %s" (error-message-string err))))))

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

;;;; Chat buffer (comint-based)

(require 'comint)

(defvar crichton-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'crichton-disconnect)
    (define-key map (kbd "C-c C-o") #'crichton-last-response-to-buffer)
    map)
  "Keymap for `crichton-chat-mode'.
Inherits from `comint-mode-map' via `define-derived-mode'.")

(define-derived-mode crichton-chat-mode comint-mode "Crichton Chat"
  "Major mode for chatting with the Crichton AI daemon.
Built on `comint-mode' — use \\[comint-send-input] to send,
\\[comint-previous-input] / \\[comint-next-input] for history."
  (setq comint-prompt-regexp "^❯ "
        comint-input-sender #'crichton--input-sender
        comint-prompt-read-only t
        comint-process-echoes nil)
  (setq-local comint-input-ring-size 200)
  (setq-local header-line-format
              '(:eval (if (crichton-connected-p) "Connected" "Disconnected"))))

(defun crichton--chat-buffer ()
  "Return the chat buffer, creating it if needed."
  (let ((buf (get-buffer-create "*crichton-chat*")))
    (unless (comint-check-proc buf)
      (with-current-buffer buf
        (crichton-chat-mode)
        ;; comint needs a process associated with the buffer.
        ;; Use a dummy `cat' — we never write to its stdin since
        ;; comint-input-sender routes input to the daemon instead.
        (let ((proc (start-process "crichton-chat" buf "cat")))
          (set-process-query-on-exit-flag proc nil)
          (set-process-filter proc #'comint-output-filter))
        (crichton--output "— Crichton Chat —\n")
        (crichton--output "\n❯ ")))
    buf))

(defun crichton--output (text)
  "Insert TEXT into the chat buffer as process output."
  (let ((proc (get-buffer-process (crichton--chat-buffer))))
    (when proc
      (comint-output-filter proc text))))

(defun crichton--input-sender (_proc input)
  "Send INPUT to the Crichton daemon.  Called by comint on \\[comint-send-input]."
  (if (not (crichton-connected-p))
      (crichton--output "\n[Not connected — M-x crichton-connect]\n\n❯ ")
    (crichton--output "\n")
    (crichton--send-request "chat"
                            'text input
                            'session_id crichton--session-id
                            'stream t)
    (setq crichton--streaming-buffer "*crichton-chat*")))

;;;###autoload
(defun crichton-chat ()
  "Open the Crichton chat buffer."
  (interactive)
  (unless (crichton-connected-p)
    (crichton-connect))
  (pop-to-buffer (crichton--chat-buffer)))

;;;; Streaming response handlers

(defun crichton--handle-chat-delta (msg)
  "Handle an incremental chat_delta push message."
  (let ((text (alist-get 'text msg)))
    (when (and text crichton--streaming-buffer
               (get-buffer crichton--streaming-buffer))
      (crichton--output text))))

(defun crichton--handle-chat-done (msg)
  "Handle a chat_done push message — finalize streaming response."
  (let ((text (alist-get 'text msg))
        (session (alist-get 'session_id msg)))
    (when session
      (setq crichton--session-id session))
    (when text
      (setq crichton--last-response text))
    (when crichton--streaming-buffer
      (crichton--output "\n\n❯ "))
    (setq crichton--streaming-buffer nil)))

(defun crichton--handle-response (msg)
  "Handle a generic ok/error response (non-streaming fallback)."
  (when (and (not crichton--streaming-buffer)
             (eq (alist-get 'ok msg) t))
    (let* ((result (alist-get 'result msg))
           (text (and (listp result) (alist-get 'text result)))
           (session (and (listp result) (alist-get 'session_id result))))
      (when session
        (setq crichton--session-id session))
      (when text
        (setq crichton--last-response text)
        (crichton--output (format "\n%s\n\n❯ " text))))))

;;;; Notification handler

(defun crichton--handle-notification (msg)
  "Handle a push notification from the daemon."
  (let ((kind (alist-get 'kind msg))
        (text (alist-get 'text msg))
        (source (alist-get 'source msg)))
    (message "Crichton [%s/%s]: %s" (or source "?") (or kind "?") text)
    (when (get-buffer "*crichton-chat*")
      (crichton--output (format "\n[%s/%s] %s\n\n❯ "
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

(defun crichton-new-session ()
  "Start a new chat session."
  (interactive)
  (setq crichton--session-id nil)
  (message "Crichton: new session started"))

(defun crichton-last-response-to-buffer ()
  "Pop the last assistant response into a new org-mode buffer."
  (interactive)
  (unless crichton--last-response
    (user-error "No response to display"))
  (let ((buf (generate-new-buffer "*crichton-result*")))
    (with-current-buffer buf
      (org-mode)
      (insert crichton--last-response)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(provide 'crichton)
;;; crichton.el ends here
