;;;; client/client.lisp
;;;;
;;;; Minimal chat client for the Crichton daemon.
;;;; Connects via Unix domain socket, speaks NDJSON.
;;;; No heavy dependencies — only sb-bsd-sockets + shasht + cl-readline.

(in-package #:crichton-client)

;;; --- Readline configuration ---

(defvar *history-file*
  (namestring (merge-pathnames "history"
                               (merge-pathnames ".crichton/"
                                                (user-homedir-pathname))))
  "Path to the readline history file.")

(defvar *history-max-entries* 1000
  "Maximum number of history entries to keep.")

;;; --- State ---

(defvar *daemon-socket-path*
  (namestring (merge-pathnames "daemon.sock"
                               (merge-pathnames ".crichton/"
                                                (user-homedir-pathname))))
  "Default path to the daemon Unix socket.")

(defvar *client-socket* nil "The connected sb-bsd-sockets socket.")
(defvar *client-stream* nil "Character stream over the socket.")
(defvar *session-id* nil "Current chat session ID.")
(defvar *next-id* 0 "Correlation ID counter.")

;;; --- JSON helpers (minimal, matching rpc/protocol.lisp style) ---

(defun json-to-string (object)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (shasht:write-json object s))))

(defun string-to-json (string)
  (shasht:read-json string))

;;; --- Wire I/O ---

(defun write-message (stream message)
  (write-string (json-to-string message) stream)
  (terpri stream)
  (finish-output stream))

(defun read-message (stream)
  (let ((line (read-line stream nil nil)))
    (when line
      (string-to-json line))))

;;; --- Connection ---

(defun connect-daemon (&optional socket-path)
  "Connect to the Crichton daemon socket. Returns T on success."
  (let ((path (or socket-path *daemon-socket-path*)))
    (unless (probe-file path)
      (error "Daemon socket not found at ~A~%Is the daemon running?" path))
    (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (handler-case
          (sb-bsd-sockets:socket-connect sock path)
        (error (c)
          (sb-bsd-sockets:socket-close sock)
          (error "Cannot connect to daemon at ~A: ~A" path c)))
      (setf *client-socket* sock
            *client-stream* (sb-bsd-sockets:socket-make-stream
                             sock :input t :output t
                             :element-type 'character
                             :buffering :line
                             :external-format :utf-8))
      t)))

(defun disconnect ()
  (when *client-stream*
    (ignore-errors (close *client-stream*))
    (setf *client-stream* nil))
  (when *client-socket*
    (ignore-errors (sb-bsd-sockets:socket-close *client-socket*))
    (setf *client-socket* nil)))

;;; --- Request/response ---

(defun next-id ()
  (incf *next-id*))

(defun send-request (op &rest pairs)
  "Send an NDJSON request and read the response.
   Signals an error if the response indicates failure."
  (let ((ht (make-hash-table :test #'equal))
        (id (next-id)))
    (setf (gethash "id" ht) id
          (gethash "op" ht) op)
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    (write-message *client-stream* ht)
    (let ((response (read-message *client-stream*)))
      (unless response
        (error "Connection closed by daemon"))
      (unless (gethash "ok" response)
        (let ((err (gethash "error" response)))
          (error "Daemon error [~A]: ~A"
                 (if err (gethash "code" err) "unknown")
                 (if err (gethash "message" err) "unknown error"))))
      response)))

(defun send-chat (text &optional session-id)
  "Send a chat message. Returns (values response-text session-id)."
  (let* ((sid (or session-id *session-id*))
         (response (if sid
                       (send-request "chat" "text" text "session_id" sid)
                       (send-request "chat" "text" text)))
         (result (gethash "result" response))
         (response-text (gethash "text" result))
         (new-sid (gethash "session_id" result)))
    (when new-sid
      (setf *session-id* new-sid))
    (values response-text new-sid)))

;;; --- History management ---

(defun load-history ()
  "Load readline history from file if it exists."
  (when (probe-file *history-file*)
    (handler-case
        (rl:read-history *history-file*)
      (error (c)
        (format *error-output* "Warning: Could not load history: ~A~%" c)))))

(defun save-history ()
  "Save readline history to file, truncating to max entries if needed."
  (handler-case
      (progn
        ;; Ensure the .crichton directory exists
        (let ((history-dir (directory-namestring *history-file*)))
          (unless (probe-file history-dir)
            (ensure-directories-exist history-dir)))
        ;; Write current history
        (rl:write-history *history-file*)
        ;; Manual truncation: read the file, keep last N lines, rewrite
        (when (probe-file *history-file*)
          (let ((lines (with-open-file (in *history-file* :direction :input
                                           :if-does-not-exist nil)
                         (when in
                           (loop for line = (read-line in nil nil)
                                 while line
                                 collect line)))))
            (when (> (length lines) *history-max-entries*)
              (let ((truncated (subseq lines (- (length lines) *history-max-entries*))))
                (with-open-file (out *history-file* :direction :output
                                     :if-exists :supersede)
                  (dolist (line truncated)
                    (write-line line out))))))))
      (error (c)
        (format *error-output* "Warning: Could not save history: ~A~%" c))))

;;; --- REPL ---

(defun chat-repl (&key socket-path)
  "Interactive chat REPL with readline support. Connect to daemon, loop reading user input."
  (connect-daemon socket-path)
  (unwind-protect
       (progn
         ;; Initialize readline
         (rl:register-function :complete nil)  ; Disable tab completion for now
         (load-history)

         (format t "Connected to Crichton daemon. Type :quit to exit.~%~%")
         (loop
           (let ((line (rl:readline :prompt "> " :add-history t)))
             (when (null line)
               ;; EOF (Ctrl-D)
               (terpri)
               (return))
             (let ((trimmed (string-trim '(#\Space #\Tab) line)))
               (when (or (string-equal trimmed ":quit")
                         (string-equal trimmed ":exit")
                         (string-equal trimmed ":q"))
                 (return))
               (unless (zerop (length trimmed))
                 (handler-case
                     (multiple-value-bind (response-text)
                         (send-chat trimmed)
                       (format t "~&~A~%~%" response-text))
                   (error (c)
                     (format *error-output* "Error: ~A~%" c))))))))
    (save-history)
    (disconnect)))

;;; --- CLI entry point ---

(defun main ()
  "CLI entry point for the crichton-client binary."
  (let ((args (rest sb-ext:*posix-argv*)))
    (handler-case
        (if args
            (let ((text (format nil "~{~A~^ ~}" args)))
              (connect-daemon)
              (unwind-protect
                   (multiple-value-bind (response-text)
                       (send-chat text)
                     (format t "~A~%" response-text))
                (disconnect)))
            (chat-repl))
      (error (c)
        (format *error-output* "~A~%" c)
        (sb-ext:exit :code 1)))))
