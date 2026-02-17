;;;; tui/protocol.lisp
;;;;
;;;; Daemon wire protocol for the Crichton TUI client.
;;;; Async-oriented NDJSON communication over Unix domain socket.
;;;; Separates message construction from sending for TUI I/O thread use.

(in-package #:crichton-tui)

;;; --- State ---

(defvar *daemon-socket-path*
  (namestring (merge-pathnames "daemon.sock"
                               (merge-pathnames ".crichton/"
                                                (user-homedir-pathname))))
  "Default path to the daemon Unix socket.")

(defvar *client-socket* nil "The connected sb-bsd-sockets socket.")
(defvar *client-stream* nil "Character stream over the socket.")
(defvar *next-id* 0 "Correlation ID counter.")

;;; --- JSON helpers ---

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
  "Read one NDJSON line. Returns hash-table or NIL on EOF."
  (let ((line (read-line stream nil nil)))
    (when line
      (string-to-json line))))

;;; --- Connection ---

(defun connect-daemon (&optional socket-path)
  "Connect to the Crichton daemon socket. Returns the stream."
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
      *client-stream*)))

(defun disconnect ()
  (when *client-stream*
    (ignore-errors (close *client-stream*))
    (setf *client-stream* nil))
  (when *client-socket*
    (ignore-errors (sb-bsd-sockets:socket-close *client-socket*))
    (setf *client-socket* nil)))

;;; --- Correlation IDs ---

(defun next-id ()
  (incf *next-id*))

;;; --- Message construction ---

(defun make-chat-request (text &key session-id stream)
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (next-id)
          (gethash "op" ht) "chat"
          (gethash "text" ht) text)
    (when session-id
      (setf (gethash "session_id" ht) session-id))
    (when stream
      (setf (gethash "stream" ht) t))
    ht))

(defun make-status-request ()
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (next-id)
          (gethash "op" ht) "status")
    ht))

(defun make-subscribe-request ()
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (next-id)
          (gethash "op" ht) "subscribe")
    ht))

;;; --- Synchronous helper (one-shot CLI mode) ---

(defun send-chat-sync (text &optional session-id)
  "Send a chat request synchronously. Returns (values response-text session-id).
For one-shot CLI mode only; TUI mode uses async I/O."
  (let ((request (make-chat-request text :session-id session-id)))
    (write-message *client-stream* request)
    (let ((response (read-message *client-stream*)))
      (unless response
        (error "Connection closed by daemon"))
      (unless (gethash "ok" response)
        (let ((err (gethash "error" response)))
          (error "Daemon error [~A]: ~A"
                 (if err (gethash "code" err) "unknown")
                 (if err (gethash "message" err) "unknown error"))))
      (let ((result (gethash "result" response)))
        (values (gethash "text" result)
                (gethash "session_id" result))))))
