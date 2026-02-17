;;;; tui/messages.lisp
;;;;
;;;; Custom message types for the Crichton TUI client.
;;;; These messages are injected by the daemon I/O thread and
;;;; internal commands into the TUI event loop.

(in-package #:crichton-tui)

;;; --- Daemon response (chat reply or error) ---

(tui:defmessage daemon-response-msg
  ((id        :initarg :id        :accessor msg-response-id)
   (text      :initarg :text      :accessor msg-response-text)
   (session   :initarg :session   :accessor msg-response-session)
   (error-p   :initarg :error-p   :initform nil :accessor msg-response-error-p)))

;;; --- Daemon notification (skill events, system alerts) ---

(tui:defmessage daemon-notification-msg
  ((kind      :initarg :kind      :accessor msg-notif-kind)
   (text      :initarg :text      :accessor msg-notif-text)
   (source    :initarg :source    :accessor msg-notif-source)))

;;; --- Daemon connection lost ---

(tui:defmessage daemon-disconnected-msg ())

;;; --- User sends a chat message ---

(tui:defmessage send-chat-msg
  ((text :initarg :text :accessor msg-chat-text)))
