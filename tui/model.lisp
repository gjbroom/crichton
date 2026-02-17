;;;; tui/model.lisp
;;;;
;;;; Application state for the Crichton TUI client.
;;;; Uses CLOS classes for hot-reload compatibility.

(in-package #:crichton-tui)

;;; --- Chat history entry ---

(defclass chat-entry ()
  ((role     :initarg :role
             :accessor entry-role
             :type (member :user :assistant :error :notification))
   (text     :initarg :text
             :accessor entry-text
             :type string)
   (time     :initarg :time
             :accessor entry-time)
   (rendered :initarg :rendered
             :initform nil
             :accessor entry-rendered)))

;;; --- Notification history entry ---

(defclass notification-entry ()
  ((kind   :initarg :kind   :accessor notif-kind)
   (text   :initarg :text   :accessor notif-text)
   (source :initarg :source :accessor notif-source)
   (time   :initarg :time   :accessor notif-time)))

;;; --- Top-level TUI model ---

(defclass tui-model ()
  ((messages           :initarg :messages
                       :initform nil
                       :accessor model-messages)
   (input              :initarg :input
                       :initform nil
                       :accessor model-input)
   (viewport           :initarg :viewport
                       :initform nil
                       :accessor model-viewport)
   (spinner            :initarg :spinner
                       :initform nil
                       :accessor model-spinner)
   (status             :initarg :status
                       :initform :idle
                       :accessor model-status
                       :type (member :idle :waiting :error))
   (status-text        :initarg :status-text
                       :initform ""
                       :accessor model-status-text
                       :type string)
   (session-id         :initarg :session-id
                       :initform nil
                       :accessor model-session-id)
   (daemon-stream      :initarg :daemon-stream
                       :initform nil
                       :accessor model-daemon-stream)
   (width              :initarg :width
                       :initform 80
                       :accessor model-width
                       :type integer)
   (height             :initarg :height
                       :initform 24
                       :accessor model-height
                       :type integer)
   (show-notifications :initarg :show-notifications
                       :initform nil
                       :accessor model-show-notifications)
   (notifications      :initarg :notifications
                       :initform nil
                       :accessor model-notifications)
   (notification-history :initarg :notification-history
                         :initform nil
                         :accessor model-notification-history)
   (confirm-quit       :initarg :confirm-quit
                       :initform nil
                       :accessor model-confirm-quit)
   (streaming-entry    :initarg :streaming-entry
                       :initform nil
                       :accessor model-streaming-entry
                       :documentation "The chat-entry currently being streamed into, or NIL.")
   (handled-stream-ids :initarg :handled-stream-ids
                       :initform nil
                       :accessor model-handled-stream-ids
                       :documentation "Request IDs already handled by streaming (to skip duplicate responses).")
   (sixel-enabled      :initarg :sixel-enabled
                       :initform nil
                       :accessor model-sixel-enabled
                       :documentation "Whether sixel rendering is enabled (:on :off :auto).")
   (sixel-cache        :initarg :sixel-cache
                       :initform (make-hash-table :test #'equal)
                       :accessor model-sixel-cache
                       :documentation "Cache of path → sixel string.")))
