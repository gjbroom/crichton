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
   (confirm-quit       :initarg :confirm-quit
                       :initform nil
                       :accessor model-confirm-quit)))
