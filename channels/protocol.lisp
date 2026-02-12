;;;; channels/protocol.lisp
;;;;
;;;; Generic channel adapter protocol (CLOS).
;;;; Each messaging platform (Discord, Mastodon, IRC, etc.) implements
;;;; these generics. The channel manager wires adapters into the agent loop.

(in-package #:crichton/channels)

;;; --- Incoming message representation ---

(defclass channel-message ()
  ((id :initarg :id
       :initform nil
       :accessor channel-message-id)
   (text :initarg :text
         :initform ""
         :type string
         :accessor channel-message-text)
   (author-id :initarg :author-id
              :initform nil
              :accessor channel-message-author-id)
   (author-name :initarg :author-name
                :initform nil
                :accessor channel-message-author-name)
   (channel-id :initarg :channel-id
               :initform nil
               :accessor channel-message-channel-id)
   (guild-id :initarg :guild-id
             :initform nil
             :accessor channel-message-guild-id)
   (raw :initarg :raw
        :initform nil
        :accessor channel-message-raw)))

(defun make-channel-message (&key id (text "") author-id author-name channel-id guild-id raw)
  (make-instance 'channel-message
                 :id id
                 :text text
                 :author-id author-id
                 :author-name author-name
                 :channel-id channel-id
                 :guild-id guild-id
                 :raw raw))

;;; --- Channel adapter protocol ---

(defgeneric channel-connect (channel)
  (:documentation "Connect the channel adapter. Starts background threads as needed.
   Returns T on success."))

(defgeneric channel-disconnect (channel)
  (:documentation "Disconnect the channel adapter. Stops background threads cleanly."))

(defgeneric channel-send (channel destination message)
  (:documentation "Send MESSAGE (a string) to DESTINATION (platform-specific ID)."))

(defgeneric channel-set-handler (channel handler)
  (:documentation "Set the message handler function. HANDLER takes two args:
   (channel channel-message) and is called for each incoming message."))

(defgeneric channel-name (channel)
  (:documentation "Return a human-readable name for this channel adapter."))
