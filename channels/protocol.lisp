;;;; channels/protocol.lisp
;;;;
;;;; Generic channel adapter protocol (CLOS).
;;;; Each messaging platform (Discord, Mastodon, IRC, etc.) implements
;;;; these generics. The channel manager wires adapters into the agent loop.

(in-package #:crichton/channels)

;;; --- Incoming message representation ---

(defstruct (channel-message (:constructor make-channel-message))
  (id nil)
  (text "" :type string)
  (author-id nil)
  (author-name nil)
  (channel-id nil)
  (guild-id nil)
  (raw nil))

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
