;;;; daemon/notifications.lisp
;;;;
;;;; Notification registry: tracks subscribed client connections and
;;;; delivers push notifications.  Each subscriber entry stores both a
;;;; stream and a per-stream write lock so notification delivery can
;;;; serialize with regular RPC response writes.

(in-package #:crichton/daemon)

;;; --- Subscriber registry ---

(defvar *notification-subscribers* nil
  "List of subscriber entries (stream . lock) pairs.")

(defvar *notification-subscribers-lock* (bt:make-lock "notification-subscribers")
  "Protects *notification-subscribers*.")

(defun add-subscriber (stream lock)
  "Register STREAM (with its write LOCK) for push notifications."
  (bt:with-lock-held (*notification-subscribers-lock*)
    (pushnew (cons stream lock) *notification-subscribers*
             :key #'car))
  (log:info "Notification subscriber added"))

(defun remove-subscriber (stream)
  "Unregister STREAM from push notifications."
  (bt:with-lock-held (*notification-subscribers-lock*)
    (let ((before (length *notification-subscribers*)))
      (setf *notification-subscribers*
            (remove stream *notification-subscribers* :key #'car))
      (when (< (length *notification-subscribers*) before)
        (log:info "Notification subscriber removed")))))

(defun notification-post (kind text source)
  "Send a notification to all subscribers.  Dead subscribers are removed.
   KIND, TEXT, and SOURCE are strings included in the notification message."
  (let ((msg (make-hash-table :test #'equal))
        (dead nil))
    (setf (gethash "op" msg) "notify"
          (gethash "kind" msg) kind
          (gethash "text" msg) text
          (gethash "source" msg) source)
    (let ((subscribers (bt:with-lock-held (*notification-subscribers-lock*)
                         (copy-list *notification-subscribers*))))
      (dolist (entry subscribers)
        (destructuring-bind (stream . lock) entry
          (handler-case
              (bt:with-lock-held (lock)
                (crichton/rpc:write-message stream msg))
            (error (c)
              (log:warn "Notification write failed, removing subscriber: ~A" c)
              (push stream dead))))))
    (when dead
      (dolist (stream dead)
        (remove-subscriber stream)))))
