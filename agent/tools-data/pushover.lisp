;;;; agent/tools-data/pushover.lisp
;;;;
;;;; Tool definition for Pushover push notifications.

(in-package #:crichton/agent)

;;; --- Pushover notification tool ---

(define-tool pushover
    (:description "Send a push notification via Pushover.net to iOS, Android, or desktop devices.  Requires 'pushover' credential with :token and :user fields.  Priority: -2 lowest, -1 low, 0 normal (default), 1 high, 2 emergency.")
  ((message "string"
            "The notification message body."
            :required-p t)
   (title "string"
          "Notification title.  Defaults to the app name.")
   (priority "integer"
             "Notification priority: -2 (lowest) to 2 (emergency).  Default: 0 (normal).")
   (url "string"
        "Supplementary URL to attach to the notification.")
   (url-title "string"
              "Title for the supplementary URL.")
   (device "string"
           "Target device name.  Omit to send to all devices.")
   (sound "string"
          "Notification sound name (e.g. 'pushover', 'magic', 'alien')."))
  (pushover-report message
                                   :title title
                                   :priority priority
                                   :url url
                                   :url-title url-title
                                   :device device
                                   :sound sound))
