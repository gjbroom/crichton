;;;; skills/builtins/pushover.lisp
;;;;
;;;; Built-in skill: push notifications via Pushover.net.
;;;; Sends real-time notifications to iOS, Android, and desktop devices.
;;;;
;;;; Credentials: store as 'pushover' with fields :token (app key) and
;;;; :user (user/group key).  From the Pushover dashboard:
;;;;   (crichton/credentials:store-credential
;;;;    "pushover" '(:token "app-token-here" :user "user-key-here"))
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

(defparameter *pushover-api-url*
  "https://api.pushover.net/1/messages.json"
  "Pushover messages API endpoint.")

(defparameter *pushover-credential-name* "pushover"
  "Credential store name for Pushover tokens.")

;;; --- Credential resolution ---

(defun pushover-credentials ()
  "Resolve Pushover :token and :user from the credential store.
   Returns (values token user)."
  (let* ((cred (crichton/credentials:resolve-credential
                *pushover-credential-name*))
         (token (getf cred :token))
         (user  (getf cred :user)))
    (unless (and token user)
      (error "Pushover credentials incomplete.  Need :token and :user.
Store them with:
  (crichton/credentials:store-credential
   \"pushover\" '(:token \"app-token\" :user \"user-key\"))"))
    (values token user)))

;;; --- API ---

(defun pushover-send (message &key title priority url url-title device sound html)
  "Send a push notification via the Pushover API.
   MESSAGE is required.  Optional keyword args:
     TITLE        — notification title (default: app name)
     PRIORITY     — integer -2 (lowest) to 2 (emergency); default 0 (normal)
     URL          — supplementary URL
     URL-TITLE    — title for URL
     DEVICE       — target device name (default: all devices)
     SOUND        — notification sound name
     HTML         — T to enable HTML formatting in message
   Returns a plist with :status (1 = success) and :request (UUID)."
  (multiple-value-bind (token user) (pushover-credentials)
    (let ((body (make-hash-table :test #'equal)))
      (setf (gethash "token"   body) token
            (gethash "user"    body) user
            (gethash "message" body) message)
      (when title     (setf (gethash "title"     body) title))
      (when priority  (setf (gethash "priority"  body) priority))
      (when url       (setf (gethash "url"       body) url))
      (when url-title (setf (gethash "url_title" body) url-title))
      (when device    (setf (gethash "device"    body) device))
      (when sound     (setf (gethash "sound"     body) sound))
      (when html      (setf (gethash "html"      body) 1))
      (log:info "Sending Pushover notification: ~A" (or title message))
      (handler-case
          (multiple-value-bind (response-body status)
              (dex:post *pushover-api-url*
                        :headers '(("Content-Type" . "application/json"))
                        :content (shasht:write-json body nil))
            (unless (= status 200)
              (error "Pushover API returned HTTP ~D: ~A" status response-body))
            (let ((result (shasht:read-json response-body)))
              (list :status (gethash "status" result)
                    :request (gethash "request" result))))
        (error (c)
          (error "Pushover send failed: ~A" c))))))

(defun pushover-report (message &rest args &key title priority &allow-other-keys)
  "Send a push notification and return a human-readable status string."
  (declare (ignore priority))
  (handler-case
      (let ((result (apply #'pushover-send message args)))
        (if (eql (getf result :status) 1)
            (format nil "Notification sent~@[: ~A~]  (request ~A)"
                    title (getf result :request))
            (format nil "Pushover returned status ~A" (getf result :status))))
    (error (c)
      (format nil "Failed to send notification: ~A" c))))
