;;;; agent/tools-data/hoobs.lisp
;;;;
;;;; Tool definition for HOOBS HomeKit hub control.

(in-package #:crichton/agent)

;;; --- HOOBS home automation tool ---

(define-tool hoobs
    (:description "Control HomeKit accessories via a HOOBS hub.  Actions: 'status' (hub status), 'rooms' (list rooms), 'accessories' (list all accessories by room), 'get_accessory' (details for one accessory), 'set_accessory' (set characteristic values — requires [hoobs] allow_control = true in config).  Requires 'hoobs' credential with :host, :username, :password and optionally :port.")
  ((action "string"
           "The HOOBS action to perform."
           :enum ("status" "rooms" "accessories" "get_accessory" "set_accessory")
           :required-p t)
   (accessory-id "string"
                 "Accessory identifier.  Required for get_accessory and set_accessory.")
   (characteristics "object"
                    "Hash of characteristic-name → value to set.  Required for set_accessory (e.g., {\"on\": true})."))
  (cond
    ((string-equal action "status")
     (with-output-to-string (s)
       (crichton/skills:hoobs-report :stream s)))
    ((string-equal action "rooms")
     (let ((rooms (crichton/skills:hoobs-rooms)))
       (if rooms
           (format nil "~{~A (id: ~A)~^~%~}"
                   (loop for r in rooms
                         collect (getf r :name)
                         collect (getf r :id)))
           "No rooms found.")))
    ((string-equal action "accessories")
     (with-output-to-string (s)
       (crichton/skills:hoobs-report :stream s)))
    ((string-equal action "get_accessory")
     (unless accessory-id
       (return-from handler "Error: 'accessory_id' is required for get_accessory."))
     (let ((data (crichton/skills:hoobs-get-accessory accessory-id)))
       (if data
           (format nil "~S" data)
           (format nil "Accessory '~A' not found." accessory-id))))
    ((string-equal action "set_accessory")
     (unless accessory-id
       (return-from handler "Error: 'accessory_id' is required for set_accessory."))
     (unless characteristics
       (return-from handler "Error: 'characteristics' is required for set_accessory."))
     (handler-case
         (progn
           (crichton/skills:hoobs-set-accessory accessory-id characteristics)
           (format nil "Accessory '~A' updated." accessory-id))
       (error (c)
         (format nil "Error: ~A" c))))
    (t
     (format nil "Unknown HOOBS action: ~A" action))))
