;;;; skills/builtins/hoobs.lisp
;;;;
;;;; Built-in skill: HOOBS home automation hub (hoobs.com).
;;;; Connects to a local HOOBS hub via its REST API.
;;;; Supports listing/controlling accessories, rooms, and querying hub status.
;;;;
;;;; Credentials: store as 'hoobs' with fields :host, :port (opt, default 80),
;;;; :username, :password:
;;;;   (crichton/credentials:store-credential
;;;;    "hoobs" '(:host "192.168.1.10" :username "admin" :password "secret"))
;;;;
;;;; Config (optional overrides):
;;;;   [hoobs]
;;;;   host = "192.168.1.10"
;;;;   port = 80
;;;;   allow_control = true   ; required to use set-accessory actions
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

(defparameter *hoobs-credential-name* "hoobs"
  "Credential store name for HOOBS connection details.")

;;; --- Session token cache ---

(defvar *hoobs-token* nil
  "Cached HOOBS session token.  Cleared on 401 and on re-authentication.")

;;; --- Config helpers ---

(defun hoobs-host ()
  "Return HOOBS host from config or credential store."
  (or (crichton/config:config-section-get :hoobs :host)
      (handler-case
          (getf (crichton/credentials:resolve-credential *hoobs-credential-name*) :host)
        (error () nil))
      (error "HOOBS host not configured.  Set [hoobs] host in config.toml or store in credentials.")))

(defun hoobs-port ()
  "Return HOOBS port from config or credential store (default 80)."
  (or (crichton/config:config-section-get :hoobs :port)
      (handler-case
          (getf (crichton/credentials:resolve-credential *hoobs-credential-name*) :port)
        (error () nil))
      80))

(defun hoobs-base-url ()
  "Build the HOOBS API base URL."
  (format nil "http://~A:~A/api" (hoobs-host) (hoobs-port)))

(defun hoobs-control-allowed-p ()
  "Return T if control operations are enabled in config."
  (crichton/config:config-section-get :hoobs :allow-control nil))

;;; --- Authentication ---

(defun hoobs-authenticate ()
  "Authenticate with HOOBS and cache the session token.
   Returns the token string."
  (setf *hoobs-token* nil)
  (let* ((cred (handler-case
                   (crichton/credentials:resolve-credential *hoobs-credential-name*)
                 (error (c)
                   (error "HOOBS credentials not found: ~A" c))))
         (username (getf cred :username))
         (password (getf cred :password))
         (body (make-hash-table :test #'equal)))
    (unless (and username password)
      (error "HOOBS credentials missing :username or :password fields."))
    (setf (gethash "username" body) username
          (gethash "password" body) password)
    (log:info "HOOBS: authenticating as ~A" username)
    (multiple-value-bind (response-body status)
        (dex:post (format nil "~A/auth/login" (hoobs-base-url))
                  :headers '(("Content-Type" . "application/json"))
                  :content (shasht:write-json body nil))
      (unless (= status 200)
        (error "HOOBS authentication failed (HTTP ~D)" status))
      (let* ((result (shasht:read-json response-body))
             (token (gethash "token" result)))
        (unless token
          (error "HOOBS authentication response contained no token"))
        (setf *hoobs-token* token)
        (log:info "HOOBS: authenticated")
        token))))

(defun hoobs-ensure-token ()
  "Return a valid HOOBS token, authenticating if needed."
  (or *hoobs-token* (hoobs-authenticate)))

;;; --- HTTP helpers ---

(defun hoobs-request (method path &key body)
  "Make an authenticated request to the HOOBS API.
   Re-authenticates once on 401.  METHOD is :get, :put, :post.
   Retries transient failures with exponential backoff."
  (let ((url (format nil "~A~A" (hoobs-base-url) path))
        (token (hoobs-ensure-token)))
    (flet ((do-request (tok)
             (let ((headers (list (cons "Authorization" tok)
                                  (cons "Content-Type" "application/json")))
                   (json-body (when body (shasht:write-json body nil))))
               (ecase method
                 (:get
                  (http-get-with-retry url :headers headers))
                 (:put
                  (http-put-with-retry url :headers headers :content json-body))
                 (:post
                  (http-post-with-retry url :headers headers :content json-body))))))
      (multiple-value-bind (response-body status)
          (do-request token)
        (when (= status 401)
          ;; Token expired — re-auth and retry once
          (log:info "HOOBS: token expired, re-authenticating")
          (setf *hoobs-token* nil)
          (multiple-value-setq (response-body status)
            (do-request (hoobs-authenticate))))
        (unless (< status 400)
          (error "HOOBS API error ~D for ~A ~A" status method path))
        (when (plusp (length response-body))
          (shasht:read-json response-body))))))

;;; --- Accessories ---

(defun hoobs-accessories ()
  "Return all accessories grouped by room.
   Returns a list of room plists, each with :name and :accessories."
  (let ((data (hoobs-request :get "/accessories")))
    (when (vectorp data)
      (map 'list
           (lambda (room)
             (list :name (gethash "name" room)
                   :accessories
                   (let ((acc (gethash "accessories" room)))
                     (when (vectorp acc)
                       (map 'list
                            (lambda (a)
                              (list :id   (gethash "accessory_identifier" a)
                                    :name (gethash "name" a)
                                    :type (gethash "type" a)
                                    :room (gethash "name" room)))
                            acc)))))
           data))))

(defun hoobs-get-accessory (accessory-id)
  "Get details and characteristics for a single accessory."
  (hoobs-request :get (format nil "/accessories/~A" accessory-id)))

(defun hoobs-set-accessory (accessory-id characteristics)
  "Set characteristic values on an accessory.
   CHARACTERISTICS is a hash-table of characteristic-name → value.
   Requires [hoobs] allow_control = true in config."
  (unless (hoobs-control-allowed-p)
    (error "HOOBS control operations are disabled.  Set [hoobs] allow_control = true to enable."))
  (log:info "HOOBS: setting accessory ~A" accessory-id)
  (hoobs-request :put (format nil "/accessories/~A" accessory-id)
                 :body characteristics))

;;; --- Rooms ---

(defun hoobs-rooms ()
  "Return a list of room plists with :id and :name."
  (let ((data (hoobs-request :get "/rooms")))
    (when (vectorp data)
      (map 'list
           (lambda (r)
             (list :id   (gethash "id" r)
                   :name (gethash "name" r)))
           data))))

;;; --- Hub status ---

(defun hoobs-service-status ()
  "Return HOOBS hub service status as a plist."
  (let ((data (hoobs-request :get "/service")))
    (when (hash-table-p data)
      (list :status   (gethash "status" data)
            :version  (gethash "version" data)
            :running  (gethash "running" data)
            :bridges  (gethash "bridges" data)))))

;;; --- Report ---

(defun hoobs-report (&key (stream *standard-output*))
  "Print a formatted HOOBS status report."
  (let ((s stream))
    (handler-case
        (let ((status (hoobs-service-status)))
          (format s "~&HOOBS Hub (~A:~A)~%" (hoobs-host) (hoobs-port))
          (format s "  Status: ~A  Version: ~A  Running: ~A~%"
                  (or (getf status :status) "?")
                  (or (getf status :version) "?")
                  (if (getf status :running) "yes" "no")))
      (error (c)
        (format s "~&HOOBS status unavailable: ~A~%" c)))
    (handler-case
        (let ((rooms (hoobs-accessories)))
          (format s "~&Accessories:~%")
          (dolist (room rooms)
            (format s "  ~A~%" (getf room :name))
            (dolist (acc (getf room :accessories))
              (format s "    ~A (~A)~%" (getf acc :name) (or (getf acc :type) "?")))))
      (error (c)
        (format s "~&Accessories unavailable: ~A~%" c)))))
