;;;; sessions/store.lisp
;;;;
;;;; Encrypted session storage.
;;;; Sessions are conversation transcripts stored as age-encrypted JSON files.
;;;; Skills cannot access sessions — this is enforced structurally
;;;; (no RPC endpoint, no host function).
;;;;
;;;; File layout: ~/.crichton/sessions/<session-id>.age
;;;; Plaintext: JSON object with id, created_at, messages, metadata.

(in-package #:crichton/sessions)

(defvar *sessions-dir* nil
  "Directory for session files. Set during initialization.")

(defun sessions-dir ()
  "Return the sessions directory, initializing if needed."
  (or *sessions-dir*
      (setf *sessions-dir*
            (let ((dir (merge-pathnames #p"sessions/"
                                        crichton/config:*agent-home*)))
              (ensure-directories-exist dir)
              #+sbcl (sb-posix:chmod (namestring dir) #o700)
              dir))))

;;; --- Session ID generation ---

(defun generate-session-id ()
  "Generate a unique session ID. Uses ironclad for random bytes."
  (let ((bytes (ironclad:random-data 16)))
    (ironclad:byte-array-to-hex-string bytes)))

;;; --- Timestamp ---

(defun unix-timestamp ()
  "Return current Unix timestamp as an integer."
  (- (get-universal-time) 2208988800))

;;; --- Session operations ---

(defun session-file-path (session-id)
  "Return the .age file path for SESSION-ID."
  (merge-pathnames (make-pathname :name session-id :type "age")
                   (sessions-dir)))

(defun encryption-enabled-p ()
  "Return T if session encryption is configured (default: T)."
  (crichton/config:config-section-get :sessions :encrypt t))

(defun no-store-mode-p ()
  "Return T if sessions are in no-store mode (retention-days = 0)."
  (let ((days (crichton/config:config-section-get :sessions :retention-days 30)))
    (and (numberp days) (zerop days))))

(defun session-to-json-bytes (session-plist)
  "Serialize a session plist to JSON bytes."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on session-plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht) v))
    (sb-ext:string-to-octets
     (let ((*print-pretty* nil))
       (with-output-to-string (s)
         (shasht:write-json ht s)))
     :external-format :utf-8)))

(defun safe-intern-keyword (key)
  "Intern KEY as a keyword after validating length and character set.
   Rejects keys that could exhaust the keyword package via untrusted JSON."
  (let ((ukey (string-upcase key)))
    (when (or (> (length ukey) 64)
              (zerop (length ukey))
              (not (every (lambda (c)
                            (or (alpha-char-p c) (digit-char-p c)
                                (char= c #\-) (char= c #\_)))
                          ukey)))
      (error "Invalid JSON key for keyword interning: ~S" key))
    (intern ukey :keyword)))

(defun json-bytes-to-session (bytes)
  "Deserialize JSON bytes to a session plist."
  (let* ((json-string (sb-ext:octets-to-string bytes :external-format :utf-8))
         (ht (shasht:read-json json-string)))
    (when (hash-table-p ht)
      (let (result)
        (maphash (lambda (k v)
                   (push v result)
                   (push (safe-intern-keyword k) result))
                 ht)
        result))))

;;; --- Create / Save ---

(defun create-session (&key (metadata nil))
  "Create a new session. Returns a plist with :ID, :CREATED-AT, :MESSAGES, :METADATA.
   Does NOT persist — call save-session to write to disk."
  (list :id (generate-session-id)
        :created-at (crichton/config:iso8601-now)
        :messages (make-array 0 :adjustable t :fill-pointer 0)
        :metadata (or metadata (make-hash-table :test #'equal))))

(defun save-session (session)
  "Persist a session to disk. Encrypts if encryption is enabled.
   In no-store mode, this is a no-op.
   Returns the session ID."
  (when (no-store-mode-p)
    (return-from save-session (getf session :id)))
  (let ((id (getf session :id)))
    (crichton/crypto:with-secret-bytes (plaintext (session-to-json-bytes session))
      (let ((path (session-file-path id)))
        (if (encryption-enabled-p)
            (crichton/crypto:encrypt-to-file plaintext path)
            (progn
              (ensure-directories-exist path)
              (with-open-file (s path :direction :output
                                      :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
                (write-sequence plaintext s))
              #+sbcl (sb-posix:chmod (namestring (truename path)) #o600)))
        (log:info "Session saved: ~A" id)
        id))))

;;; --- Load ---

(defun ensure-adjustable-vector (seq)
  "Ensure SEQ is an adjustable vector with a fill-pointer.
   Converts from list, simple-vector, or NIL as needed."
  (let* ((v (cond ((null seq) #())
                  ((listp seq) (coerce seq 'vector))
                  ((vectorp seq) seq)
                  (t (error "Expected sequence, got ~S" (type-of seq)))))
         (result (make-array (length v) :adjustable t :fill-pointer (length v))))
    (replace result v)
    result))

(defun load-session (session-id)
  "Load a session from disk. Decrypts if needed.
   Signals an error if the session does not exist."
  (let ((path (session-file-path session-id)))
    (unless (probe-file path)
      (error "Session not found: ~A" session-id))
    (let ((plaintext
            (if (encryption-enabled-p)
                (crichton/crypto:decrypt-from-file path)
                (crichton/crypto:read-file-bytes path))))
      (let ((session (json-bytes-to-session plaintext)))
        (setf (getf session :messages)
              (ensure-adjustable-vector (getf session :messages)))
        session))))

;;; --- List ---

(defun list-sessions ()
  "Return a list of all session IDs on disk."
  (let ((pattern (merge-pathnames
                  (make-pathname :name :wild :type "age")
                  (sessions-dir))))
    (mapcar #'pathname-name (directory pattern))))

;;; --- Delete ---

(defun delete-session (session-id)
  "Delete a session from disk. Returns T if deleted, NIL if not found."
  (let ((path (session-file-path session-id)))
    (when (probe-file path)
      (delete-file path)
      (log:info "Session deleted: ~A" session-id)
      t)))

;;; --- Add message ---

(defun add-message (session role content)
  "Add a message to SESSION (in-memory). ROLE is :user, :assistant, etc.
   Call save-session to persist."
  (let ((messages (getf session :messages))
        (msg (make-hash-table :test #'equal)))
    (setf (gethash "role" msg) (string-downcase (symbol-name role))
          (gethash "content" msg) content
          (gethash "timestamp" msg) (crichton/config:iso8601-now))
    (vector-push-extend msg messages)
    session))
