;;;; skills/signing.lisp
;;;;
;;;; Ed25519 signing and verification of skill WASM bundles

(in-package #:crichton/skills)

(defun key-id-from-public-bytes (pub-bytes)
  (ironclad:byte-array-to-hex-string (subseq pub-bytes 0 8)))

(defun trust-store-path ()
  (crichton/config:agent-path "trusted-keys"))

(defun pub-key-path (key-id)
  (merge-pathnames (make-pathname :name key-id :type "pub")
                   (trust-store-path)))

(defun write-key-file (path bytes)
  (with-open-file (s path :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
    (write-sequence bytes s))
  #+sbcl (sb-posix:chmod (namestring path) #o600)
  path)

(defun read-key-file (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence bytes s)
      bytes)))

(defun generate-signing-keypair (&key (private-key-path
                                       (merge-pathnames "signing-key.priv"
                                                        crichton/config:*agent-home*)))
  "Generate an Ed25519 keypair.
   Saves private key (64 bytes: 32-byte seed + 32-byte public) to PRIVATE-KEY-PATH.
   Saves public key (32 bytes) to trust store as {key-id}.pub.
   Returns (values key-id private-key-path public-key-path)."
  (multiple-value-bind (priv pub) (ironclad:generate-key-pair :ed25519)
    (declare (ignore pub))
    (let* ((priv-bytes (ironclad:ed25519-key-x priv))
           (pub-bytes (ironclad:ed25519-key-y priv))
           (key-id (key-id-from-public-bytes pub-bytes))
           (pub-path (pub-key-path key-id))
           (combined (make-array 64 :element-type '(unsigned-byte 8))))
      (replace combined priv-bytes)
      (replace combined pub-bytes :start1 32)
      (ensure-directories-exist private-key-path)
      (ensure-directories-exist pub-path)
      (write-key-file private-key-path combined)
      (write-key-file pub-path pub-bytes)
      (values key-id private-key-path pub-path))))

(defun load-private-key (path)
  "Load an Ed25519 private key from PATH (64 bytes: seed + public)."
  (let* ((bytes (read-key-file path))
         (priv-bytes (subseq bytes 0 32))
         (pub-bytes (subseq bytes 32 64)))
    (ironclad:make-private-key :ed25519 :x priv-bytes :y pub-bytes)))

(defun sign-skill-bytes (wasm-bytes private-key-path)
  "Sign WASM-BYTES with the Ed25519 private key at PRIVATE-KEY-PATH.
   Returns a base64-encoded signature string."
  (let* ((priv-key (load-private-key private-key-path))
         (data (coerce wasm-bytes '(simple-array (unsigned-byte 8) (*))))
         (signature (ironclad:sign-message priv-key data)))
    (cl-base64:usb8-array-to-base64-string signature)))

(defun verify-skill-signature (signature-b64 data-bytes public-key)
  "Verify a base64-encoded Ed25519 signature against DATA-BYTES using PUBLIC-KEY.
   Returns T if valid, NIL otherwise."
  (let ((sig-bytes (cl-base64:base64-string-to-usb8-array signature-b64))
        (data (coerce data-bytes '(simple-array (unsigned-byte 8) (*)))))
    (ironclad:verify-signature public-key data sig-bytes)))

(defun load-trusted-keys ()
  "Scan ~/.crichton/trusted-keys/*.pub and load all public keys.
   Returns a hash-table mapping key-id strings to ironclad public key objects."
  (let ((keys (make-hash-table :test #'equal))
        (pattern (merge-pathnames (make-pathname :name :wild :type "pub")
                                  (trust-store-path))))
    (dolist (path (directory pattern) keys)
      (let* ((pub-bytes (read-key-file path))
             (key-id (key-id-from-public-bytes pub-bytes))
             (pub-key (ironclad:make-public-key :ed25519 :y pub-bytes)))
        (setf (gethash key-id keys) pub-key)))))

(defun verify-skill-bundle (manifest wasm-bytes)
  "Verify a skill bundle's signature against trusted keys.
   MANIFEST is a plist with at least :SIGNATURE.
   WASM-BYTES is the raw skill WASM content.
   Returns (values valid-p key-id)."
  (let* ((skill-info (getf manifest :skill))
         (signature (getf skill-info :signature))
         (trusted (load-trusted-keys)))
    (when (or (null signature) (string= signature ""))
      (return-from verify-skill-bundle (values nil nil)))
    (maphash (lambda (key-id pub-key)
               (when (verify-skill-signature signature wasm-bytes pub-key)
                 (return-from verify-skill-bundle (values t key-id))))
             trusted)
    (values nil nil)))
