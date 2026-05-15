;;;; crypto/age.lisp
;;;;
;;;; Wrapper around the `age` CLI tool for file encryption/decryption.
;;;; Uses stdin/stdout pipes — never passes secrets as command-line arguments.
;;;;
;;;; Identity key management:
;;;;   ~/.crichton/identity.key — age identity file (0600)
;;;;   Recipient derived via `age-keygen -y`
;;;;
;;;; Requires: age (1.x) and age-keygen installed.
;;;; On Debian: apt install age

(in-package #:crichton/crypto)

;;; --- age binary detection ---

(defvar *age-binary* nil
  "Cached path to the age binary, or NIL if not yet searched.")

(defvar *age-keygen-binary* nil
  "Cached path to the age-keygen binary, or NIL if not yet searched.")

(defun find-binary (name)
  "Find a binary on PATH. Returns the path string or NIL.
   Uses /usr/bin/env for portability across Linux and macOS."
  (handler-case
      (let* ((process (sb-ext:run-program "/usr/bin/env" (list "which" name)
                                           :output :stream
                                           :error nil
                                           :wait t))
             (output (when (zerop (sb-ext:process-exit-code process))
                       (read-process-output-string process))))
        (sb-ext:process-close process)
        (when (and output (plusp (length output)) (probe-file output))
          output))
    (error () nil)))

(defun age-binary ()
  "Return path to the age binary, or signal an error."
  (or *age-binary*
      (setf *age-binary* (find-binary "age"))
      (error "age binary not found. Install with: apt install age")))

(defun age-keygen-binary ()
  "Return path to the age-keygen binary, or signal an error."
  (or *age-keygen-binary*
      (setf *age-keygen-binary* (find-binary "age-keygen"))
      (error "age-keygen binary not found. Install with: apt install age")))

(defun age-available-p ()
  "Return T if the age CLI tools are available on PATH."
  (and (find-binary "age")
       (find-binary "age-keygen")
       t))

;;; --- Process stream helpers ---

(defun read-stream-string (stream)
  "Read all lines from STREAM and return a trimmed string."
  (string-trim '(#\Newline #\Return #\Space)
               (with-output-to-string (s)
                 (loop for line = (read-line stream nil nil)
                       while line do (write-line line s)))))

(defun read-process-output-string (process)
  "Read the full output of PROCESS as a trimmed string.
   PROCESS must have been created with :output :stream."
  (read-stream-string (sb-ext:process-output process)))

(defun read-process-error-string (process)
  "Read the full error output of PROCESS as a trimmed string.
   PROCESS must have been created with :error :stream."
  (read-stream-string (sb-ext:process-error process)))

(defun read-all-bytes (stream)
  "Read all bytes from a binary STREAM into a simple octet vector."
  (let ((chunks nil)
        (total 0)
        (buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf stream)
          while (plusp n)
          do (push (subseq buf 0 n) chunks)
             (incf total n))
    (let ((result (make-array total :element-type '(unsigned-byte 8)))
          (offset 0))
      (dolist (chunk (nreverse chunks) result)
        (replace result chunk :start1 offset)
        (incf offset (length chunk))))))

;;; --- Identity key management ---

(defun identity-key-path ()
  "Return the path to ~/.crichton/identity.key."
  (merge-pathnames "identity.key" crichton/config:*agent-home*))

(defun identity-key-exists-p ()
  "Return T if the identity key file exists."
  (and (probe-file (identity-key-path)) t))

(defun ensure-identity-key ()
  "Ensure ~/.crichton/identity.key exists. Creates one via age-keygen if missing.
   Returns the path to the identity key."
  (let ((path (identity-key-path)))
    (unless (probe-file path)
      (ensure-directories-exist path)
      (let ((process (sb-ext:run-program
                      (age-keygen-binary)
                      (list "-o" (namestring path))
                      :output nil
                      :error :stream
                      :wait t)))
        (unless (zerop (sb-ext:process-exit-code process))
          (let ((err (read-process-error-string process)))
            (sb-ext:process-close process)
            (error "age-keygen failed: ~A" err)))
        (sb-ext:process-close process))
      (sb-posix:chmod (namestring path) #o600)
      (log:info "Created identity key: ~A" (namestring path)))
    path))

(defun identity-recipient ()
  "Derive the public recipient string from the identity key.
   Returns a string like 'age1...'."
  (let* ((key-path (ensure-identity-key))
         (process (sb-ext:run-program
                   (age-keygen-binary)
                   (list "-y" (namestring key-path))
                   :output :stream
                   :error :stream
                   :wait t)))
    (unless (zerop (sb-ext:process-exit-code process))
      (let ((err (read-process-error-string process)))
        (sb-ext:process-close process)
        (error "age-keygen -y failed: ~A" err)))
    (let ((recipient (read-process-output-string process)))
      (sb-ext:process-close process)
      (when (zerop (length recipient))
        (error "age-keygen -y returned empty recipient"))
      recipient)))

;;; --- Encrypt / Decrypt via pipes ---
;;;
;;; We use two separate process invocations:
;;; - Character-mode for error streams (readable)
;;; - Binary-mode for data streams (stdin/stdout)
;;;
;;; age --armor produces ASCII output, so we can use character streams
;;; for both input and output on the data side when using --armor.

(defun age-encrypt (plaintext-bytes)
  "Encrypt PLAINTEXT-BYTES (an octet vector) using age with the identity recipient.
   Returns ciphertext as an octet vector (ASCII-armored).
   Uses temp files for both input and output to avoid pipe buffer deadlocks
   on large payloads (> 64KB)."
  (let* ((recipient (identity-recipient))
         (tag (format nil "~A-~A" (get-universal-time) (random 1000000)))
         (tmp-dir (uiop:temporary-directory))
         (in-path  (merge-pathnames (format nil ".crichton-age-in-~A.tmp"  tag) tmp-dir))
         (out-path (merge-pathnames (format nil ".crichton-age-out-~A.tmp" tag) tmp-dir)))
    (unwind-protect
         (progn
           (with-open-file (s in-path :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
             (write-sequence plaintext-bytes s))
           (let* ((process (sb-ext:run-program
                            (age-binary)
                            (list "--encrypt" "--recipient" recipient "--armor"
                                  "--output" (namestring out-path)
                                  (namestring in-path))
                            :output nil
                            :error :stream
                            :wait t)))
             (unless (zerop (sb-ext:process-exit-code process))
               (let ((err (read-process-error-string process)))
                 (sb-ext:process-close process)
                 (error "age encrypt failed (exit ~D): ~A"
                        (sb-ext:process-exit-code process) err)))
             (sb-ext:process-close process)
             (read-file-bytes out-path)))
      (ignore-errors (delete-file in-path))
      (ignore-errors (delete-file out-path)))))

(defun age-decrypt (ciphertext-bytes)
  "Decrypt CIPHERTEXT-BYTES (an octet vector) using age with the identity key.
   Returns plaintext as an octet vector."
  (let* ((key-path (ensure-identity-key))
         (process (sb-ext:run-program
                   (age-binary)
                   (list "--decrypt" "--identity" (namestring key-path))
                   :input :stream
                   :output :stream
                   :error :stream
                   :wait nil)))
    (unwind-protect
         (progn
           (let ((input-string (sb-ext:octets-to-string ciphertext-bytes
                                                        :external-format :utf-8)))
             (write-string input-string (sb-ext:process-input process)))
           (close (sb-ext:process-input process))
           (sb-ext:process-wait process)
           (let ((output (read-process-output-string process)))
             (unless (zerop (sb-ext:process-exit-code process))
               (let ((err (read-process-error-string process)))
                 (error "age decrypt failed (exit ~D): ~A"
                        (sb-ext:process-exit-code process) err)))
             (sb-ext:string-to-octets output :external-format :utf-8)))
      (sb-ext:process-close process))))

;;; --- Convenience: string encrypt/decrypt ---

(defun encrypt-string (plaintext)
  "Encrypt a string using age. Returns ciphertext bytes."
  (age-encrypt (sb-ext:string-to-octets plaintext :external-format :utf-8)))

(defun decrypt-to-string (ciphertext-bytes)
  "Decrypt ciphertext bytes and return a string."
  (sb-ext:octets-to-string (age-decrypt ciphertext-bytes) :external-format :utf-8))

;;; --- File helpers ---

(defun encrypt-to-file (plaintext-bytes path)
  "Encrypt PLAINTEXT-BYTES and write the ciphertext to PATH (0600).
   Uses atomic write via temp file + rename to avoid partial writes."
  (let* ((ciphertext (age-encrypt plaintext-bytes))
         (tmp-path (make-pathname :name (format nil "~A.tmp" (pathname-name path))
                                  :defaults path)))
    (ensure-directories-exist path)
    (with-open-file (s tmp-path :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
      (write-sequence ciphertext s))
    (sb-posix:chmod (namestring (truename tmp-path)) #o600)
    (rename-file tmp-path path)
    path))

(defun decrypt-from-file (path)
  "Read ciphertext from PATH and decrypt. Returns plaintext bytes."
  (unless (probe-file path)
    (error "Encrypted file not found: ~A" path))
  (let ((ciphertext (read-file-bytes path)))
    (age-decrypt ciphertext)))

(defun read-file-bytes (path)
  "Read an entire file as an octet vector."
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence bytes s)
      bytes)))
