;;;; rpc/protocol.lisp
;;;;
;;;; NDJSON protocol for daemon↔runner RPC.
;;;; Size-capped line reading, JSON encode/decode via shasht,
;;;; base64 helpers for binary data over the wire.

(in-package #:crichton/rpc)

(defvar *max-message-bytes* (* 8 1024 1024)
  "Maximum size of a single NDJSON message in bytes (default 8 MB).")

(defvar *next-id* 0
  "Monotonically increasing correlation ID counter.")

(defun next-id ()
  "Return the next correlation ID."
  (incf *next-id*))

;;; --- Base64 helpers ---

(defun bytes-to-base64 (octets)
  "Encode an octet vector to a base64 string."
  (cl-base64:usb8-array-to-base64-string octets))

(defun base64-to-bytes (string)
  "Decode a base64 string to an octet vector."
  (cl-base64:base64-string-to-usb8-array string))

;;; --- JSON helpers ---

(defun json-to-string (object)
  "Serialize OBJECT to a JSON string (no pretty-printing).
   Hash-tables become JSON objects; lists become JSON arrays."
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (shasht:write-json object s))))

(defun string-to-json (string)
  "Parse a JSON string into Lisp objects.
   JSON objects become hash-tables with string keys.
   JSON arrays become vectors."
  (shasht:read-json string))

;;; --- Message construction ---

(defun make-request (op &rest pairs)
  "Build a request hash-table with a fresh correlation ID.
   OP is the operation string. PAIRS are key-value pairs to include.
   Returns (values hash-table id)."
  (let ((ht (make-hash-table :test #'equal))
        (id (next-id)))
    (setf (gethash "id" ht) id
          (gethash "op" ht) op)
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    (values ht id)))

(defun make-ok-response (id result)
  "Build a success response hash-table."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) id
          (gethash "ok" ht) t
          (gethash "result" ht) result)
    ht))

(defun make-error-response (id code message)
  "Build an error response hash-table."
  (let ((ht (make-hash-table :test #'equal))
        (err (make-hash-table :test #'equal)))
    (setf (gethash "code" err) code
          (gethash "message" err) message)
    (setf (gethash "id" ht) id
          (gethash "ok" ht) nil
          (gethash "error" ht) err)
    ht))

;;; --- Wire I/O ---

(defun write-message (stream message)
  "Write MESSAGE (a hash-table) as a single NDJSON line to STREAM.
   STREAM must be a character stream. Flushes after writing."
  (let ((line (json-to-string message)))
    (when (> (length (sb-ext:string-to-octets line :external-format :utf-8))
             *max-message-bytes*)
      (error "Outbound message exceeds ~A byte limit" *max-message-bytes*))
    (write-string line stream)
    (terpri stream)
    (finish-output stream)))

(defun read-message (stream)
  "Read a single NDJSON line from STREAM and parse it.
   Returns the parsed hash-table, or NIL on EOF.
   Signals an error if the line exceeds *max-message-bytes*."
  (let ((line (read-line stream nil nil)))
    (when (null line)
      (return-from read-message nil))
    (when (> (length (sb-ext:string-to-octets line :external-format :utf-8))
             *max-message-bytes*)
      (error "Inbound message exceeds ~A byte limit" *max-message-bytes*))
    (string-to-json line)))

;;; --- Accessors for parsed messages ---

(defun msg-id (msg)
  "Get the correlation ID from a parsed message."
  (gethash "id" msg))

(defun msg-op (msg)
  "Get the operation from a parsed request."
  (gethash "op" msg))

(defun msg-ok-p (msg)
  "Return T if the message is a success response."
  (gethash "ok" msg))

(defun msg-result (msg)
  "Get the result from a success response."
  (gethash "result" msg))

(defun msg-error (msg)
  "Get the error hash-table from an error response."
  (gethash "error" msg))

(defun msg-get (msg key)
  "Get an arbitrary field from a parsed message.
   JSON null (shasht deserializes as :NULL) is treated as absent, returning NIL."
  (let ((val (gethash key msg)))
    (if (eq val :null) nil val)))
