;;;; agent/sanitize.lisp
;;;;
;;;; Input sanitization for untrusted text entering the agent loop.
;;;; Detects and logs potential prompt injection markers from external
;;;; channels (Discord, RSS feeds, tool results).
;;;;
;;;; This is a heuristic defence layer, not a perfect filter.
;;;; Suspicious input is logged and audited but not silently dropped —
;;;; the agent still processes it, but the operator is informed.

(in-package #:crichton/agent)

;;; --- Limits ---

(defparameter +max-channel-input-length+ 8000
  "Maximum allowed length for user input from external channels
   (Discord, RSS-triggered messages, etc.).")

(defparameter +max-direct-input-length+ 32000
  "Maximum allowed length for user input from trusted local clients
   (TUI, REPL, crichton-client).")

;;; --- Injection marker detection ---

(defparameter *injection-marker-patterns*
  '("(?i)ignore\\s+(all\\s+)?(previous|prior)\\s+instructions?"
    "(?i)\\bsystem\\s*:\\s"
    "(?i)\\bnew\\s+instructions?\\s*:"
    "(?i)\\bforget\\s+(everything|all|prior)"
    "(?i)\\[/?INST\\]"
    "(?i)<\\|system\\|>"
    "(?i)<\\|user\\|>"
    "(?i)<\\|assistant\\|>"
    "(?i)\\bprompt\\s+injection\\b")
  "Regex patterns that may indicate prompt injection attempts in user input.
   Detection is heuristic: matches are audit-logged but input is NOT rejected.")

(defun %strip-control-chars (text)
  "Remove null bytes and non-printable control characters from TEXT.
   Newline (#\\Newline), tab (#\\Tab), and carriage return (#\\Return) are kept."
  (with-output-to-string (s)
    (loop for ch across text
          when (or (char>= ch #\Space)
                   (char= ch #\Newline)
                   (char= ch #\Tab)
                   (char= ch #\Return))
            do (write-char ch s))))

(defun %count-injection-markers (text)
  "Return the number of distinct injection marker patterns matched in TEXT."
  (loop for pattern in *injection-marker-patterns*
        count (when (cl-ppcre:scan pattern text) 1)))

;;; --- Public API ---

(defun sanitize-user-input (text &key (max-length +max-channel-input-length+) source)
  "Sanitize TEXT before it enters the agent loop.

   Operations performed (in order):
     1. Return NIL immediately if TEXT is NIL.
     2. Strip null bytes and non-printable control characters.
     3. Truncate to MAX-LENGTH characters (warns if truncated).
     4. Detect potential injection markers; if any are found, writes an
        audit event and logs a warning.  Input is still processed.

   SOURCE is an optional string naming the origin of the input (used in
   log and audit messages).  Returns the sanitised string."
  (when (null text)
    (return-from sanitize-user-input nil))
  (let* ((stripped (%strip-control-chars text))
         (truncated (if (> (length stripped) max-length)
                        (progn
                          (log:warn "Input from ~A truncated: ~D → ~D chars"
                                    (or source "unknown") (length stripped) max-length)
                          (subseq stripped 0 max-length))
                        stripped))
         (marker-count (%count-injection-markers truncated)))
    (when (plusp marker-count)
      (let ((fields (make-hash-table :test #'equal)))
        (setf (gethash "source" fields) (or source "unknown")
              (gethash "marker_count" fields) marker-count
              (gethash "input_length" fields) (length truncated))
        (crichton/logging:write-audit-event "agent.input.injection_markers" fields))
      (log:warn "~D injection marker(s) detected in input from ~A — audit event written"
                marker-count (or source "unknown")))
    truncated))
