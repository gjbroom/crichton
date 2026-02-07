;;;; crypto/wipe.lisp
;;;;
;;;; Best-effort secret zeroization utilities.
;;;;
;;;; FLAG: Common Lisp cannot reliably guarantee zeroization due to GC,
;;;; copy-on-write strings, and compiler optimizations. These utilities
;;;; are best-effort: they overwrite known references but cannot track
;;;; all copies the runtime may have made. This is a fundamental
;;;; limitation of garbage-collected runtimes.
;;;;
;;;; Mitigation strategy:
;;;; - Keep secrets as (unsigned-byte 8) vectors as long as possible
;;;; - Decode to string only at the boundary that needs it
;;;; - Use with-secret-bytes to auto-wipe after use
;;;; - Rely on crichton/logging:with-redaction to prevent accidental logging

(in-package #:crichton/crypto)

(defun wipe-bytes (vec)
  "Overwrite an octet vector with zeros. Best-effort — see file header.
   Returns VEC."
  (when (and vec (typep vec '(vector (unsigned-byte 8))))
    (fill vec 0))
  vec)

(defun wipe-string (str)
  "Overwrite a simple string with null characters. Best-effort.
   Returns STR."
  (when (and str (stringp str))
    (fill str #\Nul))
  str)

(defmacro with-secret-bytes ((var expr) &body body)
  "Bind VAR to the octet vector result of EXPR, execute BODY, then wipe VAR.
   Returns the value of BODY."
  `(let ((,var ,expr))
     (unwind-protect (progn ,@body)
       (wipe-bytes ,var))))

(defmacro with-secret-string ((var expr) &body body)
  "Bind VAR to the string result of EXPR, execute BODY, then wipe VAR.
   Returns the value of BODY."
  `(let ((,var ,expr))
     (unwind-protect (progn ,@body)
       (wipe-string ,var))))
