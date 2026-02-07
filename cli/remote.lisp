;;;; cli/remote.lisp
;;;;
;;;; Swank client for remote evaluation against the running daemon.
;;;; This is how Amp (or any automation) can modify the live daemon
;;;; without restart — the same workflow as SLIME, but scriptable.
;;;;
;;;; Usage from shell:
;;;;   sbcl --load remote-eval.lisp --eval '(crichton/cli:remote-eval "(daemon-status)")'
;;;;
;;;; Usage from Amp:
;;;;   (crichton/cli:remote-eval "(setf *some-var* 42)")
;;;;   (crichton/cli:remote-eval "(defun new-handler (x) (1+ x))")

(in-package #:crichton/cli)

(defun remote-eval (form-string &key (host "127.0.0.1") (port 4005) (timeout 10))
  "Evaluate FORM-STRING in the running Crichton daemon via Swank.
   Returns the result as a string, or signals an error."
  (handler-case
      (let ((connection (swank-client:slime-connect host port)))
        (unwind-protect
             (let ((result (swank-client:slime-eval
                            (read-from-string form-string)
                            connection)))
               (format t "~A~%" result)
               result)
          (swank-client:slime-close connection)))
    (usocket:connection-refused-error ()
      (format *error-output* "Cannot connect to Crichton daemon at ~A:~D~%" host port)
      (format *error-output* "Is the daemon running? Try: crichton start~%")
      nil)
    (error (c)
      (format *error-output* "Remote eval error: ~A~%" c)
      nil)))

(defun remote-eval-quiet (form-string &key (host "127.0.0.1") (port 4005))
  "Like REMOTE-EVAL but returns the value without printing.
   Useful for programmatic access."
  (handler-case
      (let ((connection (swank-client:slime-connect host port)))
        (unwind-protect
             (swank-client:slime-eval
              (read-from-string form-string)
              connection)
          (swank-client:slime-close connection)))
    (error () nil)))
