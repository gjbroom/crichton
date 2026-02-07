;;;; crichton.lisp
;;;;
;;;; Top-level entry point and re-exports.

(in-package #:crichton)

(defun main ()
  "CLI entry point. Used by save-lisp-and-die.
   Detects --foreground flag for systemd Type=simple services."
  (let* ((args sb-ext:*posix-argv*)
         (foreground (member "--foreground" args :test #'string-equal)))
    (if foreground
        (crichton/daemon:start-daemon :foreground t)
        (crichton/cli:main))))
