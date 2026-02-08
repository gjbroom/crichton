;;;; crichton.lisp
;;;;
;;;; Top-level entry point and re-exports.

(in-package #:crichton)

(defun main ()
  "CLI entry point. Used by save-lisp-and-die.
   Detects --foreground flag for systemd Type=simple services.
   Dispatches 'runner' subcommand to the skill runner process."
  (let* ((args sb-ext:*posix-argv*)
         (command (second args))
         (foreground (member "--foreground" args :test #'string-equal)))
    (cond
      ((and command (string-equal command "runner"))
       (crichton/runner:main (cddr args)))
      (foreground
       (crichton/daemon:start-daemon :foreground t))
      (t
       ;; CLI commands: suppress console logging (systemd captures output)
       (crichton/logging:suppress-console-logging)
       (crichton/cli:main)))))
