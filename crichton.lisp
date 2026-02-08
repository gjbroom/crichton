;;;; crichton.lisp
;;;;
;;;; Top-level entry point and re-exports.

(in-package #:crichton)

(defun main ()
  "Entry point for the save-lisp-and-die binary.
   Dispatches between runner subprocess, foreground daemon, and CLI modes."
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
