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

(defun reload-config (&optional path)
  "Hot-reload configuration without restarting the daemon.
   Convenience wrapper for SWANK REPL usage.

   Usage:
     (crichton:reload-config)
     => (:SUCCESS T :MESSAGE \"Configuration reloaded successfully\" ...)

   Returns a plist with:
     :success  - T if reload succeeded
     :message  - Human-readable status
     :errors   - List of validation errors (if any)
     :reloaded - Config sections that were reloaded
     :skipped  - Config sections requiring daemon restart"
  (crichton/config:reload-config path))
