;;;; crichton.lisp
;;;;
;;;; Top-level entry point and re-exports.

(in-package #:crichton)

(defun reinitialize-foreign-state ()
  "Clear stale foreign library state left over from save-lisp-and-die.
   cl+ssl bakes global context/method pointers into the image at build time;
   these become dangling pointers at runtime, causing memory faults when
   OpenSSL tries to dispatch through a NULL method vtable."
  (setf cl+ssl::*ssl-global-context* nil
        cl+ssl::*ssl-global-method* nil
        cl+ssl::*tmp-rsa-key-512* nil
        cl+ssl::*tmp-rsa-key-1024* nil
        cl+ssl::*tmp-rsa-key-2048* nil))

(pushnew 'reinitialize-foreign-state sb-ext:*init-hooks*)

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
