;;;; cli/main.lisp
;;;;
;;;; Command-line interface: start, stop, status, doctor

(in-package #:crichton/cli)

(defun main (&optional args)
  "Entry point for the Crichton CLI.
   ARGS defaults to sb-ext:*posix-argv* when called from command line."
  (let* ((argv (or args sb-ext:*posix-argv*))
         (command (or (second argv) "help")))
    (cond
      ((string-equal command "start")   (cmd-start))
      ((string-equal command "stop")    (cmd-stop))
      ((string-equal command "status")  (cmd-status))
      ((string-equal command "doctor")  (cmd-doctor))
      ((string-equal command "version") (cmd-version))
      ((string-equal command "eval")    (cmd-eval (cddr argv)))
      (t                                (cmd-help)))))

(defun cmd-start (&optional foreground)
  (format t "Starting Crichton daemon~:[...~; (foreground)...~]~%" foreground)
  (if (crichton/daemon:start-daemon :foreground foreground)
      (format t "Daemon started. PID ~D~%" (sb-posix:getpid))
      (format t "Daemon already running.~%")))

(defun cmd-stop ()
  (format t "Stopping Crichton daemon...~%")
  (if (crichton/daemon:stop-daemon)
      (format t "Daemon stopped.~%")
      (format t "Daemon was not running.~%")))

(defun cmd-status ()
  (let ((status (crichton/daemon:daemon-status)))
    (if (getf status :running)
        (format t "Crichton is running. PID ~D~%" (getf status :pid))
        (progn
          (format t "Crichton is not running.~%")
          (when (getf status :stale-pid)
            (format t "WARNING: Stale PID file found (PID ~D). Run 'crichton doctor'.~%"
                    (getf status :stale-pid)))))))

(defun cmd-doctor ()
  "Validate Crichton installation and config."
  (format t "Crichton Doctor~%")
  (format t "==============~%")
  (check-item "SBCL version" (lisp-implementation-version))
  (check-item "Home directory"
              (let ((home crichton/config:*agent-home*))
                (if (probe-file home)
                    (format nil "~A (exists)" (namestring home))
                    (format nil "~A (MISSING)" (namestring home)))))
  (check-item "Config file"
              (let ((cfg (merge-pathnames "config.toml" crichton/config:*agent-home*)))
                (if (probe-file cfg) "found" "not found (using defaults)")))
  (check-item "Swank available"
              (if (find-package :swank) "yes" "no (install via quicklisp)")))

(defun check-item (label value)
  (format t "  ~30A ~A~%" label value))

(defun cmd-version ()
  (format t "Crichton v~A~%"
          (asdf:component-version (asdf:find-system :crichton))))

(defun cmd-eval (rest-args)
  "Evaluate a form in the running daemon via Swank."
  (let ((form (format nil "~{~A~^ ~}" rest-args)))
    (if (zerop (length form))
        (progn
          (format t "Usage: crichton eval <form>~%")
          (format t "Example: crichton eval \"(daemon-status)\"~%"))
        (remote-eval form))))

(defun cmd-help ()
  (format t "Crichton — the quietly competent agent daemon~%~%")
  (format t "Usage: crichton <command>~%~%")
  (format t "Commands:~%")
  (format t "  start    Start the daemon~%")
  (format t "  stop     Stop the daemon~%")
  (format t "  status   Show daemon status~%")
  (format t "  eval     Evaluate a form in the running daemon via Swank~%")
  (format t "  doctor   Validate installation~%")
  (format t "  version  Show version~%")
  (format t "  help     This message~%"))
