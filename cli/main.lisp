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
      ((string-equal command "start")      (cmd-start))
      ((string-equal command "stop")       (cmd-stop))
      ((string-equal command "status")     (cmd-status))
      ((string-equal command "doctor")     (cmd-doctor))
      ((string-equal command "version")    (cmd-version))
      ((string-equal command "eval")       (cmd-eval (cddr argv)))
      ((string-equal command "cred")       (cmd-cred (cddr argv)))
      ((string-equal command "credential") (cmd-cred (cddr argv)))
      ((string-equal command "weather")    (cmd-weather (cddr argv)))
      ((string-equal command "chat")       (cmd-chat (cddr argv)))
      ((string-equal command "ask")        (cmd-chat (cddr argv)))
      (t                                   (cmd-help)))))

(defun cmd-start (&optional foreground)
  (format t "Starting Crichton daemon~:[...~; (foreground)...~]~%" foreground)
  (if (crichton/daemon:start-daemon :foreground foreground)
      (format t "Daemon started. PID ~D~%" (sb-posix:getpid))
      (format t "Daemon already running.~%")))

(defun cmd-stop ()
  (format t "Stopping Crichton daemon...~%")
  (let ((result (remote-eval-quiet "(crichton/daemon:stop-daemon)")))
    (if result
        (format t "Daemon stopped.~%")
        (format t "Daemon was not running.~%"))))

(defun cmd-status ()
  (let ((result (remote-eval-quiet "(crichton/daemon:daemon-status)")))
    (if result
        (let ((pid (getf result :pid)))
          (format t "Crichton is running. PID ~D~%" pid))
        (let ((local-status (crichton/daemon:daemon-status)))
          (format t "Crichton is not running.~%")
          (when (getf local-status :stale-pid)
            (format t "WARNING: Stale PID file found (PID ~D). Run 'crichton doctor'.~%"
                    (getf local-status :stale-pid)))))))

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
              (if (find-package :swank) "yes" "no (install via quicklisp)"))
  (check-item "age CLI"
              (if (crichton/crypto:age-available-p)
                  "found"
                  "NOT FOUND (apt install age)"))
  (check-item "Identity key"
              (if (crichton/crypto:identity-key-exists-p)
                  (format nil "~A (exists)"
                          (namestring (crichton/crypto:identity-key-path)))
                  "not yet created (will be generated on first use)"))
  (check-item "Credentials dir"
              (let ((dir (merge-pathnames #p"credentials/"
                                          crichton/config:*agent-home*)))
                (if (probe-file dir)
                    (format nil "~A (exists)" (namestring dir))
                    (format nil "~A (will be created)" (namestring dir)))))
  (check-item "Sessions dir"
              (let ((dir (merge-pathnames #p"sessions/"
                                          crichton/config:*agent-home*)))
                (if (probe-file dir)
                    (format nil "~A (exists)" (namestring dir))
                    (format nil "~A (will be created)" (namestring dir)))))
  (check-item "Session encryption"
              (if (crichton/config:config-section-get :sessions :encrypt t)
                  "enabled" "DISABLED"))
  (check-item "Session retention"
              (let ((days (crichton/config:config-section-get :sessions :retention-days 30)))
                (cond
                  ((and (numberp days) (zerop days)) "no-store mode")
                  ((numberp days) (format nil "~D days" days))
                  (t "keep forever")))))

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

(defun cmd-cred (args)
  "Manage stored credentials."
  (crichton/config:load-config)
  (let ((subcmd (or (first args) "help")))
    (cond
      ((string-equal subcmd "list")
       (let ((names (crichton/credentials:list-credentials)))
         (if names
             (dolist (n names)
               (format t "  ~A~%" n))
             (format t "No credentials stored.~%"))))

      ((string-equal subcmd "set")
       (let ((name (second args)))
         (unless name
           (format t "Usage: crichton cred set <name>~%")
           (return-from cmd-cred))
         (format t "Enter credential fields (key=value, blank line to finish):~%")
         (let ((plist nil))
           (loop for line = (read-line *standard-input* nil nil)
                 while (and line (plusp (length line)))
                 do (let ((pos (position #\= line)))
                      (if pos
                          (let ((key (intern (string-upcase (subseq line 0 pos))
                                            :keyword))
                                (val (subseq line (1+ pos))))
                            (push val plist)
                            (push key plist))
                          (format t "  Skipping (no '='): ~A~%" line))))
           (if plist
               (progn
                 (crichton/credentials:store-credential name plist :overwrite t)
                 (format t "Credential ~A stored.~%" name))
               (format t "No fields entered, nothing stored.~%")))))

      ((string-equal subcmd "get")
       (let ((name (second args)))
         (unless name
           (format t "Usage: crichton cred get <name>~%")
           (return-from cmd-cred))
         (handler-case
             (let ((plist (crichton/credentials:resolve-credential name)))
               (loop for (k v) on plist by #'cddr
                     do (format t "  ~A = ~A~%"
                                (string-downcase (symbol-name k))
                                v)))
           (crichton/credentials:credential-not-found ()
             (format t "Credential ~A not found.~%" name)))))

      ((string-equal subcmd "delete")
       (let ((name (second args)))
         (unless name
           (format t "Usage: crichton cred delete <name>~%")
           (return-from cmd-cred))
         (if (crichton/credentials:delete-credential name)
             (format t "Credential ~A deleted.~%" name)
             (format t "Credential ~A not found.~%" name))))

      (t
       (format t "Usage: crichton cred <subcommand>~%~%")
       (format t "Subcommands:~%")
       (format t "  list            List stored credential names~%")
       (format t "  set <name>      Store a credential (interactive)~%")
       (format t "  get <name>      Show credential fields~%")
       (format t "  delete <name>   Delete a credential~%")))))

(defun cmd-weather (args)
  "Display weather for a Canadian city."
  (crichton/config:load-config)
  (let ((city (or (first args)
                  (crichton/config:config-section-get :weather :city)
                  "Victoria")))
    (handler-case
        (crichton/skills:weather-report :city city)
      (error (c)
        (format *error-output* "Weather error: ~A~%" c)))))

(defun cmd-chat (args)
  "Chat with the agent. One-shot with args, interactive without."
  (crichton/config:load-config)
  (crichton/agent:register-all-tools)
  (let ((text (format nil "~{~A~^ ~}" args)))
    (if (plusp (length text))
        (handler-case
            (multiple-value-bind (response)
                (crichton/agent:ask text)
              (format t "~A~%" response))
          (error (c)
            (format *error-output* "Agent error: ~A~%" c)))
        (handler-case
            (crichton/agent:chat-session)
          (error (c)
            (format *error-output* "Agent error: ~A~%" c))))))

(defun cmd-help ()
  (format t "Crichton — the quietly competent agent daemon~%~%")
  (format t "Usage: crichton <command>~%~%")
  (format t "Commands:~%")
  (format t "  start      Start the daemon~%")
  (format t "  stop       Stop the daemon~%")
  (format t "  status     Show daemon status~%")
  (format t "  chat       Chat with the agent (interactive or one-shot)~%")
  (format t "  ask        Alias for chat~%")
  (format t "  eval       Evaluate a form in the running daemon via Swank~%")
  (format t "  cred       Manage credentials (list, set, get, delete)~%")
  (format t "  weather    Current conditions and forecast (default: Victoria)~%")
  (format t "  doctor     Validate installation~%")
  (format t "  version    Show version~%")
  (format t "  help       This message~%"))
