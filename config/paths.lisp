;;;; config/paths.lisp
;;;;
;;;; Filesystem layout for ~/.crichton/

(in-package #:crichton/config)

(defvar *agent-home*
  (merge-pathnames #p".crichton/" (user-homedir-pathname))
  "Root directory for all Crichton data. Never load config from CWD.")

(defun agent-path (&rest components)
  "Build a path relative to *agent-home*.
   (agent-path \"logs\" \"daemon.jsonl\") => ~/.crichton/logs/daemon.jsonl"
  (let ((path *agent-home*))
    (dolist (c components path)
      (setf path (merge-pathnames (make-pathname :directory `(:relative ,c))
                                  path)))))

(defun ensure-directories ()
  "Create the Crichton directory tree with secure permissions.
   Directories: 0700, Files will be 0600 when written."
  (let ((dirs (list *agent-home*
                    (agent-path "logs")
                    (agent-path "credentials")
                    (agent-path "sessions")
                    (agent-path "skills")
                    (agent-path "trusted-keys")
                    (agent-path "state"))))
    (dolist (dir dirs)
      (ensure-directories-exist dir)
      #+sbcl (sb-posix:chmod (namestring dir) #o700))))
