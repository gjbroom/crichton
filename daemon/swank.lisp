;;;; daemon/swank.lisp
;;;;
;;;; SLIME/SLY connectivity for live development.
;;;; The killer feature: connect to the running daemon, redefine functions,
;;;; inspect state, fix bugs without restart.

(in-package #:crichton/daemon)

(defun start-swank (&key (port 4005) (dont-close t))
  "Start a Swank server for SLIME/SLY connections.
   Connect from Emacs with M-x slime-connect RET 127.0.0.1 RET 4005"
  (handler-case
      (progn
        (asdf:load-system :swank)
        (let ((create-server (find-symbol "CREATE-SERVER" :swank)))
          (funcall create-server :port port :dont-close dont-close)
          (log:info "Swank server started" :port port)
          port))
    (error (c)
      (log:warn "Could not start Swank: ~A" c)
      (log:info "Install slime via Quicklisp: (ql:quickload :swank)")
      nil)))
