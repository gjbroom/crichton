;;;; daemon/swank.lisp
;;;;
;;;; SLIME/SLY connectivity for live development.
;;;; The killer feature: connect to the running daemon, redefine functions,
;;;; inspect state, fix bugs without restart.

(in-package #:crichton/daemon)

(defvar *swank-port* nil
  "Port the Swank server is running on, or NIL if not started.")

(defun start-swank (&key (port nil) (dont-close t))
  "Start a Swank server for SLIME/SLY connections.
   PORT defaults to config value or 4005.
   Idempotent — returns existing port if already running.
   Connect from Emacs with M-x slime-connect RET 127.0.0.1 RET 4005"
  (when *swank-port*
    (log:info "Swank already running" :port *swank-port*)
    (return-from start-swank *swank-port*))
  (let ((actual-port (or port
                         (crichton/config:config-section-get :daemon :swank-port)
                         4005)))
    (handler-case
        (progn
          (asdf:load-system :swank)
          (let ((create-server (find-symbol "CREATE-SERVER" :swank)))
            (funcall create-server :port actual-port :dont-close dont-close)
            (setf *swank-port* actual-port)
            (log:info "Swank server started on port ~D" actual-port)
            actual-port))
      (error (c)
        (log:warn "Could not start Swank: ~A" c)
        (log:info "Install slime via Quicklisp: (ql:quickload :swank)")
        nil))))
