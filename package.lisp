;;;; package.lisp
;;;;
;;;; Package definitions for Crichton

(defpackage #:crichton/config
  (:use #:cl)
  (:export #:*agent-home*
           #:*config*
           #:agent-path
           #:ensure-directories
           #:load-config
           #:config-get
           #:config-section-get))

(defpackage #:crichton/logging
  (:use #:cl)
  (:export #:setup-logging
           #:with-redaction))

(defpackage #:crichton/daemon
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:start-daemon
           #:stop-daemon
           #:daemon-status
           #:start-swank))

(defpackage #:crichton/cli
  (:use #:cl)
  (:export #:main
           #:remote-eval
           #:remote-eval-quiet))

(defpackage #:crichton
  (:use #:cl)
  (:export #:main))
