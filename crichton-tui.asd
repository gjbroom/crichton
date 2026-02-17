;;;; crichton-tui.asd
;;;;
;;;; TUI chat client for the Crichton daemon.
;;;; Uses cl-tuition (TEA/Elm architecture) for terminal UI.

(asdf:defsystem #:crichton-tui
  :description "TUI chat client for the Crichton daemon"
  :author "Gord Broom <gjbroom@thebrooms.ca>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:shasht
               #:tuition
               #:bordeaux-threads)
  :serial t
  :components ((:module "tui"
                :components ((:file "package")
                             (:file "protocol")
                             (:file "model")
                             (:file "messages")
                             (:file "theme")
                             (:file "render")
                             (:file "client")))))
