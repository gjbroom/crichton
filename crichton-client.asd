;;;; crichton-client.asd
;;;;
;;;; Minimal chat client for the Crichton daemon.
;;;; Connects via Unix socket, no heavy dependencies.

(asdf:defsystem #:crichton-client
  :description "Minimal chat client for the Crichton daemon"
  :author "Gord Broom <gjbroom@thebrooms.ca>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:shasht
               #:cl-readline)
  :serial t
  :components ((:module "client"
                :components ((:file "package")
                             (:file "client")))))
