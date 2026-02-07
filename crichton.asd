;;;; crichton.asd
;;;;
;;;; Crichton — a secure background AI agent daemon
;;;; Named for "The Admirable Crichton" — the quietly competent servant
;;;; who's actually running everything.

(asdf:defsystem #:crichton
  :description "Secure background AI agent with WASM-sandboxed skills"
  :long-description "A local AI agent daemon with encrypted credentials,
WASM-isolated skill execution, network egress control, and Ed25519 code
signing. Informed by a security audit of OpenClaw."
  :author "Gord Broom <gjbroom@camosun.ca>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cffi
               #:bordeaux-threads
               #:usocket
               #:ironclad
               #:shasht
               #:log4cl
               #:cl-ppcre
               #:cl-toml
               #:dexador
               #:hunchentoot
               #:swank-client)
  :serial t
  :components ((:file "package")
               (:module "config"
                :components ((:file "paths")
                             (:file "loader")))
               (:module "logging"
                :components ((:file "logger")))
               (:module "daemon"
                :components ((:file "lifecycle")
                             (:file "swank")))
               (:module "cli"
                :components ((:file "main")
                             (:file "remote")))
               (:file "crichton")))
