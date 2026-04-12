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
  :author "Gord Broom <gjbroom@thebrooms.ca>"
  :license "MIT"
  :version "0.4.0"
  :depends-on (#:cffi
               #:bordeaux-threads
               #:usocket
               #:ironclad
               #:shasht
               #:log4cl
               #:cl-ppcre
               #:cl-toml
               #:cl-base64
               #:dexador
               #:hunchentoot
               #:swank-client
               #:xmls
               #:websocket-driver-client
               #:sqlite)
  :serial t
  :components ((:file "package")
               (:module "config"
                :components ((:file "paths")
                             (:file "loader")
                             (:file "util")))
               (:module "logging"
                :components ((:file "logger")))
               (:module "storage"
                :components ((:file "store")))
               (:module "rpc"
                :components ((:file "protocol")))
               (:module "wasm"
                :components ((:file "ffi")
                             (:file "engine")))
               (:module "skills"
                :components ((:file "retry-infrastructure")
                             (:file "manifest")
                             (:file "signing")
                             (:file "kv-store")
                             (:file "registry")
                             (:file "pipeline")
                             (:module "builtins"
                              :components ((:file "weather")
                                            (:file "system-info")
                                            (:file "timing")
                                            (:file "scheduler")
                                            (:file "task-store")
                                            (:file "ephemeris")
                                            (:file "rss")
                                            (:file "rss-monitor")
                                            (:file "rss-publish")
                                            (:file "token-usage")
                                            (:file "battery")
                                            (:file "log-inspector")
                                            (:file "amp-orchestrator")
                                            (:file "raindrop")
                                            (:file "books")
                                            (:file "orgmode")
                                            (:file "orgmode-roam")
                                            (:file "orgmode-api")
                                            (:file "pushover")
                                            (:file "github")
                                            (:file "hoobs")
                                            (:file "git")))))
               (:module "crypto"
                :components ((:file "wipe")
                             (:file "age")))
               (:module "credentials"
                :components ((:file "protocol")
                             (:file "backend-age-file")
                             (:file "mediator")))
               (:module "state"
                  :components ((:file "bootstrap")
                               (:file "journal")))
               (:module "sessions"
                 :components ((:file "store")
                              (:file "retention")))
                (:module "llm"
                 :components ((:file "protocol")
                              (:file "anthropic")
                              (:file "registry")))
                (:module "daemon"
                :components ((:file "lifecycle")
                             (:file "notifications")
                             (:file "swank")
                             (:file "runner-client")))
               (:module "agent"
                :components ((:file "sanitize")
                             (:file "tools")
                             (:file "tools-system")
                             (:file "tools-ops")
                             (:file "tools-data")
                             (:file "tools-register")
                             (:file "loop")))
               (:file "daemon/rpc-server")
               (:module "channels"
                :components ((:file "protocol")
                             (:file "discord")
                             (:file "manager")))
               (:module "runner"
                :components ((:file "skill-context")
                             (:file "server")))))
