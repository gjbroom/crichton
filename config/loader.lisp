;;;; config/loader.lisp
;;;;
;;;; TOML config loading from ~/.crichton/config.toml
;;;; Stub — TOML parser TBD (may use cl-toml or hand-roll a minimal one)

(in-package #:crichton/config)

(defvar *config* nil
  "Parsed configuration plist. Loaded from ~/.crichton/config.toml.")

(defun load-config (&optional (path (merge-pathnames "config.toml" *agent-home*)))
  "Load configuration from PATH. Only loads from *agent-home*, never CWD."
  (unless (probe-file path)
    (warn "No config file at ~A — using defaults." path)
    (setf *config* (default-config))
    (return-from load-config *config*))
  ;; TODO: parse TOML. For now, return defaults.
  (setf *config* (default-config)))

(defun default-config ()
  "Sensible defaults for a fresh install."
  (list :llm (list :provider :anthropic
                   :model "claude-sonnet-4-20250514")
        :logging (list :level :info
                       :format :json)
        :daemon (list :swank-port 4005
                      :pid-file (namestring (merge-pathnames "daemon.pid" *agent-home*)))
        :sessions (list :retention-days 30
                        :encrypt t)
        :network (list :egress-policy :deny-all)))
