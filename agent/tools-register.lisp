;;;; agent/tools-register.lisp
;;;;
;;;; Channel tool allowlist and register-all-tools.
;;;; Must be loaded last in the agent module — calls every register-*-tool
;;;; function defined across tools-system, tools-ops, and tools-data.

(in-package #:crichton/agent)

;;; --- Channel tool allowlist ---

(defparameter *default-channel-allowed-tools*
  '("weather" "time" "ephemeris" "system_info" "rss" "battery" "resource_usage")
  "Tools available in external channel sessions (Discord, etc.) when no
   config override is set.  Intentionally excludes privileged tools such
   as amp_code, amp_test, orgmode, raindrop, pushover, and github.")

(defun channel-safe-tool-defs ()
  "Return tool definitions safe for external channel sessions.
   Reads [agent] channel_allowed_tools from config (a list of tool-name
   strings); falls back to *default-channel-allowed-tools* when absent or
   empty.  Logs the effective allowlist at startup."
  (let* ((configured (crichton/config:config-section-get :agent :channel-allowed-tools))
         (allowed (if (and (listp configured) (plusp (length configured)))
                      configured
                      *default-channel-allowed-tools*))
         (allowed-set (make-hash-table :test #'equal)))
    (dolist (name allowed)
      (setf (gethash name allowed-set) t))
    (log:info "Channel tool allowlist: ~{~A~^, ~}" allowed)
    (remove-if-not (lambda (def)
                     (gethash (getf def :name) allowed-set))
                   (all-tool-defs))))

;;; --- Registration ---

(defun register-all-tools ()
  "Register all built-in tools for agent use."
  (register-weather-tool)
  (register-system-info-tool)
  (register-scheduler-tool)
  (register-time-tool)
  (register-ephemeris-tool)
  (register-rss-tool)
  (register-usage-tool)
  (register-battery-tool)
  (register-log-inspector-tool)
  (register-amp-check-tool)
  (register-amp-code-tool)
  (register-amp-test-tool)
  (register-amp-check-json-tool)
  (register-amp-code-json-tool)
  (register-amp-test-json-tool)
  (register-skills-tool)
  (register-memory-write-tool)
  (register-memory-search-tool)
  (register-raindrop-tool)
  (register-books-tool)
  (register-orgmode-tool)
  (register-pushover-tool)
  (register-github-tool)
  (register-hoobs-tool)
  (register-git-tool)
  ;; Register pipeline built-in functions
  (crichton/skills:register-default-pipeline-builtins)
  (log:info "Registered ~D agent tools" (hash-table-count *agent-tools*)))
