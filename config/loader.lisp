;;;; config/loader.lisp
;;;;
;;;; TOML config loading from ~/.crichton/config.toml
;;;; Parses via cl-toml, converts to nested plists, merges with defaults.

(in-package #:crichton/config)

(defvar *config* nil
  "Parsed configuration plist. Loaded from ~/.crichton/config.toml.")

(defun load-config (&optional (path (merge-pathnames "config.toml" *agent-home*)))
  "Load configuration from PATH. Only loads from *agent-home*, never CWD.
   Missing keys fall back to defaults. Parse errors fall back entirely to defaults."
  (unless (probe-file path)
    (warn "No config file at ~A — using defaults." path)
    (setf *config* (default-config))
    (return-from load-config *config*))
  (handler-case
      (let* ((parsed (cl-toml:parse-file path))
             (converted (toml-table-to-plist parsed))
             (merged (deep-merge-plist (default-config) converted)))
        (setf *config* merged))
    (error (c)
      (warn "Failed to parse ~A: ~A — using defaults." path c)
      (setf *config* (default-config))))
  *config*)

(defun toml-table-to-plist (table)
  "Convert a cl-toml hash-table (with string keys) to a nested plist with keyword keys.
   Underscores in keys become hyphens. Booleans normalized to T/NIL."
  (when (hash-table-p table)
    (let (result)
      (maphash (lambda (k v)
                 (let ((key (toml-key-to-keyword k))
                       (val (toml-value-to-lisp v)))
                   (push val result)
                   (push key result)))
               table)
      result)))

(defun toml-key-to-keyword (key)
  "Convert a TOML string key to a keyword. foo_bar => :FOO-BAR"
  (intern (string-upcase (substitute #\- #\_ (string key)))
          :keyword))

(defun toml-value-to-lisp (value)
  "Convert a cl-toml value to a Lisp value.
   Hash-tables become plists, cl-toml booleans become T/NIL, rest pass through.
   NOTE: String values remain strings — callers expecting keywords (e.g. :provider,
   :backend, :egress-policy) must normalize themselves. See llm/registry.lisp for
   the pattern. A future config-section-get-keyword accessor could centralize this."
  (cond
    ((hash-table-p value) (toml-table-to-plist value))
    ((eq value 'cl-toml:true) t)
    ((eq value 'cl-toml:false) nil)
    ((stringp value) value)
    (t value)))

(defun deep-merge-plist (defaults overrides)
  "Merge OVERRIDES into DEFAULTS. Both are plists. Nested plists are merged recursively.
   Keys in OVERRIDES replace those in DEFAULTS; missing keys keep defaults."
  (let ((result (copy-list defaults)))
    (loop for (key val) on overrides by #'cddr
          do (let ((default-val (getf result key :not-found)))
               (if (and (plistp default-val)
                        (plistp val))
                   (setf (getf result key) (deep-merge-plist default-val val))
                   (setf (getf result key) val))))
    result))

(defun plistp (thing)
  "Return T if THING looks like a plist (list with even length, keyword keys)."
  (and (listp thing)
       (evenp (length thing))
       (loop for (k) on thing by #'cddr
             always (keywordp k))))

(defun config-get (key &optional default)
  "Get a top-level config value. KEY is a keyword."
  (getf (or *config* (default-config)) key default))

(defun config-section-get (section key &optional default)
  "Get a nested config value. (config-section-get :daemon :swank-port) => 4005"
  (getf (config-get section) key default))

(defun default-config ()
  "Sensible defaults for a fresh install."
  (list :llm (list :provider :anthropic
                   :model "claude-sonnet-4-20250514"
                   :api-key-credential "anthropic-api-key")
        :logging (list :level :info
                       :format :json)
        :daemon (list :swank-port 4005
                      :pid-file (namestring (merge-pathnames "daemon.pid" *agent-home*)))
        :sessions (list :retention-days 30
                         :encrypt t)
        :credentials (list :backend :auto)
        :network (list :egress-policy :deny-all)
        :weather (list :city "Victoria")))

(defun validate-config (config)
  "Validate a parsed configuration. Returns (values valid-p errors).
   ERRORS is a list of strings describing validation problems."
  (let ((errors nil))
    ;; Check that config is a plist
    (unless (plistp config)
      (push "Configuration must be a property list" errors)
      (return-from validate-config (values nil errors)))

    ;; Validate :llm section
    (let ((llm (getf config :llm)))
      (when llm
        (unless (plistp llm)
          (push ":llm section must be a property list" errors))
        (let ((provider (getf llm :provider)))
          (when (and provider
                     (stringp provider)
                     (not (string-equal provider "anthropic")))
            (push (format nil "Unsupported LLM provider: ~A (only 'anthropic' supported)" provider)
                  errors)))))

    ;; Validate :daemon section
    (let ((daemon (getf config :daemon)))
      (when daemon
        (unless (plistp daemon)
          (push ":daemon section must be a property list" errors))
        (let ((port (getf daemon :swank-port)))
          (when (and port (not (and (integerp port) (> port 0) (< port 65536))))
            (push (format nil "Invalid swank-port: ~A (must be 1-65535)" port) errors)))))

    ;; Validate :sessions section
    (let ((sessions (getf config :sessions)))
      (when sessions
        (unless (plistp sessions)
          (push ":sessions section must be a property list" errors))
        (let ((days (getf sessions :retention-days)))
          (when (and days (not (and (integerp days) (>= days 0))))
            (push (format nil "Invalid retention-days: ~A (must be non-negative integer)" days)
                  errors)))))

    ;; Validate :logging section
    (let ((logging (getf config :logging)))
      (when logging
        (unless (plistp logging)
          (push ":logging section must be a property list" errors))
        (let ((level (getf logging :level)))
          (when (and level (stringp level))
            (let ((level-kw (intern (string-upcase level) :keyword)))
              (unless (member level-kw '(:trace :debug :info :warn :error :fatal))
                (push (format nil "Invalid logging level: ~A" level) errors)))))))

    (values (null errors) (nreverse errors))))

(defun reload-config (&optional (path (merge-pathnames "config.toml" *agent-home*)))
  "Hot-reload configuration from PATH without restarting the daemon.

   Returns a plist with keys:
     :success  - T if reload succeeded, NIL if it failed
     :message  - Human-readable status message
     :errors   - List of validation errors (if any)
     :reloaded - List of config sections that were successfully reloaded
     :skipped  - List of config sections that require restart

   Hot-reloadable sections:
     - :llm (model, API key credential name)
     - :sessions (retention days, encryption)
     - :weather (city)
     - :channels (Discord token, enabled status)
     - :logging (level, format) [partially - see notes]
     - :network (egress policy)

   Sections that REQUIRE daemon restart:
     - :daemon :swank-port (SWANK server binding)
     - :daemon :pid-file (daemon lifecycle management)

   Notes:
     - Logging level changes take effect immediately
     - Logging format changes require reinitializing the logging subsystem
     - LLM provider changes recreate the provider instance on next use
     - Channel adapters will need to be manually restarted to use new config
     - Port bindings cannot be changed without daemon restart

   Usage from SWANK REPL:
     (crichton/config:reload-config)
     => (:SUCCESS T :MESSAGE \"Configuration reloaded successfully\" ...)

   Or with specific path:
     (crichton/config:reload-config #p\"~/.crichton/config.toml\")"
  (log:info "Configuration reload requested" :path (namestring path))

  ;; Check if config file exists
  (unless (probe-file path)
    (log:warn "Config file not found during reload" :path (namestring path))
    (return-from reload-config
      (list :success nil
            :message (format nil "Config file not found: ~A" (namestring path))
            :errors (list "File does not exist")
            :reloaded nil
            :skipped nil)))

  ;; Save old config for rollback
  (let ((old-config (copy-list *config*))
        (reloaded-sections nil)
        (skipped-sections nil))

    (handler-case
        (progn
          ;; Parse new config
          (let* ((parsed (cl-toml:parse-file path))
                 (converted (toml-table-to-plist parsed))
                 (merged (deep-merge-plist (default-config) converted)))

            ;; Validate new config
            (multiple-value-bind (valid-p errors) (validate-config merged)
              (unless valid-p
                (log:error "Configuration validation failed" :errors errors)
                (return-from reload-config
                  (list :success nil
                        :message "Configuration validation failed"
                        :errors errors
                        :reloaded nil
                        :skipped nil)))

              ;; Apply new config
              (setf *config* merged)
              (log:info "Configuration updated in memory")

              ;; Detect which sections changed and categorize them
              (dolist (section '(:llm :sessions :weather :channels :network :logging :credentials))
                (let ((old-val (getf old-config section))
                      (new-val (getf merged section)))
                  (unless (equal old-val new-val)
                    (push section reloaded-sections))))

              ;; Check for non-reloadable changes
              (let ((old-port (config-section-get-old old-config :daemon :swank-port))
                    (new-port (config-section-get :daemon :swank-port))
                    (old-pid (config-section-get-old old-config :daemon :pid-file))
                    (new-pid (config-section-get :daemon :pid-file)))
                (when (and old-port new-port (not (equal old-port new-port)))
                  (push :daemon-swank-port skipped-sections)
                  (log:warn "Swank port change requires daemon restart"
                           :old-port old-port :new-port new-port))
                (when (and old-pid new-pid (not (equal old-pid new-pid)))
                  (push :daemon-pid-file skipped-sections)
                  (log:warn "PID file change requires daemon restart"
                           :old-path old-pid :new-path new-pid)))

              ;; Clear cached LLM provider to force recreation with new config
              (when (member :llm reloaded-sections)
                (when (find-package :crichton/llm)
                  (let ((provider-var (find-symbol "*LLM-PROVIDER*" :crichton/llm)))
                    (when (and provider-var (boundp provider-var))
                      (setf (symbol-value provider-var) nil)
                      (log:info "LLM provider cleared, will be recreated with new config")))))

              ;; Log the successful reload
              (log:info "Configuration reloaded successfully"
                       :reloaded-sections reloaded-sections
                       :skipped-sections skipped-sections)

              (list :success t
                    :message "Configuration reloaded successfully"
                    :errors nil
                    :reloaded reloaded-sections
                    :skipped skipped-sections))))

      (error (c)
        ;; Rollback on error
        (setf *config* old-config)
        (log:error "Configuration reload failed, rolled back" :error (princ-to-string c))
        (list :success nil
              :message (format nil "Reload failed: ~A" c)
              :errors (list (princ-to-string c))
              :reloaded nil
              :skipped nil)))))

(defun config-section-get-old (config section key &optional default)
  "Helper to get nested config value from a specific config plist (not *config*)."
  (getf (getf config section) key default))
