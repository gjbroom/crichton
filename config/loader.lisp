;;;; config/loader.lisp
;;;;
;;;; TOML config loading from ~/.crichton/config.toml
;;;; Parses via cl-toml, converts to nested plists, merges with defaults.

(in-package #:crichton/config)

(defparameter *crichton-version* "0.2.0"
  "Crichton daemon version string. Must match crichton.asd :version.")

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
   :backend, :egress-policy) should use CONFIG-SECTION-GET-KEYWORD."
  (typecase value
    (hash-table (toml-table-to-plist value))
    (string value)
    (vector (map 'list #'toml-value-to-lisp value))
    (t (cond
         ((eq value 'cl-toml:true) t)
         ((eq value 'cl-toml:false) nil)
         (t value)))))

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

(defun config-section-get-keyword (section key &optional default)
  "Like CONFIG-SECTION-GET but normalizes string values to keywords.
   Use for config values that should be keywords (:provider, :backend, :egress-policy, etc.)."
  (let ((val (config-section-get section key default)))
    (if (stringp val)
        (intern (string-upcase val) :keyword)
        val)))

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
        :weather (list :city "Victoria")
        :amp (list :enable nil
                   :allowed-repo-roots nil)
        :state (list :enabled t
                     :max-file-chars 20000
                     :max-total-chars 150000)))

(defun reload-config (&optional (path (merge-pathnames "config.toml" *agent-home*)))
  "Hot-reload configuration from PATH without restarting the daemon.

   Reloadable settings:
   - LLM provider, model, API key credential name
   - Logging level
   - Session retention policy
   - Weather city

   Non-reloadable (require daemon restart):
   - Swank port (daemon.swank-port)
   - PID file location

   Returns a plist with :success (T/NIL) and :message describing the result.
   On failure, rolls back to the previous configuration."
  (let ((old-config *config*)
        (reload-errors nil))
    (handler-case
        (progn
          ;; Step 1: Load and parse new config
          (unless (probe-file path)
            (return-from reload-config
              (list :success nil
                    :message (format nil "Config file not found: ~A" path))))

          (let* ((parsed (handler-case
                             (cl-toml:parse-file path)
                           (error (c)
                             (return-from reload-config
                               (list :success nil
                                     :message (format nil "Failed to parse config: ~A" c))))))
                 (converted (toml-table-to-plist parsed))
                 (merged (deep-merge-plist (default-config) converted)))

            ;; Step 2: Validate that critical non-reloadable settings haven't changed
            (let ((old-swank-port (config-section-get :daemon :swank-port))
                  (new-swank-port (getf (getf merged :daemon) :swank-port)))
              (when (and old-swank-port new-swank-port
                         (/= old-swank-port new-swank-port))
                (push (format nil "Swank port cannot be changed at runtime (old: ~A, new: ~A)"
                              old-swank-port new-swank-port)
                      reload-errors)))

            (when reload-errors
              (return-from reload-config
                (list :success nil
                      :message (format nil "Configuration validation failed: ~{~A~^; ~}"
                                      reload-errors))))

            ;; Step 3: Apply the new configuration
            (setf *config* merged)

            ;; Step 4: Attempt to reinitialize subsystems that depend on config
            (handler-case
                (progn
                  ;; Reinitialize LLM provider if config changed
                  (when (find-symbol "*LLM-PROVIDER*" :crichton/llm)
                    (let ((provider-var (find-symbol "*LLM-PROVIDER*" :crichton/llm)))
                      (when (boundp provider-var)
                        (setf (symbol-value provider-var) nil))))

                  ;; Update logging level if changed
                  (let ((new-level (config-section-get-keyword :logging :level)))
                    (when new-level
                      (log4cl:set-log-level log4cl:*root-logger* new-level)))

                  (log:info "Configuration reloaded successfully from ~A" path)
                  (list :success t
                        :message (format nil "Configuration reloaded from ~A" path)))
              (error (c)
                ;; Step 5: Rollback on failure
                (setf *config* old-config)
                (log:error "Configuration reload failed, rolled back: ~A" c)
                (list :success nil
                      :message (format nil "Reload failed, rolled back: ~A" c))))))
      (error (c)
        ;; Catch-all error handler
        (setf *config* old-config)
        (log:error "Unexpected error during config reload: ~A" c)
        (list :success nil
              :message (format nil "Unexpected error, rolled back: ~A" c))))))
