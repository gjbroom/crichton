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
