;;;; agent/tools-ops.lisp
;;;;
;;;; Operations-facing tool definitions: daemon log inspector and
;;;; Amp CLI orchestration (human-readable and JSON variants).

(in-package #:crichton/agent)

;;; --- Daemon log inspector tool ---

(defun format-log-entries (entries)
  "Format a list of log entry hash-tables as human-readable text."
  (with-output-to-string (s)
    (if (null entries)
        (format s "No log entries found.~%")
        (dolist (entry entries)
          (format s "[~A] [~A] (~A) ~A~%"
                  (gethash "timestamp" entry "?")
                  (gethash "level" entry "?")
                  (gethash "logger" entry "?")
                  (gethash "message" entry ""))))))

(define-tool log-inspector
    (:description "Inspect the daemon's own log files.  Use to check for errors, warnings, or verify that systems are functioning correctly.  Logs are sanitized — safe to share with users."
     :tool-name "daemon_logs")
  ((action "string"
           "The log inspection action to perform."
           :enum ("summary" "recent" "errors" "search")
           :required-p t)
   (count "integer"
          "Number of log entries to examine. Default: 50."
          :default 50)
   (pattern "string"
            "Search substring for the 'search' action. Case-insensitive match against message text.")
   (level "string"
          "Filter by log level (ERROR, WARN, INFO, DEBUG). Optional for 'search' action."
          :enum ("ERROR" "WARN" "INFO" "DEBUG")))
  (cond
    ((string-equal action "summary")
     (with-output-to-string (s)
       (log-report :stream s :count count)))
    ((string-equal action "recent")
     (format-log-entries
      (read-log-tail :count count)))
     ((string-equal action "errors")
      (format-log-entries
      (search-log :level "ERROR" :count count)))
     ((string-equal action "search")
      (if (and (null pattern) (null level))
          "Error: 'pattern' and/or 'level' required for search action."
          (format-log-entries
          (search-log :pattern pattern
                                      :level level
                                      :count count))))
    (t
     (format nil "Unknown daemon_logs action: ~A" action))))

;;; --- Amp orchestration tools ---

(define-tool amp-check
    (:description "Check whether the Amp CLI coding agent is available and enabled on this system.  Returns availability and configuration status.  Use this before attempting amp_code or amp_test tasks.")
  ()
  (let ((status (amp-status)))
    (cond
      ((not (getf status :enabled))
       "Amp orchestration is DISABLED in config. Set [amp] enable = true in config.toml.")
      ((not (getf status :binary-available))
       "Amp is enabled in config but the CLI binary is NOT found on PATH. Install it to enable code delegation.")
      (t
       (format nil "Amp CLI is available and enabled.~%Binary: ~A~%Allowed repo roots: ~A"
               (getf status :binary-path)
               (or (getf status :allowed-repo-roots) "unrestricted"))))))

(define-tool amp-code
    (:description "Delegate a coding task to the Amp CLI agent.  Amp is an AI coding assistant that can read, write, and modify code files.  Use this for implementation tasks: writing new functions, fixing bugs, refactoring code, adding features.  Requires Amp to be enabled in config and CLI installed (check with amp_check first).")
  ((description "string"
                "Description of the coding task to delegate to Amp. Be specific about what code to write or modify."
                :required-p t)
   (repo-path "string"
              "Absolute path to the repository to work in. Must be under an allowed_repo_roots path."
              :required-p t)
   (files "string"
          "Comma-separated list of file paths to focus on (relative to repo_path).")
   (context "string"
            "Additional context for the task (e.g., coding standards, constraints, existing patterns).")
   (timeout-seconds "integer"
                    "Maximum time in seconds to wait for Amp to complete. Default: 300 (5 minutes)."
                    :default 300))
  (let ((file-list (when files
                     (mapcar (lambda (f)
                               (string-trim '(#\Space) f))
                             (cl-ppcre:split "," files)))))
    (let ((result (amp-code-task
                   description
                   :repo-path repo-path
                   :files file-list
                   :context context
                   :timeout-seconds timeout-seconds)))
      (with-output-to-string (s)
        (amp-report result :stream s)))))

(define-tool amp-test
    (:description "Run a test suite and optionally use the Amp CLI agent to fix failures.  Executes the test runner directly (no shell).  If tests fail and fix_failures is true, Amp is invoked with the failure context to attempt repairs, iterating up to max_iterations.  Requires Amp to be enabled in config for fix mode.")
  ((test-runner "string"
                "The test runner executable (e.g., 'pytest', 'make', 'cargo', 'sbcl')."
                :required-p t)
   (test-args "string"
              "Comma-separated arguments for the test runner (e.g., 'test,--verbose'). Each comma-separated element is passed as a separate argument.")
   (repo-path "string"
              "Absolute path to the repository. Tests run with this as the working directory.")
   (fix-failures "boolean"
                 "If true, invoke Amp to fix test failures automatically. Default: false.")
   (max-iterations "integer"
                   "Maximum test/fix cycles when fix_failures is true. Default: 3."
                   :default 3)
   (timeout-seconds "integer"
                    "Maximum time in seconds per Amp fix invocation. Default: 300."
                    :default 300))
  (let ((args-list (when test-args
                     (mapcar (lambda (a) (string-trim '(#\Space) a))
                             (cl-ppcre:split "," test-args)))))
    (let ((result (amp-test-task
                   test-runner
                   :test-args args-list
                   :repo-path repo-path
                   :fix-failures fix-failures
                   :max-iterations max-iterations
                   :timeout-seconds timeout-seconds)))
      (with-output-to-string (s)
        (amp-report result :stream s)))))

;;; --- Amp JSON tools ---

(defun plist-to-json-ht (plist)
  "Recursively convert a plist to a JSON-compatible hash-table.
   Keyword keys become lowercase underscore strings."
  (cond
    ((and (listp plist) (evenp (length plist))
          (plusp (length plist))
          (loop for (k) on plist by #'cddr always (keywordp k)))
     (let ((ht (make-hash-table :test #'equal)))
       (loop for (k v) on plist by #'cddr
             do (setf (gethash (substitute #\_ #\-
                                 (string-downcase (symbol-name k)))
                               ht)
                      (plist-to-json-ht v)))
       ht))
    ((listp plist) (mapcar #'plist-to-json-ht plist))
    ((vectorp plist) (map 'vector #'plist-to-json-ht plist))
    (t plist)))

(defun json-string (plist)
  "Convert a plist to a JSON string."
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (shasht:write-json (plist-to-json-ht plist) s))))

(define-tool amp-check-json
    (:description "Check Amp CLI status and return structured JSON. Includes enabled state, binary availability, binary path, and allowed repo roots."
     :tool-name "amp_check_json")
  ()
  (json-string (amp-status)))

(define-tool amp-code-json
    (:description "Delegate a coding task to the Amp CLI agent and return structured JSON result. Includes success, output, error, exit code, elapsed time, changed files, and truncation flags."
     :tool-name "amp_code_json")
  ((description "string"
                "Description of the coding task to delegate to Amp."
                :required-p t)
   (repo-path "string"
              "Absolute path to the repository to work in."
              :required-p t)
   (files "string"
          "Comma-separated list of file paths to focus on (relative to repo_path).")
   (context "string"
            "Additional context for the task.")
   (timeout-seconds "integer"
                    "Maximum time in seconds. Default: 300."
                    :default 300))
  (let ((file-list (when files
                     (mapcar (lambda (f) (string-trim '(#\Space) f))
                             (cl-ppcre:split "," files)))))
    (json-string (amp-code-task
                   description
                   :repo-path repo-path
                   :files file-list
                   :context context
                   :timeout-seconds timeout-seconds))))

(define-tool amp-test-json
    (:description "Run a test suite with optional Amp fix mode and return structured JSON result. Includes success, iterations, final output, and elapsed time."
     :tool-name "amp_test_json")
  ((test-runner "string"
                "The test runner executable."
                :required-p t)
   (test-args "string"
              "Comma-separated arguments for the test runner.")
   (repo-path "string"
              "Absolute path to the repository.")
   (fix-failures "boolean"
                 "If true, invoke Amp to fix test failures. Default: false.")
   (max-iterations "integer"
                   "Maximum test/fix cycles. Default: 3."
                   :default 3)
   (timeout-seconds "integer"
                    "Maximum time per Amp fix invocation. Default: 300."
                    :default 300))
  (let ((args-list (when test-args
                     (mapcar (lambda (a) (string-trim '(#\Space) a))
                             (cl-ppcre:split "," test-args)))))
    (json-string (amp-test-task
                   test-runner
                   :test-args args-list
                   :repo-path repo-path
                   :fix-failures fix-failures
                   :max-iterations max-iterations
                   :timeout-seconds timeout-seconds))))
