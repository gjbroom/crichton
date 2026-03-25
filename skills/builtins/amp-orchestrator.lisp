;;;; skills/builtins/amp-orchestrator.lisp
;;;;
;;;; Built-in skill: Amp CLI coding/testing orchestration.
;;;; Allows Crichton to delegate coding and testing tasks to the Amp CLI
;;;; agent, acting as a supervisor that plans work and dispatches
;;;; implementation.
;;;;
;;;; Security model:
;;;;   - Config gate: [amp] enable = true required in config.toml
;;;;   - Repo allowlist: allowed_repo_roots restricts where Amp may operate
;;;;   - Credential mediation: API key resolved from credential store,
;;;;     passed via minimal subprocess environment
;;;;   - No shell execution: all subprocesses use direct argv invocation
;;;;   - Structured audit logging for every invocation attempt
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Output limits ---

(defparameter +amp-stdout-cap-chars+ 20000
  "Maximum characters to retain from Amp stdout.")

(defparameter +amp-stderr-cap-chars+ 8000
  "Maximum characters to retain from Amp stderr.")

;;; --- Amp binary detection ---

(defvar *amp-binary* nil
  "Cached path to the amp binary, or NIL if not yet searched.")

(defun find-amp-binary ()
  "Find the amp binary on PATH. Returns the path string or NIL."
  (handler-case
      (let* ((process (sb-ext:run-program "/usr/bin/env" (list "which" "amp")
                                           :output :stream
                                           :error nil
                                           :wait t))
             (output (when (zerop (sb-ext:process-exit-code process))
                       (string-trim '(#\Newline #\Return #\Space)
                                    (with-output-to-string (s)
                                      (loop for line = (read-line
                                                        (sb-ext:process-output process)
                                                        nil nil)
                                            while line do (write-string line s)))))))
        (sb-ext:process-close process)
        (when (and output (plusp (length output)))
          output))
    (error () nil)))

(defun amp-binary ()
  "Return cached path to the amp binary, searching PATH if needed.
   Returns the path string or NIL (does not signal an error)."
  (or *amp-binary*
      (setf *amp-binary* (find-amp-binary))))

(defun amp-available-p ()
  "Return T if the amp CLI is available on PATH."
  (not (null (amp-binary))))

;;; --- Config gate and policy ---

(defun amp-enabled-p ()
  "Return T if Amp orchestration is enabled in config.
   Requires [amp] enable = true in config.toml."
  (let ((val (crichton/config:config-section-get :amp :enable)))
    (cond
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      (t nil))))

(defun amp-allowed-repo-roots ()
  "Return the list of allowed repository roots from config.
   Each entry is a directory path string."
  (let ((roots (crichton/config:config-section-get :amp :allowed-repo-roots)))
    (cond
      ((null roots) nil)
      ((listp roots) roots)
      (t nil))))

(defun %ensure-trailing-slash (path)
  "Ensure PATH ends with a slash."
  (let ((s (namestring path)))
    (if (char= (char s (1- (length s))) #\/)
        s
        (concatenate 'string s "/"))))

(defun %canonical-dir (path)
  "Return a canonical directory namestring for PATH, or NIL if it doesn't exist."
  (handler-case
      (let ((truepath (truename (parse-namestring path))))
        (%ensure-trailing-slash truepath))
    (error () nil)))

(defun amp-repo-allowed-p (repo-path)
  "Return the matching allowed root if REPO-PATH is under an allowed root,
   or T if the allowlist is empty (unrestricted).  Returns NIL if denied."
  (let ((roots (amp-allowed-repo-roots)))
    (when (null roots)
      (return-from amp-repo-allowed-p t))
    (let ((canonical-repo (%canonical-dir repo-path)))
      (unless canonical-repo
        (return-from amp-repo-allowed-p nil))
      (dolist (root roots nil)
        (let ((canonical-root (%canonical-dir root)))
          (when (and canonical-root
                     (>= (length canonical-repo) (length canonical-root))
                     (string= canonical-repo canonical-root
                              :end1 (length canonical-root)))
            (return canonical-root)))))))

(defun validate-amp-invocation (repo-path)
  "Validate that Amp invocation is permitted.  Returns a policy decision plist:
   (:allowed-p T/NIL :enabled-p T/NIL :repo-allowed-p T/NIL
    :denial-reason STRING-or-NIL :matched-root STRING-or-NIL)."
  (let ((enabled (amp-enabled-p))
        (binary-present (amp-available-p)))
    (cond
      ((not enabled)
       (list :allowed-p nil :enabled-p nil :repo-allowed-p nil
             :denial-reason "Amp orchestration is disabled in config (amp.enable = false)"))
      ((not binary-present)
       (list :allowed-p nil :enabled-p t :repo-allowed-p nil
             :denial-reason "Amp CLI binary not found on PATH"))
      ((null repo-path)
       (list :allowed-p nil :enabled-p t :repo-allowed-p nil
             :denial-reason "repo-path is required"))
      (t
       (let ((matched-root (amp-repo-allowed-p repo-path)))
         (if matched-root
             (list :allowed-p t :enabled-p t :repo-allowed-p t
                   :matched-root (if (eq matched-root t) "unrestricted" matched-root))
             (list :allowed-p nil :enabled-p t :repo-allowed-p nil
                   :denial-reason (format nil "repo-path ~A is not under any allowed root"
                                          repo-path))))))))

(defun amp-status ()
  "Return a plist describing current Amp orchestration status."
  (list :enabled (amp-enabled-p)
        :binary-available (amp-available-p)
        :binary-path (amp-binary)
        :allowed-repo-roots (amp-allowed-repo-roots)))

;;; --- Credential mediation ---

(defun resolve-amp-api-key ()
  "Resolve the Amp API key from the credential store.
   Returns the API key string, or signals an error."
  (handler-case
      (crichton/credentials:resolve-credential "amp-api" :api-key)
    (error (c)
      (error "Cannot resolve Amp API key from credential store (amp-api): ~A" c))))

(defun amp-child-environment ()
  "Build a minimal environment for the Amp subprocess.
   Only HOME, PATH, and the API key are passed."
  (let ((home (sb-ext:posix-getenv "HOME"))
        (path (sb-ext:posix-getenv "PATH"))
        (api-key (resolve-amp-api-key)))
    (remove nil
            (list (when home (format nil "HOME=~A" home))
                  (when path (format nil "PATH=~A" path))
                  (format nil "AMP_API_KEY=~A" api-key)))))

;;; --- ANSI stripping and output processing ---

(defvar *ansi-escape-regex*
  (let ((esc (string (code-char 27))))
    (concatenate 'string esc "\\[[0-9;?]*[ -/]*[@-~]"))
  "Precompiled regex string matching ANSI escape sequences.")

(defun strip-ansi-escapes (s)
  "Remove ANSI escape sequences from string S."
  (cl-ppcre:regex-replace-all *ansi-escape-regex* s ""))

(defun truncate-output (s cap)
  "Truncate string S to CAP characters.
   Returns (values truncated-string truncated-p)."
  (if (<= (length s) cap)
      (values s nil)
      (values (subseq s 0 cap) t)))

;;; --- Prompt input sanitization ---

(defparameter +amp-max-description-length+ 10000
  "Maximum length for a coding task description passed to Amp.")

(defparameter +amp-max-context-length+ 5000
  "Maximum length for additional context passed to Amp.")

(defparameter +amp-max-test-output-length+ 8000
  "Maximum length of test output embedded in an Amp fix prompt.")

(defparameter *amp-injection-patterns*
  '("(?i)ignore\\s+(all\\s+)?(previous|prior)\\s+instructions?"
    "(?i)\\bsystem\\s*:\\s"
    "(?i)\\bnew\\s+instructions?\\s*:"
    "(?i)\\bforget\\s+(everything|all|prior)"
    "(?i)\\[/?INST\\]"
    "(?i)<\\|system\\|>"
    "(?i)<\\|user\\|>"
    "(?i)<\\|assistant\\|>")
  "Patterns indicating potential prompt injection in Amp prompt inputs.")

(defun sanitize-for-amp-prompt (text &key (max-length +amp-max-description-length+) label)
  "Sanitize TEXT before embedding it in an Amp CLI prompt.
   - Strips null bytes and non-printable control characters.
   - Truncates to MAX-LENGTH (logs a warning if truncated).
   - Detects and audit-logs potential injection markers.
   LABEL is used in log and audit messages for context.
   Returns the sanitised string, or NIL if TEXT is NIL."
  (when (null text)
    (return-from sanitize-for-amp-prompt nil))
  (let* ((stripped (with-output-to-string (s)
                     (loop for ch across text
                           when (or (char>= ch #\Space)
                                    (char= ch #\Newline)
                                    (char= ch #\Tab)
                                    (char= ch #\Return))
                             do (write-char ch s))))
         (truncated (if (> (length stripped) max-length)
                        (progn
                          (log:warn "Amp prompt input truncated (~D → ~D chars)~@[ [~A]~]"
                                    (length stripped) max-length label)
                          (subseq stripped 0 max-length))
                        stripped))
         (marker-count (loop for pattern in *amp-injection-patterns*
                             count (when (cl-ppcre:scan pattern truncated) 1))))
    (when (plusp marker-count)
      (log:warn "~D injection marker(s) in Amp prompt input~@[ [~A]~] — audit event written"
                marker-count label)
      (let ((fields (make-hash-table :test #'equal)))
        (setf (gethash "label" fields) (or label "unknown")
              (gethash "marker_count" fields) marker-count
              (gethash "input_length" fields) (length truncated))
        (crichton/logging:write-audit-event "amp.prompt.injection_markers" fields)))
    truncated))

;;; --- Git status tracking ---

(defun git-status-snapshot (repo-path)
  "Capture a git status snapshot for REPO-PATH.
   Returns a list of (status . path) cons cells, or NIL if not a git repo."
  (handler-case
      (let* ((process (sb-ext:run-program "git"
                                           (list "status" "--porcelain=v1"
                                                 "--untracked-files=all")
                                           :output :stream
                                           :error nil
                                           :directory (namestring (truename repo-path))
                                           :wait t))
             (lines (when (zerop (sb-ext:process-exit-code process))
                      (loop for line = (read-line (sb-ext:process-output process) nil nil)
                            while line
                            when (>= (length line) 4)
                              collect (cons (subseq line 0 2)
                                            (string-trim '(#\Space) (subseq line 3)))))))
        (sb-ext:process-close process)
        lines)
    (error () nil)))

(defun diff-git-snapshots (before after)
  "Return a list of file paths that changed between BEFORE and AFTER snapshots."
  (let ((before-ht (make-hash-table :test #'equal))
        (changed nil))
    (dolist (entry before)
      (setf (gethash (cdr entry) before-ht) (car entry)))
    (dolist (entry after)
      (let ((old-status (gethash (cdr entry) before-ht :absent)))
        (when (or (eq old-status :absent)
                  (not (string= old-status (car entry))))
          (pushnew (cdr entry) changed :test #'equal))))
    ;; Also track deletions (in before but not in after)
    (dolist (entry before)
      (unless (find (cdr entry) after :key #'cdr :test #'equal)
        (pushnew (cdr entry) changed :test #'equal)))
    (sort changed #'string<)))

;;; --- Structured audit logging ---

(defun log-amp-audit-event (event-type &key repo argv policy
                                         exit-code elapsed-seconds
                                         timed-out-p changed-files
                                         prompt-length)
  "Write a structured audit event for an Amp invocation.
   Never logs raw stdout/stderr content."
  (let ((fields (make-hash-table :test #'equal)))
    (when repo
      (setf (gethash "repo" fields) repo))
    (when argv
      ;; Sanitize: replace the prompt argument with a placeholder
      (setf (gethash "argv" fields)
            (coerce (mapcar (lambda (a)
                              (if (> (length a) 100) "<prompt>" a))
                            argv)
                    'vector)))
    (when prompt-length
      (setf (gethash "prompt_length" fields) prompt-length))
    (setf (gethash "credential_names" fields)
          (coerce '("amp-api") 'vector))
    (when policy
      (let ((policy-ht (make-hash-table :test #'equal)))
        (loop for (k v) on policy by #'cddr
              do (setf (gethash (substitute #\_ #\-
                                  (string-downcase (symbol-name k)))
                                policy-ht)
                       v))
        (setf (gethash "policy" fields) policy-ht)))
    (when exit-code
      (setf (gethash "exit_code" fields) exit-code))
    (when elapsed-seconds
      (setf (gethash "elapsed_seconds" fields) elapsed-seconds))
    (when timed-out-p
      (setf (gethash "timed_out" fields) t))
    (when changed-files
      (setf (gethash "changed_files" fields)
            (coerce changed-files 'vector)))
    (crichton/logging:write-audit-event event-type fields)))

;;; --- Direct process execution ---

(defun run-command-argv (program argv &key directory environment (timeout-seconds 300))
  "Run PROGRAM with ARGV directly (no shell).  Returns a plist:
   (:output STRING :error STRING :exit-code N :elapsed-seconds N
    :timed-out-p BOOL :output-truncated-p BOOL :error-truncated-p BOOL).
   Output is ANSI-stripped and truncated to caps."
  (let* ((start-time (get-internal-real-time))
         (process (sb-ext:run-program program argv
                                       :output :stream
                                       :error :stream
                                       :wait nil
                                       :directory directory
                                       :environment environment))
         (stdout "")
         (stderr "")
         (timed-out nil))
    (unwind-protect
         (progn
           ;; Poll for completion with timeout
           (let ((deadline (+ (get-internal-real-time)
                              (* timeout-seconds internal-time-units-per-second))))
             (loop
               (unless (sb-ext:process-alive-p process)
                 (return))
               (when (>= (get-internal-real-time) deadline)
                 (setf timed-out t)
                 (log:warn "Process timed out after ~Ds, killing" timeout-seconds)
                 (handler-case (sb-ext:process-kill process 15 :process)
                   (error (c) (declare (ignore c))))
                 (sleep 1)
                 (when (sb-ext:process-alive-p process)
                   (handler-case (sb-ext:process-kill process 9 :process)
                     (error (c) (declare (ignore c)))))
                 ;; Wait briefly for process to fully exit after kill
                 (handler-case (sb-ext:process-wait process t)
                   (error (c) (declare (ignore c))))
                 (return))
               (sleep 0.5)))
           ;; Read output after process finishes (avoids stream read issues)
           (setf stdout (with-output-to-string (s)
                          (loop for line = (read-line
                                            (sb-ext:process-output process) nil nil)
                                while line do (write-line line s))))
           (setf stderr (with-output-to-string (s)
                          (loop for line = (read-line
                                            (sb-ext:process-error process) nil nil)
                                while line do (write-line line s))))
           ;; Strip ANSI and truncate
           (setf stdout (strip-ansi-escapes
                         (string-trim '(#\Newline #\Return #\Space) stdout)))
           (setf stderr (strip-ansi-escapes
                         (string-trim '(#\Newline #\Return #\Space) stderr)))
           (multiple-value-bind (out out-trunc) (truncate-output stdout +amp-stdout-cap-chars+)
             (multiple-value-bind (err err-trunc) (truncate-output stderr +amp-stderr-cap-chars+)
               (let* ((end-time (get-internal-real-time))
                      (elapsed (/ (- end-time start-time)
                                  (float internal-time-units-per-second 1.0d0)))
                      (exit-code (or (sb-ext:process-exit-code process) -1)))
                 (list :output out
                       :error err
                       :exit-code exit-code
                       :elapsed-seconds (round elapsed)
                       :timed-out-p timed-out
                       :output-truncated-p out-trunc
                       :error-truncated-p err-trunc)))))
      (sb-ext:process-close process))))

;;; --- Core invocation ---

(defun %amp-invoke-internal (prompt &key repo-path (timeout-seconds 300))
  "Internal Amp invocation.  Assumes policy already validated."
  (let* ((binary (amp-binary))
         (directory (when repo-path (namestring (truename repo-path))))
         (argv (list "--execute"
                     "--dangerously-allow-all"
                     "--no-notifications"
                     "--no-ide"
                     prompt))
         (environment (amp-child-environment))
         (git-before (when repo-path (git-status-snapshot repo-path)))
         (result (run-command-argv binary argv
                                   :directory directory
                                   :environment environment
                                   :timeout-seconds timeout-seconds))
         (git-after (when repo-path (git-status-snapshot repo-path)))
         (changed-files (when (and git-before git-after)
                          (diff-git-snapshots git-before git-after))))
    ;; Record usage
    (record-usage "amp" "cli-invocation"
                  (length prompt) (length (getf result :output)))
    (log:info "Amp invocation completed: exit=~D elapsed=~,1Fs output=~D chars"
              (getf result :exit-code)
              (getf result :elapsed-seconds)
              (length (getf result :output)))
    ;; Add changed-files to result
    (append result (list :changed-files changed-files))))

(defun amp-invoke (prompt &key repo-path (timeout-seconds 300))
  "Invoke the amp CLI with PROMPT. Returns a plist:
   (:output stdout :error stderr :exit-code N :elapsed-seconds N :timed-out-p BOOL
    :output-truncated-p BOOL :error-truncated-p BOOL :changed-files LIST
    :policy-decision PLIST).
   REPO-PATH sets the working directory. TIMEOUT-SECONDS defaults to 300 (5 min).
   Validates config gate, repo allowlist, and credential availability before invocation."
  (let ((policy (validate-amp-invocation repo-path)))
    (unless (getf policy :allowed-p)
      ;; Log audit event for denial
      (log-amp-audit-event "amp.invoke.denied"
                           :repo repo-path
                           :policy policy
                           :prompt-length (length prompt))
      (return-from amp-invoke
        (list :output ""
              :error (or (getf policy :denial-reason) "Amp invocation denied by policy")
              :exit-code -1
              :elapsed-seconds 0
              :timed-out-p nil
              :output-truncated-p nil
              :error-truncated-p nil
              :changed-files nil
              :policy-decision policy)))
    ;; Restarts
    (restart-case
        (let ((result (%amp-invoke-internal prompt
                                            :repo-path repo-path
                                            :timeout-seconds timeout-seconds)))
          ;; Log audit event for completion
          (log-amp-audit-event "amp.invoke"
                               :repo repo-path
                               :argv (list "--execute" "--dangerously-allow-all"
                                           "--no-notifications" "--no-ide" "<prompt>")
                               :policy policy
                               :exit-code (getf result :exit-code)
                               :elapsed-seconds (getf result :elapsed-seconds)
                               :timed-out-p (getf result :timed-out-p)
                               :changed-files (getf result :changed-files)
                               :prompt-length (length prompt))
          (append result (list :policy-decision policy)))
      (use-different-repo (new-repo-path)
        :report (lambda (s) (format s "Retry with a different repo path"))
        :interactive (lambda ()
                       (format *query-io* "~&New repo path: ")
                       (list (read-line *query-io*)))
        (amp-invoke prompt :repo-path new-repo-path :timeout-seconds timeout-seconds))
      (retry-with-longer-timeout (new-timeout)
        :report (lambda (s) (format s "Retry with a longer timeout"))
        :interactive (lambda ()
                       (format *query-io* "~&New timeout (seconds): ")
                       (list (parse-integer (read-line *query-io*))))
        (amp-invoke prompt :repo-path repo-path :timeout-seconds new-timeout))
      (return-partial-result ()
        :report (lambda (s) (format s "Return whatever partial output was captured"))
        ;; This restart is primarily useful when invoked from a handler-bind
        ;; around a timeout condition.  Return a timeout result.
        (list :output "" :error "Invocation aborted via return-partial-result restart"
              :exit-code -1 :elapsed-seconds 0 :timed-out-p t
              :output-truncated-p nil :error-truncated-p nil
              :changed-files nil :policy-decision policy)))))

;;; --- High-level coding task ---

(defun build-code-prompt (description &key files context)
  "Build a structured prompt for a coding task.
   Sanitizes DESCRIPTION and CONTEXT before embedding."
  (let ((safe-description (sanitize-for-amp-prompt description
                                                   :max-length +amp-max-description-length+
                                                   :label "description"))
        (safe-context (when context
                        (sanitize-for-amp-prompt context
                                                 :max-length +amp-max-context-length+
                                                 :label "context"))))
    (with-output-to-string (s)
      (write-string safe-description s)
      (when files
        (format s "~%~%Files to focus on:~%")
        (dolist (f files)
          (format s "- ~A~%" f)))
      (when safe-context
        (format s "~%~%Additional context:~%~A" safe-context)))))

(defun amp-code-task (description &key repo-path files context (timeout-seconds 300))
  "Delegate a coding task to Amp. Builds a structured prompt from
   DESCRIPTION, FILES list, and CONTEXT string. Returns a plist:
   (:success BOOL :output STRING :error STRING :exit-code N :elapsed-seconds N
    :changed-files LIST :output-truncated-p BOOL :error-truncated-p BOOL)."
  (let* ((prompt (build-code-prompt description :files files :context context))
         (result (amp-invoke prompt :repo-path repo-path
                                    :timeout-seconds timeout-seconds))
         (success (and (zerop (getf result :exit-code))
                       (not (getf result :timed-out-p)))))
    (list :success success
          :output (getf result :output)
          :error (getf result :error)
          :exit-code (getf result :exit-code)
          :elapsed-seconds (getf result :elapsed-seconds)
          :changed-files (getf result :changed-files)
          :output-truncated-p (getf result :output-truncated-p)
          :error-truncated-p (getf result :error-truncated-p))))

;;; --- Direct test execution ---

(defun run-test-command (test-runner test-args &key repo-path (timeout-seconds 60))
  "Run TEST-RUNNER with TEST-ARGS directly (no shell).
   Returns (:output :error :exit-code :elapsed-seconds :timed-out-p
            :output-truncated-p :error-truncated-p)."
  (let ((directory (when repo-path (namestring (truename repo-path)))))
    (run-command-argv test-runner (or test-args nil)
                      :directory directory
                      :timeout-seconds timeout-seconds)))

(defun run-single-test-iteration (test-runner test-args repo-path iteration-number)
  "Run test once and return a test-iteration plist.
   Includes :test-passed, :test-exit-code, :test-output, :test-error,
   :elapsed-seconds, and stub :fix-attempted/:fix-result fields."
  (let ((result (run-test-command test-runner test-args :repo-path repo-path)))
    (list :iteration iteration-number
          :test-passed (zerop (getf result :exit-code))
          :test-exit-code (getf result :exit-code)
          :test-output (getf result :output)
          :test-error (getf result :error)
          :elapsed-seconds (getf result :elapsed-seconds)
          :fix-attempted nil
          :fix-result nil)))

(defun attempt-fix (iteration test-runner test-args repo-path timeout-seconds)
  "Invoke Amp to fix a failed test ITERATION. Mutates the iteration plist
   to record the fix attempt. Returns the fix elapsed seconds.
   Sanitizes test output before embedding in the fix prompt."
  (let* ((command-str (format nil "~A~{ ~A~}" test-runner test-args))
         (safe-output (sanitize-for-amp-prompt (getf iteration :test-output)
                                               :max-length +amp-max-test-output-length+
                                               :label "test-output"))
         (safe-error (sanitize-for-amp-prompt (getf iteration :test-error)
                                              :max-length +amp-max-test-output-length+
                                              :label "test-error"))
         (fix-prompt
           (format nil "The following test command failed:~%~%  ~A~%~%~
                        Exit code: ~D~%~%~
                        Test output:~%~A~%~%~
                        Error output:~%~A~%~%~
                        Please fix the code so the tests pass."
                   command-str
                   (getf iteration :test-exit-code)
                   safe-output
                   safe-error))
         (fix-result (amp-invoke fix-prompt
                                :repo-path repo-path
                                :timeout-seconds timeout-seconds)))
    (setf (getf iteration :fix-attempted) t
          (getf iteration :fix-result) (getf fix-result :output))
    (getf fix-result :elapsed-seconds)))

(defun amp-test-task (test-runner &key test-args repo-path fix-failures
                                    (max-iterations 3) (timeout-seconds 300))
  "Run TEST-RUNNER with TEST-ARGS and optionally use Amp to fix failures.
   Returns a plist with :success, :iterations (list of attempt plists),
   :final-output, and :total-elapsed-seconds."
  (let ((iterations nil)
        (total-elapsed 0)
        (success nil))
    (dotimes (i max-iterations)
      (let ((iteration (run-single-test-iteration
                        test-runner test-args repo-path (1+ i))))
        (incf total-elapsed (getf iteration :elapsed-seconds))
        (cond
          ((getf iteration :test-passed)
           (setf success t)
           (push iteration iterations)
           (log:info "Tests passed on iteration ~D" (1+ i))
           (return))
          ((and fix-failures (< i (1- max-iterations)))
           (log:info "Tests failed (iteration ~D), invoking Amp to fix" (1+ i))
           (incf total-elapsed
                 (attempt-fix iteration test-runner test-args
                              repo-path timeout-seconds))
           (push iteration iterations))
          (t
           (push iteration iterations)
           (log:info "Tests failed on final iteration ~D" (1+ i))
           (return)))))
    (let ((final-iterations (nreverse iterations)))
      (list :success success
            :iterations final-iterations
            :iteration-count (length final-iterations)
            :final-output (when final-iterations
                            (getf (car (last final-iterations)) :test-output))
            :total-elapsed-seconds total-elapsed))))

;;; --- Report formatting ---

(defun amp-report (result &key (stream *standard-output*))
  "Format a human-readable report from an Amp task result plist.
   Handles both code-task and test-task results."
  (cond
    ((getf result :iterations)
     (format stream "=== Amp Test Task Report ===~%")
     (format stream "Success: ~A~%" (if (getf result :success) "Yes" "No"))
     (format stream "Iterations: ~D~%" (getf result :iteration-count))
     (format stream "Total elapsed: ~Ds~%" (getf result :total-elapsed-seconds))
     (dolist (iter (getf result :iterations))
       (format stream "~%--- Iteration ~D ---~%" (getf iter :iteration))
       (format stream "  Tests passed: ~A~%" (if (getf iter :test-passed) "Yes" "No"))
       (format stream "  Exit code: ~D~%" (getf iter :test-exit-code))
       (format stream "  Elapsed: ~Ds~%" (getf iter :elapsed-seconds))
       (when (getf iter :fix-attempted)
         (format stream "  Fix attempted: Yes~%"))))
    ((getf result :output)
     (format stream "=== Amp Code Task Report ===~%")
     (format stream "Success: ~A~%" (if (getf result :success) "Yes" "No"))
     (when (getf result :exit-code)
       (format stream "Exit code: ~D~%" (getf result :exit-code)))
     (when (getf result :elapsed-seconds)
       (format stream "Elapsed: ~Ds~%" (getf result :elapsed-seconds)))
     (when (getf result :output-truncated-p)
       (format stream "Output: TRUNCATED (>~D chars)~%" +amp-stdout-cap-chars+))
     (when (getf result :changed-files)
       (format stream "Changed files:~%")
       (dolist (f (getf result :changed-files))
         (format stream "  ~A~%" f)))
     (let ((output (getf result :output)))
       (when (and output (plusp (length output)))
         (format stream "~%Output:~%~A~%" output)))
     (let ((error-text (getf result :error)))
       (when (and error-text (plusp (length error-text)))
         (format stream "~%Errors:~%~A~%" error-text)))))
  result)
