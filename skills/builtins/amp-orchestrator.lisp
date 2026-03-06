;;;; skills/builtins/amp-orchestrator.lisp
;;;;
;;;; Built-in skill: Amp CLI coding/testing orchestration.
;;;; Allows Crichton to delegate coding and testing tasks to the Amp CLI
;;;; agent, acting as a supervisor that plans work and dispatches
;;;; implementation.
;;;;
;;;; Integration is CLI-based: invokes `amp` with structured prompts,
;;;; captures output, and tracks usage via the metering system.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

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

;;; --- Stream reading helpers ---

(defun read-stream-to-string (stream)
  "Read all content from STREAM into a string."
  (with-output-to-string (s)
    (loop for line = (read-line stream nil nil)
          while line do (write-line line s))))

;;; --- Core invocation ---

(defun wait-for-thread (thread timeout-seconds)
  "Wait for THREAD to finish, returning T if it completed within TIMEOUT-SECONDS.
   Returns NIL if it timed out. Uses polling with sleep."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop
      (unless (bt:thread-alive-p thread)
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.5))))

(defun amp-invoke (prompt &key repo-path (timeout-seconds 300))
  "Invoke the amp CLI with PROMPT. Returns a plist:
   (:output stdout :error stderr :exit-code N :elapsed-seconds N :timed-out-p BOOL).
   REPO-PATH sets the working directory. TIMEOUT-SECONDS defaults to 300 (5 min)."
  (let ((binary (amp-binary)))
    (unless binary
      (return-from amp-invoke
        (list :output "" :error "amp binary not found on PATH"
              :exit-code -1 :elapsed-seconds 0 :timed-out-p nil)))
    (let* ((start-time (get-internal-real-time))
           (directory (when repo-path (namestring (truename repo-path))))
           (process (sb-ext:run-program binary
                                        (list "--execute"
                                              "--dangerously-allow-all"
                                              "--no-notifications"
                                              "--no-ide"
                                              prompt)
                                        :output :stream
                                        :error :stream
                                        :wait nil
                                        :directory directory))
           (stdout "")
           (stderr "")
           (timed-out nil)
           (worker (bt:make-thread
                    (lambda ()
                      (setf stdout (read-stream-to-string
                                    (sb-ext:process-output process)))
                      (setf stderr (read-stream-to-string
                                    (sb-ext:process-error process)))
                      (sb-ext:process-wait process))
                    :name "amp-invoke-worker")))
      (unwind-protect
           (progn
             (unless (wait-for-thread worker timeout-seconds)
               (setf timed-out t)
               (log:warn "Amp invocation timed out after ~Ds, killing process"
                         timeout-seconds)
               (handler-case (sb-ext:process-kill process 15 :process)
                 (error (c) (declare (ignore c))))
               (sleep 1)
               (when (sb-ext:process-alive-p process)
                 (handler-case (sb-ext:process-kill process 9 :process)
                   (error (c) (declare (ignore c))))))
             (let* ((end-time (get-internal-real-time))
                    (elapsed (/ (- end-time start-time)
                                (float internal-time-units-per-second 1.0d0)))
                    (exit-code (or (sb-ext:process-exit-code process) -1)))
               (record-usage "amp" "cli-invocation"
                             (length prompt) (length stdout))
               (log:info "Amp invocation completed: exit=~D elapsed=~,1Fs output=~D chars"
                         exit-code elapsed (length stdout))
               (list :output (string-trim '(#\Newline #\Return #\Space) stdout)
                     :error (string-trim '(#\Newline #\Return #\Space) stderr)
                     :exit-code exit-code
                     :elapsed-seconds (round elapsed)
                     :timed-out-p timed-out)))
        (sb-ext:process-close process)))))

;;; --- High-level coding task ---

(defun build-code-prompt (description &key files context)
  "Build a structured prompt for a coding task."
  (with-output-to-string (s)
    (write-string description s)
    (when files
      (format s "~%~%Files to focus on:~%")
      (dolist (f files)
        (format s "- ~A~%" f)))
    (when context
      (format s "~%~%Additional context:~%~A" context))))

(defun amp-code-task (description &key repo-path files context (timeout-seconds 300))
  "Delegate a coding task to Amp. Builds a structured prompt from
   DESCRIPTION, FILES list, and CONTEXT string. Returns a plist:
   (:success BOOL :output STRING :error STRING :exit-code N :elapsed-seconds N)."
  (let* ((prompt (build-code-prompt description :files files :context context))
         (result (amp-invoke prompt :repo-path repo-path
                                    :timeout-seconds timeout-seconds))
         (success (and (zerop (getf result :exit-code))
                       (not (getf result :timed-out-p)))))
    (list :success success
          :output (getf result :output)
          :error (getf result :error)
          :exit-code (getf result :exit-code)
          :elapsed-seconds (getf result :elapsed-seconds))))

;;; --- High-level testing task ---

(defun run-shell-command (command &key repo-path)
  "Run a shell command via /bin/sh -c. Returns (:output :error :exit-code)."
  (let* ((directory (when repo-path (namestring (truename repo-path))))
         (process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                      :output :stream
                                      :error :stream
                                      :wait t
                                      :directory directory)))
    (let ((stdout (read-stream-to-string (sb-ext:process-output process)))
          (stderr (read-stream-to-string (sb-ext:process-error process)))
          (exit-code (sb-ext:process-exit-code process)))
      (sb-ext:process-close process)
      (list :output stdout :error stderr :exit-code exit-code))))

(defun run-single-test-iteration (test-command repo-path iteration-number)
  "Run TEST-COMMAND once and return a test-iteration plist.
   Includes :test-passed, :test-exit-code, :test-output, :test-error,
   :elapsed-seconds, and stub :fix-attempted/:fix-result fields."
  (let* ((start (get-internal-real-time))
         (test-result (run-shell-command test-command :repo-path repo-path))
         (end (get-internal-real-time))
         (elapsed (round (/ (- end start)
                             (float internal-time-units-per-second 1.0d0)))))
    (list :iteration iteration-number
          :test-passed (zerop (getf test-result :exit-code))
          :test-exit-code (getf test-result :exit-code)
          :test-output (getf test-result :output)
          :test-error (getf test-result :error)
          :elapsed-seconds elapsed
          :fix-attempted nil
          :fix-result nil)))

(defun attempt-fix (iteration test-command repo-path timeout-seconds)
  "Invoke Amp to fix a failed test ITERATION. Mutates the iteration plist
   to record the fix attempt. Returns the fix elapsed seconds."
  (let* ((fix-prompt
           (format nil "The following test command failed:~%~%  ~A~%~%~
                        Exit code: ~D~%~%~
                        Test output:~%~A~%~%~
                        Error output:~%~A~%~%~
                        Please fix the code so the tests pass."
                   test-command
                   (getf iteration :test-exit-code)
                   (getf iteration :test-output)
                   (getf iteration :test-error)))
         (fix-result (amp-invoke fix-prompt
                                :repo-path repo-path
                                :timeout-seconds timeout-seconds)))
    (setf (getf iteration :fix-attempted) t
          (getf iteration :fix-result) (getf fix-result :output))
    (getf fix-result :elapsed-seconds)))

(defun amp-test-task (test-command &key repo-path fix-failures (max-iterations 3)
                                     (timeout-seconds 300))
  "Run TEST-COMMAND and optionally use Amp to fix failures.
   Returns a plist with :success, :iterations (list of attempt plists),
   :final-output, and :total-elapsed-seconds."
  (let ((iterations nil)
        (total-elapsed 0)
        (success nil))
    (dotimes (i max-iterations)
      (let ((iteration (run-single-test-iteration
                        test-command repo-path (1+ i))))
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
                 (attempt-fix iteration test-command repo-path timeout-seconds))
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
     (let ((output (getf result :output)))
       (when (and output (plusp (length output)))
         (format stream "~%Output:~%~A~%" output)))
     (let ((error-text (getf result :error)))
       (when (and error-text (plusp (length error-text)))
         (format stream "~%Errors:~%~A~%" error-text))))
    (t
     (format stream "No result data.~%")))
  result)
