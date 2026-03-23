;;;; skills/builtins/git.lisp
;;;;
;;;; Built-in skill: Local git repository operations and file editing.
;;;; Supports reading files, writing files, and git operations (status, log,
;;;; diff, stage, commit, branch management, worktrees).
;;;;
;;;; LOCAL-ONLY: Gated by [git] config section.  Requires explicit
;;;; allowed_repos configuration.  Write operations additionally require
;;;; allow_write = true.  This is the mechanism by which Crichton can
;;;; modify its own source code.
;;;;
;;;; Config example:
;;;;   [git]
;;;;   enable = true
;;;;   allowed_repos = ["/home/user/devel/myrepo"]
;;;;   allow_write = true
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; ====================================================================
;;; Config gate and path policy
;;; ====================================================================

(defun git-enabled-p ()
  "Return T if the git skill is enabled in config.
   Requires [git] enable = true in config.toml."
  (let ((val (crichton/config:config-section-get :git :enable)))
    (cond
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      (t nil))))

(defun git-allowed-repos ()
  "Return the list of allowed repository paths from config.
   Each entry is a directory path string."
  (let ((paths (crichton/config:config-section-get :git :allowed-repos)))
    (cond
      ((null paths) nil)
      ((listp paths) paths)
      ((stringp paths) (list paths))
      (t nil))))

(defun git-allow-write-p ()
  "Return T if write operations are allowed.
   Requires [git] allow_write = true in config.toml."
  (let ((val (crichton/config:config-section-get :git :allow-write)))
    (cond
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      (t nil))))

(defun %git-ensure-trailing-slash (path)
  "Ensure PATH ends with a slash."
  (let ((s (namestring path)))
    (if (char= (char s (1- (length s))) #\/)
        s
        (concatenate 'string s "/"))))

(defun %git-canonical-dir (path)
  "Return a canonical directory namestring for PATH, or NIL if it doesn't exist."
  (handler-case
      (let ((truepath (truename (parse-namestring path))))
        (%git-ensure-trailing-slash truepath))
    (error () nil)))

(defun git-repo-allowed-p (path)
  "Return the matching allowed repo root if PATH is under an allowed root, or NIL.
   An empty allowlist means DENY all (never unrestricted)."
  (let ((roots (git-allowed-repos)))
    (when (null roots)
      (return-from git-repo-allowed-p nil))
    (let ((canonical (handler-case
                         (namestring (truename path))
                       (error () nil))))
      (unless canonical
        (return-from git-repo-allowed-p nil))
      (dolist (root roots nil)
        (let ((canonical-root (%git-canonical-dir root)))
          (when (and canonical-root
                     (>= (length canonical) (length canonical-root))
                     (string= canonical canonical-root
                              :end1 (length canonical-root)))
            (return canonical-root)))))))

(defun %git-validate-enabled ()
  "Check that git skill is enabled.  Signals error if not."
  (unless (git-enabled-p)
    (error "Git skill is disabled (set [git] enable = true in config.toml)")))

(defun %git-validate-repo (repo-path)
  "Validate REPO-PATH is an allowed repository.
   Returns canonical path string.  Signals error if denied."
  (%git-validate-enabled)
  (unless repo-path
    (error "repo_path is required"))
  (let ((canonical (handler-case
                       (namestring (truename repo-path))
                     (error ()
                       (error "Repository path does not exist: ~A" repo-path)))))
    (unless (git-repo-allowed-p canonical)
      (error "Repository ~A is not in the allowed_repos list" canonical))
    canonical))

(defun %git-validate-write ()
  "Check that write operations are permitted.  Signals error if not."
  (unless (git-allow-write-p)
    (error "Git write operations are disabled (set [git] allow_write = true in config.toml)")))

(defun %git-validate-file-path (repo-canonical file-path)
  "Validate that FILE-PATH is within REPO-CANONICAL (the canonical repo root).
   FILE-PATH may be absolute or relative to the repo.
   Returns the absolute path string.  Signals error if denied."
  (let ((abs-path (if (and (> (length file-path) 0)
                           (char= (char file-path 0) #\/))
                      file-path
                      (concatenate 'string
                                   (%git-ensure-trailing-slash repo-canonical)
                                   file-path))))
    ;; Canonicalize to resolve ../ etc.  For new files, check parent dir.
    (let ((canonical (handler-case
                         (namestring (truename abs-path))
                       (error ()
                         ;; File may not exist yet (for writes) — check parent
                         (let* ((pname (pathname abs-path))
                                (parent-dir (directory-namestring pname))
                                (parent-canonical (handler-case
                                                      (namestring (truename parent-dir))
                                                    (error ()
                                                      (error "Parent directory does not exist: ~A" parent-dir)))))
                           (unless (and (>= (length parent-canonical)
                                            (length (%git-ensure-trailing-slash repo-canonical)))
                                        (string= parent-canonical
                                                 (%git-ensure-trailing-slash repo-canonical)
                                                 :end1 (length (%git-ensure-trailing-slash repo-canonical))))
                             (error "File path ~A is outside the repository ~A" file-path repo-canonical))
                           (namestring pname))))))
      ;; For existing files, verify they're under the repo
      (let ((repo-root (%git-ensure-trailing-slash repo-canonical)))
        (when (>= (length canonical) (length repo-root))
          (unless (string= canonical repo-root :end1 (length repo-root))
            (error "File path ~A is outside the repository ~A" file-path repo-canonical))))
      canonical)))

;;; ====================================================================
;;; Git command runner
;;; ====================================================================

(defun %git-run (repo-path args &key (input nil) (error-on-failure t))
  "Run git with ARGS in REPO-PATH.
   Returns (values output-string exit-code).
   Signals error on non-zero exit when ERROR-ON-FAILURE is T."
  (let ((output-stream (make-string-output-stream))
        (error-stream  (make-string-output-stream)))
    (let* ((process (sb-ext:run-program
                     "git" args
                     :directory (namestring (truename repo-path))
                     :output output-stream
                     :error  error-stream
                     :input  input
                     :wait   t
                     :search t))
           (exit-code (sb-ext:process-exit-code process))
           (output    (get-output-stream-string output-stream))
           (err-out   (get-output-stream-string error-stream)))
      (sb-ext:process-close process)
      (when (and error-on-failure (not (zerop exit-code)))
        (error "git ~{~A~^ ~} failed (exit ~D):~%~A~A"
               args exit-code output err-out))
      (values output exit-code))))

;;; ====================================================================
;;; Read operations
;;; ====================================================================

(defun git-status (repo-path &key (short t))
  "Return git status output for REPO-PATH.
   SHORT T (default) uses --short format."
  (let ((canonical (%git-validate-repo repo-path)))
    (let ((args (if short
                    '("status" "--short" "--branch")
                    '("status"))))
      (%git-run canonical args))))

(defun git-log (repo-path &key (count 20) (oneline t) branch path)
  "Return git log for REPO-PATH.
   COUNT limits the number of entries (default 20).
   ONELINE T (default) uses --oneline format.
   BRANCH filters to a specific branch.
   PATH filters to commits touching a specific file path."
  (let ((canonical (%git-validate-repo repo-path)))
    (let ((args (list "log"
                      (format nil "-~D" count)
                      "--no-merges")))
      (when oneline (push "--oneline" (cdr (last args))))
      (when branch  (push branch (cdr (last args))))
      (when path    (progn (push "--" (cdr (last args)))
                           (push path (cdr (last args)))))
      (%git-run canonical args))))

(defun git-diff (repo-path &key ref path (staged nil))
  "Return git diff output for REPO-PATH.
   REF: compare against this ref (e.g. \"HEAD\", \"main\", \"HEAD~3\").
   PATH: restrict diff to this file.
   STAGED T: diff staged changes (--cached)."
  (let ((canonical (%git-validate-repo repo-path)))
    (let ((args (list "diff")))
      (when staged (push "--cached" (cdr (last args))))
      (when ref    (push ref (cdr (last args))))
      (when path
        (push "--" (cdr (last args)))
        (push path (cdr (last args))))
      (%git-run canonical args))))

(defun git-branches (repo-path &key (all nil))
  "List branches in REPO-PATH.
   ALL T includes remote tracking branches."
  (let ((canonical (%git-validate-repo repo-path)))
    (%git-run canonical (if all '("branch" "-a" "-v") '("branch" "-v")))))

(defun git-worktrees (repo-path)
  "List git worktrees for REPO-PATH."
  (let ((canonical (%git-validate-repo repo-path)))
    (%git-run canonical '("worktree" "list"))))

(defun git-read-file (repo-path file-path)
  "Read the contents of FILE-PATH (relative to or absolute within REPO-PATH).
   Returns the file contents as a string.
   FILE-PATH must be within the repository."
  (let ((canonical (%git-validate-repo repo-path)))
    (let ((abs-path (%git-validate-file-path canonical file-path)))
      (handler-case
          (with-open-file (s abs-path :direction :input :external-format :utf-8)
            (let ((contents (make-string (file-length s))))
              (read-sequence contents s)
              contents))
        (error (c)
          (error "Could not read file ~A: ~A" abs-path c))))))

(defun git-show (repo-path ref file-path)
  "Show the content of FILE-PATH at REF (e.g. \"HEAD:path/to/file\").
   Use this to view a file as it exists at a particular commit without
   touching the working tree."
  (let ((canonical (%git-validate-repo repo-path)))
    ;; Validate file-path doesn't escape, but don't require it to exist on disk
    (unless (and file-path (> (length file-path) 0))
      (error "file_path is required for show"))
    ;; Prevent shell injection: file-path must not contain null bytes or newlines
    (when (find #\Nul file-path)
      (error "file_path contains invalid characters"))
    (let ((git-ref (format nil "~A:~A" (or ref "HEAD") file-path)))
      (%git-run canonical (list "show" git-ref)))))

(defun git-config-status ()
  "Return git skill configuration status as a plist."
  (list :enabled (git-enabled-p)
        :allowed-repos (git-allowed-repos)
        :allow-write (git-allow-write-p)))

;;; ====================================================================
;;; Write operations (require allow_write = true)
;;; ====================================================================

(defun git-write-file (repo-path file-path content)
  "Write CONTENT to FILE-PATH within REPO-PATH.
   FILE-PATH must be within the repository.
   Requires [git] allow_write = true."
  (%git-validate-write)
  (let ((canonical (%git-validate-repo repo-path)))
    (let ((abs-path (%git-validate-file-path canonical file-path)))
      ;; Ensure parent directories exist
      (ensure-directories-exist abs-path)
      (handler-case
          (with-open-file (s abs-path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
            (write-string content s))
        (error (c)
          (error "Could not write file ~A: ~A" abs-path c)))
      (log:info "git-write-file: wrote ~A" abs-path)
      abs-path)))

(defun git-stage (repo-path paths)
  "Stage PATHS (a list of strings) in REPO-PATH for commit.
   Each path must be within the repository.
   Requires [git] allow_write = true."
  (%git-validate-write)
  (let ((canonical (%git-validate-repo repo-path)))
    ;; Validate each path
    (dolist (p paths)
      (%git-validate-file-path canonical p))
    (%git-run canonical (list* "add" "--" paths))))

(defun git-unstage (repo-path paths)
  "Unstage PATHS from the index in REPO-PATH.
   Requires [git] allow_write = true."
  (%git-validate-write)
  (let ((canonical (%git-validate-repo repo-path)))
    (dolist (p paths)
      (%git-validate-file-path canonical p))
    (%git-run canonical (list* "restore" "--staged" "--" paths))))

(defun git-commit (repo-path message)
  "Create a commit in REPO-PATH with MESSAGE.
   Requires [git] allow_write = true.
   Never adds Co-Authored-By lines."
  (%git-validate-write)
  (unless (and message (> (length (string-trim '(#\Space #\Newline #\Tab) message)) 0))
    (error "Commit message is required and must not be empty"))
  (let ((canonical (%git-validate-repo repo-path)))
    (%git-run canonical (list "commit" "-m" message))))

(defun git-create-branch (repo-path branch-name &key from-ref)
  "Create a new branch BRANCH-NAME in REPO-PATH.
   FROM-REF: optional starting point (default: current HEAD).
   Requires [git] allow_write = true."
  (%git-validate-write)
  (unless (and branch-name (> (length branch-name) 0))
    (error "branch_name is required"))
  ;; Validate branch name: no shell metacharacters
  (when (find-if (lambda (c) (member c '(#\Space #\\ #\~ #\^ #\: #\? #\* #\[ #\Nul) :test #'char=))
                 branch-name)
    (error "Branch name ~S contains invalid characters" branch-name))
  (let ((canonical (%git-validate-repo repo-path)))
    (let ((args (if from-ref
                    (list "checkout" "-b" branch-name from-ref)
                    (list "checkout" "-b" branch-name))))
      (%git-run canonical args))))

(defun git-checkout (repo-path branch-name)
  "Check out BRANCH-NAME in REPO-PATH.
   Requires [git] allow_write = true."
  (%git-validate-write)
  (unless (and branch-name (> (length branch-name) 0))
    (error "branch_name is required"))
  (when (find-if (lambda (c) (member c '(#\Space #\\ #\~ #\^ #\: #\? #\* #\[ #\Nul) :test #'char=))
                 branch-name)
    (error "Branch name ~S contains invalid characters" branch-name))
  (let ((canonical (%git-validate-repo repo-path)))
    (%git-run canonical (list "checkout" branch-name))))
