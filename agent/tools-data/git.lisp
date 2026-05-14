;;;; agent/tools-data/git.lisp
;;;;
;;;; Tool definition for local git repository access.

(in-package #:crichton/agent)

;;; --- Local git tool ---

(define-tool git
    (:description "Read and modify local git repositories and files.  LOCAL-ONLY: requires [git] enable = true and allowed_repos in config.  Write operations (write_file, stage, unstage, commit, create_branch, checkout) also require allow_write = true.  Actions: 'status' (git status), 'log' (commit history), 'diff' (changes), 'branches' (list branches), 'worktrees' (list worktrees), 'read_file' (read a local file), 'show' (file at a commit), 'write_file' (write a local file), 'stage' (git add), 'unstage' (git restore --staged), 'commit' (git commit), 'create_branch' (new branch), 'checkout' (switch branch), 'config_status' (skill config).")
  ((action "string"
           "The git action to perform."
           :enum ("status" "log" "diff" "branches" "worktrees" "read_file" "show"
                  "write_file" "stage" "unstage" "commit" "create_branch" "checkout"
                  "config_status")
           :required-p t)
   (repo-path "string"
              "Absolute path to the git repository.  Required for all actions except config_status.")
   (file-path "string"
              "File path (relative to repo or absolute).  Used with read_file, write_file, show, diff, log.")
   (content "string"
            "File content to write.  Required for write_file.")
   (paths "string"
          "Comma-separated file paths to stage/unstage.  Required for stage and unstage.")
   (message "string"
            "Commit message.  Required for commit.")
   (branch-name "string"
                "Branch name.  Required for create_branch and checkout.")
   (ref "string"
        "Git ref (commit, branch, tag).  Used with show and diff (e.g. 'HEAD', 'main', 'HEAD~3').")
   (count "integer"
          "Number of log entries to return.  Default: 20."
          :default 20)
   (all-branches "boolean"
                 "Include remote tracking branches.  Used with branches action.")
   (staged "boolean"
           "Show staged changes only.  Used with diff action."))
  (handler-case
      (cond
        ((string-equal action "config_status")
         (format nil "~S" (crichton/skills:git-config-status)))
        ((string-equal action "status")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (crichton/skills:git-status repo-path))
        ((string-equal action "log")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (crichton/skills:git-log repo-path :count count :path file-path))
        ((string-equal action "diff")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (crichton/skills:git-diff repo-path :ref ref :path file-path :staged staged))
        ((string-equal action "branches")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (crichton/skills:git-branches repo-path :all all-branches))
        ((string-equal action "worktrees")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (crichton/skills:git-worktrees repo-path))
        ((string-equal action "read_file")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless file-path
           (return-from handler "Error: 'file_path' is required for read_file."))
         (crichton/skills:git-read-file repo-path file-path))
        ((string-equal action "show")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless file-path
           (return-from handler "Error: 'file_path' is required for show."))
         (crichton/skills:git-show repo-path ref file-path))
        ((string-equal action "write_file")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless file-path
           (return-from handler "Error: 'file_path' is required for write_file."))
         (when (null content)
           (return-from handler "Error: 'content' is required for write_file."))
         (let ((written (crichton/skills:git-write-file repo-path file-path content)))
           (format nil "Written: ~A" written)))
        ((string-equal action "stage")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless paths
           (return-from handler "Error: 'paths' is required for stage."))
         (let ((path-list (mapcar (lambda (p) (string-trim '(#\Space) p))
                                  (cl-ppcre:split "," paths))))
           (crichton/skills:git-stage repo-path path-list)
           (format nil "Staged: ~{~A~^, ~}" path-list)))
        ((string-equal action "unstage")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless paths
           (return-from handler "Error: 'paths' is required for unstage."))
         (let ((path-list (mapcar (lambda (p) (string-trim '(#\Space) p))
                                  (cl-ppcre:split "," paths))))
           (crichton/skills:git-unstage repo-path path-list)
           (format nil "Unstaged: ~{~A~^, ~}" path-list)))
        ((string-equal action "commit")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless message
           (return-from handler "Error: 'message' is required for commit."))
         (crichton/skills:git-commit repo-path message))
        ((string-equal action "create_branch")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless branch-name
           (return-from handler "Error: 'branch_name' is required for create_branch."))
         (crichton/skills:git-create-branch repo-path branch-name :from-ref ref)
         (format nil "Created and checked out branch: ~A~@[ (from ~A)~]" branch-name ref))
        ((string-equal action "checkout")
         (unless repo-path
           (return-from handler "Error: 'repo_path' is required."))
         (unless branch-name
           (return-from handler "Error: 'branch_name' is required for checkout."))
         (crichton/skills:git-checkout repo-path branch-name)
         (format nil "Checked out: ~A" branch-name))
        (t (format nil "Unknown git action: ~A" action)))
    (error (c)
      (format nil "Git error: ~A" c))))
