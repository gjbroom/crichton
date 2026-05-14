;;;; agent/tools-data/github.lisp
;;;;
;;;; Tool definition for GitHub API access.

(in-package #:crichton/agent)

;;; --- GitHub tool ---

(define-tool github
    (:description "Interact with the GitHub API.  Actions: 'repos' (list your repos), 'repo' (get a repo's details), 'issues' (list issues), 'create_issue' (open a new issue), 'prs' (list pull requests), 'ci' (workflow runs), 'releases' (list releases), 'search_code' (code search).  Requires 'github-api-key' credential with :token field (personal access token).")
  ((action "string"
           "The GitHub action to perform."
           :enum ("repos" "repo" "issues" "create_issue" "prs" "ci" "releases" "search_code")
           :required-p t)
   (owner-repo "string"
               "Repository as 'owner/repo'.  Required for repo, issues, create_issue, prs, ci, releases.  For search_code, used as the query if 'query' is omitted.")
   (state "string"
          "Issue/PR state filter: 'open', 'closed', 'all'.  Default: open."
          :enum ("open" "closed" "all"))
   (per-page "integer"
             "Max results per page.  Default: 20."
             :default 20)
   (branch "string"
           "Branch filter for 'ci' action.")
   (query "string"
          "Code search query string (e.g. 'foo repo:owner/repo').  For 'search_code' action.")
   (title "string"
          "Issue title.  Required for 'create_issue'.")
   (body "string"
         "Issue body markdown.  Optional for 'create_issue'.")
   (labels "string"
           "Comma-separated labels to apply.  Optional for 'create_issue'."))
  (cond
    ((string-equal action "create_issue")
     (unless owner-repo
       (return-from handler "Error: 'owner_repo' is required for create_issue."))
     (unless title
       (return-from handler "Error: 'title' is required for create_issue."))
     (let* ((parts (cl-ppcre:split "/" owner-repo :limit 2))
            (owner (first parts))
            (repo  (second parts))
            (label-list (when labels
                          (mapcar (lambda (s) (string-trim '(#\Space) s))
                                  (cl-ppcre:split "," labels))))
            (result (crichton/skills:github-create-issue
                     owner repo title :body body :labels label-list)))
       (format nil "Created issue #~D: ~A~%  ~A"
               (getf result :number) (getf result :title) (getf result :url))))
    (t
     (with-output-to-string (s)
       (crichton/skills:github-report
        action owner-repo
        :state state
        :per-page per-page
        :branch branch
        :query query
        :stream s)))))
