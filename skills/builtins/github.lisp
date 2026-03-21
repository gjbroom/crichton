;;;; skills/builtins/github.lisp
;;;;
;;;; Built-in skill: GitHub API interaction.
;;;; Supports repos, issues, PRs, CI/Actions, releases, and code search.
;;;;
;;;; Credentials: store personal access token as 'github-api-key':
;;;;   (crichton/credentials:store-credential
;;;;    "github-api-key" '(:token "ghp_..."))
;;;;
;;;; This is a daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

(defparameter *github-api-base* "https://api.github.com"
  "GitHub REST API v3 base URL.")

(defparameter *github-credential-name* "github-api-key"
  "Credential store name for the GitHub PAT.")

;;; --- HTTP helpers ---

(defun github-token ()
  "Resolve the GitHub PAT from the credential store."
  (handler-case
      (crichton/credentials:resolve-credential *github-credential-name* :token)
    (error (c)
      (error "GitHub credential not found: ~A~%Store it with:~%  (crichton/credentials:store-credential \"github-api-key\" '(:token \"ghp_...\"))" c))))

(defun github-headers ()
  "Build standard GitHub API request headers."
  (list (cons "Authorization" (format nil "Bearer ~A" (github-token)))
        (cons "Accept" "application/vnd.github+json")
        (cons "X-GitHub-Api-Version" "2022-11-28")
        (cons "User-Agent" (format nil "Crichton/~A" crichton/config:*crichton-version*))))

(defun github-get (path &key query-params)
  "GET PATH from the GitHub API.  Returns parsed JSON.
   QUERY-PARAMS is an alist of (key . value) string pairs."
  (let ((url (format nil "~A~A~A" *github-api-base* path
                     (if query-params
                         (with-output-to-string (s)
                           (write-char #\? s)
                           (loop for ((k . v) . rest) on query-params
                                 do (format s "~A=~A" k v)
                                 when rest do (write-char #\& s)))
                         ""))))
    (log:info "GitHub GET: ~A" path)
    (handler-case
        (multiple-value-bind (body status)
            (dex:get url :headers (github-headers))
          (cond
            ((= status 404) (error "GitHub: not found: ~A" path))
            ((>= status 400) (error "GitHub API error ~D for ~A" status path))
            (t (shasht:read-json body))))
      (dex:http-request-failed (c)
        (error "GitHub request failed: ~A" c)))))

(defun github-post (path body-ht)
  "POST to PATH with BODY-HT as JSON.  Returns parsed JSON."
  (let ((url (format nil "~A~A" *github-api-base* path))
        (json-body (shasht:write-json body-ht nil)))
    (log:info "GitHub POST: ~A" path)
    (handler-case
        (multiple-value-bind (body status)
            (dex:post url :headers (github-headers) :content json-body)
          (unless (member status '(200 201) :test #'=)
            (error "GitHub API error ~D for POST ~A" status path))
          (shasht:read-json body))
      (dex:http-request-failed (c)
        (error "GitHub request failed: ~A" c)))))

;;; --- JSON navigation ---

(defun gh (ht &rest keys)
  "Nested hash-table get for GitHub API responses."
  (let ((v ht))
    (dolist (k keys v)
      (unless (and v (hash-table-p v))
        (return nil))
      (setf v (gethash k v)))))

(defun gh-array (ht &rest keys)
  "Navigate to a value and coerce it to a list."
  (let ((v (apply #'gh ht keys)))
    (when (vectorp v) (coerce v 'list))))

;;; --- Repository operations ---

(defun github-list-repos (&key (type "owner") (sort "updated") (per-page 30))
  "List repositories for the authenticated user.
   TYPE: owner, all, public, private, member.
   Returns a list of repo plists."
  (let ((data (github-get "/user/repos"
                           :query-params `(("type" . ,type)
                                           ("sort" . ,sort)
                                           ("per_page" . ,(write-to-string per-page))))))
    (when (vectorp data)
      (map 'list (lambda (r)
                   (list :name (gethash "name" r)
                         :full-name (gethash "full_name" r)
                         :description (gethash "description" r)
                         :private (gethash "private" r)
                         :stars (gethash "stargazers_count" r)
                         :language (gethash "language" r)
                         :updated (gethash "updated_at" r)
                         :url (gethash "html_url" r)))
           data))))

(defun github-get-repo (owner repo)
  "Get details for OWNER/REPO.  Returns a plist."
  (let ((data (github-get (format nil "/repos/~A/~A" owner repo))))
    (list :name (gethash "name" data)
          :full-name (gethash "full_name" data)
          :description (gethash "description" data)
          :private (gethash "private" data)
          :stars (gethash "stargazers_count" data)
          :forks (gethash "forks_count" data)
          :open-issues (gethash "open_issues_count" data)
          :language (gethash "language" data)
          :default-branch (gethash "default_branch" data)
          :updated (gethash "updated_at" data)
          :url (gethash "html_url" data))))

;;; --- Issues ---

(defun github-list-issues (owner repo &key (state "open") (per-page 30))
  "List issues for OWNER/REPO.  STATE: open, closed, all."
  (let ((data (github-get (format nil "/repos/~A/~A/issues" owner repo)
                           :query-params `(("state" . ,state)
                                           ("per_page" . ,(write-to-string per-page))
                                           ("pulls" . "false")))))
    (when (vectorp data)
      (remove nil
       (map 'list (lambda (i)
                    ;; Skip pull requests (they appear in issues endpoint)
                    (unless (gethash "pull_request" i)
                      (list :number (gethash "number" i)
                            :title (gethash "title" i)
                            :state (gethash "state" i)
                            :author (gh i "user" "login")
                            :created (gethash "created_at" i)
                            :url (gethash "html_url" i))))
                  data)))))

(defun github-create-issue (owner repo title &key body labels)
  "Create an issue in OWNER/REPO with TITLE.  Returns the new issue plist."
  (let ((req (make-hash-table :test #'equal)))
    (setf (gethash "title" req) title)
    (when body   (setf (gethash "body"   req) body))
    (when labels (setf (gethash "labels" req) (coerce labels 'vector)))
    (let ((data (github-post (format nil "/repos/~A/~A/issues" owner repo) req)))
      (list :number (gethash "number" data)
            :title (gethash "title" data)
            :url (gethash "html_url" data)))))

;;; --- Pull requests ---

(defun github-list-prs (owner repo &key (state "open") (per-page 30))
  "List pull requests for OWNER/REPO."
  (let ((data (github-get (format nil "/repos/~A/~A/pulls" owner repo)
                           :query-params `(("state" . ,state)
                                           ("per_page" . ,(write-to-string per-page))))))
    (when (vectorp data)
      (map 'list (lambda (p)
                   (list :number (gethash "number" p)
                         :title (gethash "title" p)
                         :state (gethash "state" p)
                         :author (gh p "user" "login")
                         :base (gh p "base" "ref")
                         :head (gh p "head" "ref")
                         :created (gethash "created_at" p)
                         :url (gethash "html_url" p)))
           data))))

;;; --- CI / Actions ---

(defun github-list-workflow-runs (owner repo &key (per-page 10) branch)
  "List recent workflow runs for OWNER/REPO."
  (let ((params `(("per_page" . ,(write-to-string per-page))
                  ,@(when branch `(("branch" . ,branch))))))
    (let ((data (github-get (format nil "/repos/~A/~A/actions/runs" owner repo)
                             :query-params params)))
      (let ((runs (gh data "workflow_runs")))
        (when (vectorp runs)
          (map 'list (lambda (r)
                       (list :id (gethash "id" r)
                             :name (gethash "name" r)
                             :status (gethash "status" r)
                             :conclusion (gethash "conclusion" r)
                             :branch (gethash "head_branch" r)
                             :commit (gh r "head_commit" "message")
                             :created (gethash "created_at" r)
                             :url (gethash "html_url" r)))
               runs))))))

;;; --- Releases ---

(defun github-list-releases (owner repo &key (per-page 10))
  "List releases for OWNER/REPO."
  (let ((data (github-get (format nil "/repos/~A/~A/releases" owner repo)
                           :query-params `(("per_page" . ,(write-to-string per-page))))))
    (when (vectorp data)
      (map 'list (lambda (r)
                   (list :tag (gethash "tag_name" r)
                         :name (gethash "name" r)
                         :draft (gethash "draft" r)
                         :prerelease (gethash "prerelease" r)
                         :published (gethash "published_at" r)
                         :url (gethash "html_url" r)))
           data))))

;;; --- Code search ---

(defun github-search-code (query &key (per-page 10))
  "Search GitHub code with QUERY (e.g. \"foo repo:owner/repo\").
   Returns a list of match plists."
  (let ((data (github-get "/search/code"
                           :query-params `(("q" . ,query)
                                           ("per_page" . ,(write-to-string per-page))))))
    (let ((items (gh data "items")))
      (when (vectorp items)
        (map 'list (lambda (item)
                     (list :name (gethash "name" item)
                           :path (gethash "path" item)
                           :repo (gh item "repository" "full_name")
                           :url (gethash "html_url" item)))
             items)))))

;;; --- Formatted report ---

(defun github-report (action owner/repo &key state per-page branch query stream)
  "Print a formatted GitHub report for ACTION on OWNER/REPO.
   ACTION: repos, repo, issues, prs, ci, releases, search-code."
  (let ((s (or stream *standard-output*))
        (parts (when owner/repo
                 (cl-ppcre:split "/" owner/repo :limit 2))))
    (let ((owner (first parts))
          (repo  (second parts)))
      (crichton/config:string-case action
        ("repos"
         (let ((repos (github-list-repos :per-page (or per-page 20))))
           (format s "~&Repositories (~D):~%" (length repos))
           (dolist (r repos)
             (format s "  ~A~@[ — ~A~]~%"
                     (getf r :full-name) (getf r :description)))))
        ("repo"
         (let ((r (github-get-repo owner repo)))
           (format s "~&~A~@[ — ~A~]~%  Stars: ~D  Forks: ~D  Issues: ~D~%  Language: ~A  Branch: ~A~%  ~A~%"
                   (getf r :full-name) (getf r :description)
                   (getf r :stars) (getf r :forks) (getf r :open-issues)
                   (or (getf r :language) "?") (getf r :default-branch)
                   (getf r :url))))
        ("issues"
         (let ((issues (github-list-issues owner repo
                                            :state (or state "open")
                                            :per-page (or per-page 20))))
           (format s "~&Issues for ~A/~A (~A, ~D):~%"
                   owner repo (or state "open") (length issues))
           (dolist (i issues)
             (format s "  #~D ~A  [~A]~%"
                     (getf i :number) (getf i :title) (getf i :state)))))
        ("prs"
         (let ((prs (github-list-prs owner repo
                                      :state (or state "open")
                                      :per-page (or per-page 20))))
           (format s "~&Pull Requests for ~A/~A (~A, ~D):~%"
                   owner repo (or state "open") (length prs))
           (dolist (p prs)
             (format s "  #~D ~A  [~A → ~A]~%"
                     (getf p :number) (getf p :title)
                     (getf p :head) (getf p :base)))))
        ("ci"
         (let ((runs (github-list-workflow-runs owner repo
                                                 :per-page (or per-page 10)
                                                 :branch branch)))
           (format s "~&Workflow Runs for ~A/~A (~D):~%"
                   owner repo (length runs))
           (dolist (r runs)
             (format s "  ~A  ~A/~A  [~A]~%"
                     (getf r :name) (getf r :status)
                     (or (getf r :conclusion) "in-progress")
                     (getf r :branch)))))
        ("releases"
         (let ((rels (github-list-releases owner repo :per-page (or per-page 10))))
           (format s "~&Releases for ~A/~A (~D):~%"
                   owner repo (length rels))
           (dolist (r rels)
             (format s "  ~A~@[ — ~A~]~@[ [draft]~]~@[ [pre]~]~%"
                     (getf r :tag) (getf r :name)
                     (getf r :draft) (getf r :prerelease)))))
        ("search-code"
         (let ((results (github-search-code (or query owner/repo)
                                             :per-page (or per-page 10))))
           (format s "~&Code search results for ~S (~D):~%"
                   (or query owner/repo) (length results))
           (dolist (r results)
             (format s "  ~A  ~A~%"
                     (getf r :repo) (getf r :path)))))
        (otherwise
         (format s "Unknown GitHub action: ~A~%" action))))))
