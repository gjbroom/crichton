;;;; agent/tools-data.lisp
;;;;
;;;; Data-management tool definitions: memory, raindrop bookmarks,
;;;; WASM skills/pipelines, org-mode, books, pushover, GitHub, git,
;;;; and HOOBS home automation.

(in-package #:crichton/agent)

;;; --- Memory tools ---

(define-tool memory-write
    (:description "Write a durable fact to long-term memory (MEMORY.org) or today's journal.  Use 'memory' target for important, curated facts that should persist across sessions (user preferences, project context, lessons learned).  Use 'journal' target for session-specific notes and observations that don't rise to long-term memory level.")
  ((target "string"
           "Where to write: 'memory' for MEMORY.org (curated, long-term), 'journal' for today's daily log (raw, session-specific)."
           :enum ("memory" "journal")
           :required-p t)
   (content "string"
            "The content to write. Use org-mode formatting (- for list items, * for headings)."
            :required-p t))
  (cond
    ((string-equal target "memory")
     (let ((path (crichton/state:append-to-memory content)))
       (format nil "Written to long-term memory (~A)." (file-namestring path))))
    ((string-equal target "journal")
     (let ((path (crichton/state:journal-append content)))
       (format nil "Written to journal (~A)." (file-namestring path))))
    (t
     (format nil "Unknown target: ~A" target))))

(define-tool memory-search
    (:description "Search daily journal entries by keyword.  Returns matching excerpts with dates and context.  Use this to recall session-specific notes and observations from recent days.")
  ((query "string"
          "Search term to look for in journal entries. Case-insensitive substring match."
          :required-p t)
   (days-back "integer"
              "Number of days to search back from today. Default: 7."
              :default 7))
  (crichton/state:journal-search query :days-back days-back))

;;; --- Raindrop.io bookmarks tool ---

(define-tool raindrop
    (:description "Manage bookmarks in Raindrop.io.  Actions: 'save' (create a new bookmark), 'search' (find bookmarks by keyword), 'list' (list bookmarks in a collection), 'get' (get a bookmark by ID), 'update' (modify a bookmark), 'delete' (trash a bookmark), 'collections' (list all collections), 'create_collection' (create a new collection), 'tags' (list all tags).  Requires Raindrop.io API token stored as 'raindrop-api' in credentials.")
  ((action "string"
           "The bookmark action to perform."
           :enum ("save" "search" "list" "get" "update" "delete"
                  "collections" "create_collection" "tags")
           :required-p t)
   (url "string"
        "Bookmark URL. Required for 'save'.")
   (title "string"
          "Bookmark title. Optional for 'save' and 'update'.")
   (excerpt "string"
            "Bookmark description/excerpt. Optional for 'save' and 'update'.")
   (tags "string"
         "Comma-separated tags. Optional for 'save' and 'update'.")
   (collection-id "integer"
                   "Collection ID. For 'save'/'update': target collection. For 'list'/'tags': scope. Use 0 for all, -1 for Unsorted.")
   (query "string"
          "Search query text. Required for 'search'.")
   (id "integer"
       "Raindrop (bookmark) ID. Required for 'get', 'update', 'delete'.")
   (important "string"
              "Mark as favorite: 'true' or 'false'. For 'update'.")
   (page "integer"
         "Page number (0-based) for 'list' and 'search'. Default: 0."
         :default 0)
   (per-page "integer"
             "Results per page for 'list' and 'search'. Max 50. Default: 25."
             :default 25))
  (let ((tag-list (when tags
                    (mapcar (lambda (s) (string-trim '(#\Space) s))
                            (cl-ppcre:split "," tags)))))
    (cond
      ((string-equal action "save")
       (unless url
         (return-from handler "Error: 'url' is required for save."))
       (with-output-to-string (s)
         (crichton/skills:raindrop-save-report
          url :title title :excerpt excerpt :tags tag-list
              :collection-id collection-id :stream s)))
      ((string-equal action "search")
       (unless query
         (return-from handler "Error: 'query' is required for search."))
       (with-output-to-string (s)
         (crichton/skills:raindrop-find-report
          query :collection-id (or collection-id 0)
                :max-items per-page :stream s)))
      ((string-equal action "list")
       (with-output-to-string (s)
         (multiple-value-bind (items total)
             (crichton/skills:raindrop-list
              (or collection-id 0) :page page :per-page per-page)
           (crichton/skills:format-raindrop-list
            items :stream s :total total))))
      ((string-equal action "get")
       (unless id
         (return-from handler "Error: 'id' is required for get."))
       (let ((item (crichton/skills:raindrop-get-one id)))
         (if item
             (with-output-to-string (s)
               (crichton/skills:format-raindrop item s))
             (format nil "Bookmark ~D not found." id))))
      ((string-equal action "update")
       (unless id
         (return-from handler "Error: 'id' is required for update."))
       (let ((result (crichton/skills:raindrop-update
                      id :title title :excerpt excerpt
                         :tags tag-list :link url
                         :collection-id collection-id
                         :important important)))
         (with-output-to-string (s)
           (format s "Updated bookmark:~%")
           (crichton/skills:format-raindrop result s))))
      ((string-equal action "delete")
       (unless id
         (return-from handler "Error: 'id' is required for delete."))
       (if (crichton/skills:raindrop-remove id)
           (format nil "Bookmark ~D moved to Trash." id)
           (format nil "Failed to delete bookmark ~D." id)))
      ((string-equal action "collections")
       (with-output-to-string (s)
         (crichton/skills:raindrop-collections-report :stream s)))
      ((string-equal action "create_collection")
       (unless title
         (return-from handler "Error: 'title' is required for create_collection."))
       (let ((result (crichton/skills:raindrop-create-collection
                      title :parent-id collection-id)))
         (format nil "Created collection: ~A (id: ~D)"
                 (getf result :title) (getf result :id))))
      ((string-equal action "tags")
       (with-output-to-string (s)
         (crichton/skills:raindrop-tags-report
          :collection-id collection-id :stream s)))
      (t
       (format nil "Unknown raindrop action: ~A" action)))))

;;; --- Skills tool helpers ---

(defun skills-run-pipeline (steps)
  "Execute a multi-step skill pipeline from the STEPS array.
   Returns a formatted result string."
  (unless steps
    (return-from skills-run-pipeline
      "Error: 'steps' array is required for pipeline action."))
  (handler-case
      (let* ((step-list (coerce steps 'list))
             (results (crichton/skills:execute-pipeline step-list))
             (step-count (hash-table-count results)))
        (with-output-to-string (s)
          (format s "Pipeline completed (~D step~:P):~%" step-count)
          (maphash (lambda (id result)
                     (format s "  ~A: ~S~%" id result))
                   results)))
    (crichton/skills:pipeline-error (c)
      (format nil "Pipeline failed at step '~A': ~A"
              (crichton/skills:pipeline-error-step-id c) c))
    (error (c)
      (format nil "Pipeline error: ~A" c))))

;;; --- Skills tool ---

(define-tool skills
    (:description "Manage external WASM skills and pipelines.  Actions: 'list' (show all discovered skills), 'info' (get details for a specific skill), 'invoke' (run a skill with optional params), 'refresh' (re-scan skills directory), 'pipeline' (run a multi-step pipeline chaining skills together), 'save_pipeline' (save a named pipeline for scheduling), 'delete_pipeline' (remove a saved pipeline), 'list_pipelines' (show saved pipelines).  Saved pipelines are registered as schedulable actions (name: 'pipeline:<name>') and can be scheduled via the scheduler tool.  Use 'params' to pass structured input to skills that expect JSON data (e.g., rss-filter).  Use 'pipeline' to chain multiple steps where later steps reference earlier results via {\"ref\": \"step_id.key\"}.")
  ((action "string"
           "The skills action to perform."
           :enum ("list" "info" "invoke" "refresh" "pipeline"
                  "save_pipeline" "delete_pipeline" "list_pipelines")
           :required-p t)
   (name "string"
         "Skill name. Required for info, invoke, save_pipeline, delete_pipeline.")
   (entry-point "string"
                "Function entry point to call. Optional; defaults to the manifest's declared entry point.")
   (params "object"
           "JSON parameters to pass to the skill. When provided, the JSON ABI is used automatically. Required for pure-function skills like rss-filter.")
   (steps "array"
          "Pipeline steps array (for 'pipeline' and 'save_pipeline' actions). Each step is an object with: id (string, required), kind ('wasm'/'builtin'/'auto'), skill (string, for WASM steps), builtin (string, for builtin steps like 'rss_fetch', 'rss_check', 'weather'), entry_point (string), params (object, may contain {\"ref\": \"step_id.key\"} references to earlier step outputs)."))
  (cond
    ((string-equal action "list")
     (crichton/skills:discover-skills)            ; refresh before listing
     (with-output-to-string (s)
       (crichton/skills:skill-report :stream s)))
    ((string-equal action "info")
     (unless name
       (return-from handler "Error: 'name' is required for info action."))
     (let ((info (crichton/skills:skill-info name)))
       (if info
           (format nil "~S" info)
           (format nil "Skill '~A' not found." name))))
    ((string-equal action "invoke")
     (unless name
       (return-from handler "Error: 'name' is required for invoke action."))
     (handler-case
         (let ((result (crichton/skills:invoke-skill name
                         :entry-point entry-point
                         :params params)))
           (format nil "Skill '~A' returned: ~A" name result))
       (error (c)
         (format nil "Error invoking skill '~A': ~A" name c))))
    ((string-equal action "pipeline")
     (skills-run-pipeline steps))
    ((string-equal action "save_pipeline")
     (unless name
       (return-from handler "Error: 'name' is required for save_pipeline."))
     (unless steps
       (return-from handler "Error: 'steps' is required for save_pipeline."))
     (crichton/skills:save-pipeline name steps)
     (format nil "Pipeline '~A' saved (~D step~:P). Schedulable as action 'pipeline:~A'."
             name (length steps) name))
    ((string-equal action "delete_pipeline")
     (unless name
       (return-from handler "Error: 'name' is required for delete_pipeline."))
     (if (crichton/skills:delete-pipeline name)
         (format nil "Pipeline '~A' deleted." name)
         (format nil "Pipeline '~A' not found." name)))
    ((string-equal action "list_pipelines")
     (let ((pipelines (crichton/skills:list-saved-pipelines)))
       (if pipelines
           (with-output-to-string (s)
             (format s "Saved pipelines:~%")
             (dolist (p pipelines)
               (format s "  ~A (~D step~:P) — schedulable as 'pipeline:~A'~%"
                       (getf p :name) (getf p :step-count) (getf p :name))))
           "No saved pipelines.")))
    ((string-equal action "refresh")
     (let ((count (crichton/skills:discover-skills)))
       (format nil "Discovered ~D skill~:P." count)))
    (t
     (format nil "Unknown skills action: ~A" action))))

;;; --- Org-mode tool ---

(define-tool orgmode
    (:description "Read, search, create, and manage org-mode files and org-roam notes.  LOCAL-ONLY: requires [orgmode] config.  Actions: 'read' (parse an org file), 'read_file' (read any file as raw text), 'search' (find notes by title/tag), 'list_tags' (org-roam tags), 'backlinks' (graph links), 'create_note' (new org-roam note), 'append' (add text to a file), 'list_files' (enumerate org files), 'list_todos' (query TODO items), 'set_todo' (change TODO state), 'set_filetags' (replace file-level tags), 'status' (skill config).")
  ((action "string"
           "The orgmode action to perform."
           :enum ("read" "read_file" "search" "list_tags" "backlinks" "create_note" "append" "list_files" "list_todos" "set_todo" "set_filetags" "status")
           :required-p t)
   (path "string"
         "File path or org-roam node ID (UUID).  Required for read, read_file, backlinks, append, set_todo.")
   (max-chars "integer"
              "Maximum characters to return for read_file.  Default: 50000.")
   (query "string"
          "Search query string.  Required for search.")
   (tag "string"
        "Filter by org-roam tag.  Used with search, list_todos.")
   (title "string"
          "Note title.  Required for create_note.")
   (body "string"
         "Note body content.  Used with create_note.")
   (filetags "array"
             "File-level tags for the new note.  Used with create_note.")
   (root "string"
         "Root directory for create_note or list_files.  Must be in allowed_paths.")
   (text "string"
         "Text to append.  Required for append.")
   (headline "string"
             "Headline title.  Used with append, set_todo.")
   (direction "string"
              "Link direction for backlinks: 'backlinks' (default), 'forward', 'both'."
              :enum ("backlinks" "forward" "both"))
   (include-raw "boolean"
                "Include raw file text in read results.")
   (state "string"
          "TODO state filter for list_todos, or new state for set_todo.")
   (priority "string"
             "Priority filter ('A', 'B', 'C') for list_todos."
             :enum ("A" "B" "C"))
   (include-done "boolean"
                 "Include DONE/CANCELLED items in list_todos.")
   (limit "integer"
          "Maximum results to return."
          :default 50))
  (handler-case
      (cond
        ((string-equal action "status")
         (format nil "~S" (crichton/skills:orgmode-status)))
        ((string-equal action "read")
         (unless path
           (return-from handler "Error: 'path' is required for read."))
         (format nil "~S" (crichton/skills:orgmode-read path :include-raw include-raw)))
        ((string-equal action "read_file")
         (unless path
           (return-from handler "Error: 'path' is required for read_file."))
         (crichton/skills:orgmode-read-file path :max-chars (or max-chars 50000)))
        ((string-equal action "search")
         (unless query
           (return-from handler "Error: 'query' is required for search."))
         (let ((results (crichton/skills:orgmode-search query :tag tag :limit limit)))
           (format nil "~D result~:P:~%~{~S~^~%~}" (length results) results)))
        ((string-equal action "list_tags")
         (let ((tags (crichton/skills:orgmode-list-tags)))
           (format nil "~D tag~:P: ~{~A~^, ~}" (length tags) tags)))
        ((string-equal action "backlinks")
         (unless path
           (return-from handler "Error: 'path' is required for backlinks."))
         (let* ((dir-kw (cond
                          ((or (null direction) (string-equal direction "backlinks")) :backlinks)
                          ((string-equal direction "forward") :forward)
                          ((string-equal direction "both") :both)
                          (t :backlinks)))
                (results (crichton/skills:orgmode-backlinks path :direction dir-kw :limit limit)))
           (format nil "~D link~:P:~%~{~S~^~%~}" (length results) results)))
        ((string-equal action "create_note")
         (unless title
           (return-from handler "Error: 'title' is required for create_note."))
         (let ((ft-list (when filetags (coerce filetags 'list))))
           (let ((created-path (crichton/skills:orgmode-create-note title
                                 :root root :body body :filetags ft-list)))
             (format nil "Created: ~A" created-path))))
        ((string-equal action "append")
         (unless path
           (return-from handler "Error: 'path' is required for append."))
         (unless text
           (return-from handler "Error: 'text' is required for append."))
         (let ((result (crichton/skills:orgmode-append path text :headline headline)))
           (format nil "Appended to: ~A" result)))
        ((string-equal action "list_files")
         (let ((results (crichton/skills:orgmode-list-files :root root :limit limit)))
           (format nil "~D file~:P:~%~{~S~^~%~}" (length results) results)))
        ((string-equal action "list_todos")
         (let ((results (crichton/skills:orgmode-list-todos
                         :state state :priority priority :tag tag
                         :file path :include-done include-done :limit limit)))
           (format nil "~D TODO~:P:~%~{~S~^~%~}" (length results) results)))
        ((string-equal action "set_todo")
         (unless path
           (return-from handler "Error: 'path' is required for set_todo."))
         (unless headline
           (return-from handler "Error: 'headline' is required for set_todo."))
         (let ((result (crichton/skills:orgmode-set-todo path headline state)))
           (format nil "Updated: ~A (headline ~S → ~A)" result headline (or state "cleared"))))
        ((string-equal action "set_filetags")
         (unless path
           (return-from handler "Error: 'path' is required for set_filetags."))
         (let* ((ft-list (if filetags (coerce filetags 'list) nil))
                (result (crichton/skills:orgmode-set-filetags path ft-list)))
           (format nil "Updated filetags: ~A → ~{:~A~}:" result ft-list)))
        (t (format nil "Unknown orgmode action: ~A" action)))
    (error (c)
      (format nil "Orgmode error: ~A" c))))

;;; --- Books tool ---

(define-tool books
    (:description "Query the LibraryThing book database.  LOCAL-ONLY: requires [books] config and a database built by scripts/lt_import.py.  Actions: 'search' (full-text over title/author), 'by_tag' (books with a tag), 'by_author' (books by author), 'by_collection' (books in a LT collection), 'by_series' (books in a named series), 'get' (full detail for one book), 'tags' (all tags with counts), 'collections' (all collections with counts), 'series' (all series with counts), 'status' (skill config).")
  ((action "string"
           "The books action to perform."
           :enum ("search" "by_tag" "by_author" "by_collection" "by_series"
                  "get" "tags" "collections" "series" "status")
           :required-p t)
   (query "string"
          "Full-text search query.  Required for 'search'.")
   (tag "string"
        "Tag name.  Required for 'by_tag'.")
   (author "string"
           "Author name (partial match).  Required for 'by_author'.")
   (collection "string"
               "Collection name.  Required for 'by_collection'.")
   (series "string"
           "Series name (partial match).  Required for 'by_series'.")
   (title "string"
          "Book title or numeric ID.  Required for 'get'.")
   (limit "integer"
          "Maximum results to return."
          :default 50))
  (handler-case
      (cond
        ((string-equal action "status")
         (format nil "~S" (crichton/skills:books-status)))
        ((string-equal action "search")
         (unless query
           (return-from handler "Error: 'query' is required for search."))
         (crichton/skills:books-search query :limit (or limit 20)))
        ((string-equal action "by_tag")
         (unless tag
           (return-from handler "Error: 'tag' is required for by_tag."))
         (crichton/skills:books-by-tag tag :limit (or limit 50)))
        ((string-equal action "by_author")
         (unless author
           (return-from handler "Error: 'author' is required for by_author."))
         (crichton/skills:books-by-author author :limit (or limit 50)))
        ((string-equal action "by_collection")
         (unless collection
           (return-from handler "Error: 'collection' is required for by_collection."))
         (crichton/skills:books-by-collection collection :limit (or limit 100)))
        ((string-equal action "by_series")
         (unless series
           (return-from handler "Error: 'series' is required for by_series."))
         (crichton/skills:books-by-series series :limit (or limit 50)))
        ((string-equal action "get")
         (unless title
           (return-from handler "Error: 'title' is required for get."))
         (crichton/skills:books-get title))
        ((string-equal action "tags")
         (crichton/skills:books-list-tags :limit (or limit 200)))
        ((string-equal action "collections")
         (crichton/skills:books-list-collections))
        ((string-equal action "series")
         (crichton/skills:books-list-series :limit (or limit 200)))
        (t (format nil "Unknown books action: ~A" action)))
    (error (c)
      (format nil "Books error: ~A" c))))

;;; --- Pushover notification tool ---

(define-tool pushover
    (:description "Send a push notification via Pushover.net to iOS, Android, or desktop devices.  Requires 'pushover' credential with :token and :user fields.  Priority: -2 lowest, -1 low, 0 normal (default), 1 high, 2 emergency.")
  ((message "string"
            "The notification message body."
            :required-p t)
   (title "string"
          "Notification title.  Defaults to the app name.")
   (priority "integer"
             "Notification priority: -2 (lowest) to 2 (emergency).  Default: 0 (normal).")
   (url "string"
        "Supplementary URL to attach to the notification.")
   (url-title "string"
              "Title for the supplementary URL.")
   (device "string"
           "Target device name.  Omit to send to all devices.")
   (sound "string"
          "Notification sound name (e.g. 'pushover', 'magic', 'alien')."))
  (crichton/skills:pushover-report message
                                   :title title
                                   :priority priority
                                   :url url
                                   :url-title url-title
                                   :device device
                                   :sound sound))

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

;;; --- HOOBS home automation tool ---

(define-tool hoobs
    (:description "Control HomeKit accessories via a HOOBS hub.  Actions: 'status' (hub status), 'rooms' (list rooms), 'accessories' (list all accessories by room), 'get_accessory' (details for one accessory), 'set_accessory' (set characteristic values — requires [hoobs] allow_control = true in config).  Requires 'hoobs' credential with :host, :username, :password and optionally :port.")
  ((action "string"
           "The HOOBS action to perform."
           :enum ("status" "rooms" "accessories" "get_accessory" "set_accessory")
           :required-p t)
   (accessory-id "string"
                 "Accessory identifier.  Required for get_accessory and set_accessory.")
   (characteristics "object"
                    "Hash of characteristic-name → value to set.  Required for set_accessory (e.g., {\"on\": true})."))
  (cond
    ((string-equal action "status")
     (with-output-to-string (s)
       (crichton/skills:hoobs-report :stream s)))
    ((string-equal action "rooms")
     (let ((rooms (crichton/skills:hoobs-rooms)))
       (if rooms
           (format nil "~{~A (id: ~A)~^~%~}"
                   (loop for r in rooms
                         collect (getf r :name)
                         collect (getf r :id)))
           "No rooms found.")))
    ((string-equal action "accessories")
     (with-output-to-string (s)
       (crichton/skills:hoobs-report :stream s)))
    ((string-equal action "get_accessory")
     (unless accessory-id
       (return-from handler "Error: 'accessory_id' is required for get_accessory."))
     (let ((data (crichton/skills:hoobs-get-accessory accessory-id)))
       (if data
           (format nil "~S" data)
           (format nil "Accessory '~A' not found." accessory-id))))
    ((string-equal action "set_accessory")
     (unless accessory-id
       (return-from handler "Error: 'accessory_id' is required for set_accessory."))
     (unless characteristics
       (return-from handler "Error: 'characteristics' is required for set_accessory."))
     (handler-case
         (progn
           (crichton/skills:hoobs-set-accessory accessory-id characteristics)
           (format nil "Accessory '~A' updated." accessory-id))
       (error (c)
         (format nil "Error: ~A" c))))
    (t
     (format nil "Unknown HOOBS action: ~A" action))))
