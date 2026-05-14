;;;; agent/tools-data/orgmode.lisp
;;;;
;;;; Tool definition for org-mode and org-roam file management.

(in-package #:crichton/agent)

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
