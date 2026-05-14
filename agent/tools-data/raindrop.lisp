;;;; agent/tools-data/raindrop.lisp
;;;;
;;;; Tool definition for Raindrop.io bookmark management.

(in-package #:crichton/agent)

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
