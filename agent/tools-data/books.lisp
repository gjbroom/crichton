;;;; agent/tools-data/books.lisp
;;;;
;;;; Tool definition for LibraryThing book database queries.

(in-package #:crichton/agent)

;;; --- Books tool ---

(define-tool books
    (:description "Query the LibraryThing book database.  LOCAL-ONLY: requires [books] config and a database built by scripts/lt_import.py.  Actions: 'search' (full-text over title/author), 'by_tag' (books with a single tag), 'by_tags' (books matching multiple tags, AND/OR), 'by_author' (books by author), 'by_collection' (books in a LT collection), 'by_series' (books in a named series), 'get' (full detail for one book), 'tags' (all tags with counts), 'collections' (all collections with counts), 'series' (all series with counts), 'status' (skill config).")
  ((action "string"
           "The books action to perform."
           :enum ("search" "by_tag" "by_tags" "by_author" "by_collection" "by_series"
                  "get" "tags" "collections" "series" "status")
           :required-p t)
   (query "string"
          "Full-text search query.  Required for 'search'.")
   (tag "string"
        "Tag name.  Required for 'by_tag'.")
   (tags "string"
         "Comma-separated tag names.  Required for 'by_tags'.  Example: \"fiction,sci-fi\".")
   (tags-mode "string"
              "Combination mode for 'by_tags': 'and' (default, book must have all tags) or 'or' (book needs any tag)."
              :enum ("and" "or")
              :default "and")
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
         (format nil "~S" (books-status)))
        ((string-equal action "search")
         (unless query
           (return-from handler "Error: 'query' is required for search."))
         (books-search query :limit (or limit 20)))
        ((string-equal action "by_tag")
         (unless tag
           (return-from handler "Error: 'tag' is required for by_tag."))
         (books-by-tag tag :limit (or limit 50)))
        ((string-equal action "by_tags")
         (unless tags
           (return-from handler "Error: 'tags' is required for by_tags."))
         (let ((tag-list (loop for start = 0 then (1+ end)
                               for end   = (position #\, tags :start start)
                               for part  = (string-trim " " (subseq tags start (or end (length tags))))
                               when (plusp (length part)) collect part
                               while end))
               (mode-kw  (if (string-equal tags-mode "or") :or :and)))
           (if (null tag-list)
               "Error: 'tags' must contain at least one non-empty tag."
               (books-by-tags tag-list :mode mode-kw :limit (or limit 50)))))
        ((string-equal action "by_author")
         (unless author
           (return-from handler "Error: 'author' is required for by_author."))
         (books-by-author author :limit (or limit 50)))
        ((string-equal action "by_collection")
         (unless collection
           (return-from handler "Error: 'collection' is required for by_collection."))
         (books-by-collection collection :limit (or limit 100)))
        ((string-equal action "by_series")
         (unless series
           (return-from handler "Error: 'series' is required for by_series."))
         (books-by-series series :limit (or limit 50)))
        ((string-equal action "get")
         (unless title
           (return-from handler "Error: 'title' is required for get."))
         (books-get title))
        ((string-equal action "tags")
         (books-list-tags :limit (or limit 200)))
        ((string-equal action "collections")
         (books-list-collections))
        ((string-equal action "series")
         (books-list-series :limit (or limit 200)))
        (t (format nil "Unknown books action: ~A" action)))
    (error (c)
      (format nil "Books error: ~A" c))))
