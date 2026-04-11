;;;; skills/builtins/books.lisp
;;;;
;;;; Built-in skill: LibraryThing book database queries.
;;;; Reads from a SQLite database populated by scripts/lt_import.py.
;;;;
;;;; Gated by [books] config section.  Database path defaults to
;;;; ~/.crichton/data/books.db.

(in-package #:crichton/skills)

;;; ====================================================================
;;; Config gate
;;; ====================================================================

(defun books-enabled-p ()
  "Return T if the books skill is enabled in config."
  (let ((val (crichton/config:config-section-get :books :enable)))
    (cond
      ((eq val t) t)
      ((and (stringp val) (string-equal val "true")) t)
      (t nil))))

(defun books-db-path ()
  "Return the books SQLite database path from config, or the default."
  (or (crichton/config:config-section-get :books :db-path)
      (namestring (merge-pathnames ".crichton/data/books.db"
                                   (user-homedir-pathname)))))

(defun books-ensure-enabled ()
  "Signal an error unless the books skill is enabled."
  (unless (books-enabled-p)
    (error "Books skill is disabled (set [books] enable = true in config.toml)")))

;;; ====================================================================
;;; Database access
;;; ====================================================================

(defmacro with-books-db ((db) &body body)
  "Open the books database, bind it to DB, and execute BODY.
Signals an error if the skill is disabled or the database file is absent."
  `(progn
     (books-ensure-enabled)
     (let ((path (books-db-path)))
       (unless (probe-file path)
         (error "Books database not found: ~A — run scripts/lt_import.py first" path))
       (sqlite:with-open-database (,db path)
         ,@body))))

;;; ====================================================================
;;; Output formatting
;;; ====================================================================

(defun book-summary-line (row)
  "One-line summary from a query row: (id title author year rating ...)."
  (destructuring-bind (id title author year rating &rest _rest) row
    (declare (ignore id _rest))
    (with-output-to-string (s)
      (format s "~A" title)
      (when author (format s " / ~A" author))
      (when year   (format s " (~A)" year))
      (when rating (format s " [~A★]" rating)))))

(defun format-book-detail (db row)
  "Full detail block for a single book.
ROW is (id title author year rating pages format language isbn publication)."
  (destructuring-bind (id title author year rating pages fmt lang isbn pub) row
    (with-output-to-string (s)
      (format s "Title:   ~A~%" title)
      (when author (format s "Author:  ~A~%" author))
      (when year   (format s "Year:    ~A~%" year))
      (when isbn   (format s "ISBN:    ~A~%" isbn))
      (when pub    (format s "Pub:     ~A~%" pub))
      (when pages  (format s "Pages:   ~A~%" pages))
      (when fmt    (format s "Format:  ~A~%" fmt))
      (when lang   (format s "Lang:    ~A~%" lang))
      (when rating (format s "Rating:  ~A/5~%" rating))
      (let ((tags (sqlite:execute-to-list db
                    "SELECT t.name FROM tags t
                     JOIN book_tags bt ON bt.tag_id = t.id
                     WHERE bt.book_id = ?
                     ORDER BY t.name"
                    id))
            (series (sqlite:execute-to-list db
                      "SELECT s.name FROM series s
                       JOIN book_series bs ON bs.series_id = s.id
                       WHERE bs.book_id = ?
                       ORDER BY s.name"
                      id)))
        (when tags
          (format s "Tags:    ~{~A~^, ~}~%" (mapcar #'car tags)))
        (when series
          (format s "Series:  ~{~A~^, ~}~%" (mapcar #'car series)))))))

;;; ====================================================================
;;; Query functions
;;; ====================================================================

(defun books-search (query &key (limit 20))
  "Full-text search over title and primary author.
Returns a formatted result string."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                          b.pages, b.format, b.language
                   FROM books b
                   JOIN books_fts f ON f.rowid = b.id
                   WHERE books_fts MATCH ?
                   ORDER BY rank
                   LIMIT ?"
                  query limit)))
      (if rows
          (format nil "~D result~:P for ~S:~%~{  ~A~^~%~}"
                  (length rows) query
                  (mapcar #'book-summary-line rows))
          (format nil "No results for ~S." query)))))

(defun books-by-tag (tag &key (limit 50))
  "List books with the given TAG, sorted alphabetically by title."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                          b.pages, b.format, b.language
                   FROM books b
                   JOIN book_tags bt ON bt.book_id = b.id
                   JOIN tags t ON t.id = bt.tag_id
                   WHERE t.name = ?
                   ORDER BY b.sort_title
                   LIMIT ?"
                  tag limit)))
      (if rows
          (format nil "~D book~:P tagged ~S:~%~{  ~A~^~%~}"
                  (length rows) tag
                  (mapcar #'book-summary-line rows))
          (format nil "No books tagged ~S." tag)))))

(defun books-by-author (author &key (limit 50))
  "List books where primary_author contains AUTHOR (case-insensitive partial match)."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                          b.pages, b.format, b.language
                   FROM books b
                   WHERE b.primary_author LIKE ?
                   ORDER BY b.sort_title
                   LIMIT ?"
                  (format nil "%~A%" author) limit)))
      (if rows
          (format nil "~D book~:P matching author ~S:~%~{  ~A~^~%~}"
                  (length rows) author
                  (mapcar #'book-summary-line rows))
          (format nil "No books matching author ~S." author)))))

(defun books-by-collection (collection &key (limit 100))
  "List books in COLLECTION, sorted alphabetically by title."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                          b.pages, b.format, b.language
                   FROM books b
                   JOIN book_collections bc ON bc.book_id = b.id
                   JOIN collections c ON c.id = bc.collection_id
                   WHERE c.name = ?
                   ORDER BY b.sort_title
                   LIMIT ?"
                  collection limit)))
      (if rows
          (format nil "~D book~:P in collection ~S:~%~{  ~A~^~%~}"
                  (length rows) collection
                  (mapcar #'book-summary-line rows))
          (format nil "No books in collection ~S." collection)))))

(defun books-by-series (series-name &key (limit 50))
  "List books in a series whose name contains SERIES-NAME."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                          b.pages, b.format, b.language
                   FROM books b
                   JOIN book_series bs ON bs.book_id = b.id
                   JOIN series s ON s.id = bs.series_id
                   WHERE s.name LIKE ?
                   ORDER BY b.sort_title
                   LIMIT ?"
                  (format nil "%~A%" series-name) limit)))
      (if rows
          (format nil "~D book~:P in series matching ~S:~%~{  ~A~^~%~}"
                  (length rows) series-name
                  (mapcar #'book-summary-line rows))
          (format nil "No books in series matching ~S." series-name)))))

(defun books-get (title-or-id)
  "Get full detail for a book by title (partial match) or numeric ID string."
  (with-books-db (db)
    (let* ((by-id-p (and (plusp (length title-or-id))
                         (every #'digit-char-p title-or-id)))
           (row (if by-id-p
                    (car (sqlite:execute-to-list db
                           "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                                   b.pages, b.format, b.language, b.isbn, b.publication
                            FROM books b WHERE b.id = ?"
                           (parse-integer title-or-id)))
                    (car (sqlite:execute-to-list db
                           "SELECT b.id, b.title, b.primary_author, b.year, b.rating,
                                   b.pages, b.format, b.language, b.isbn, b.publication
                            FROM books b
                            WHERE b.title LIKE ?
                            ORDER BY b.sort_title
                            LIMIT 1"
                           (format nil "%~A%" title-or-id))))))
      (if row
          (format-book-detail db row)
          (format nil "No book found matching ~S." title-or-id)))))

(defun books-list-tags (&key (limit 200))
  "Return all tags with book counts, sorted by frequency descending."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT t.name, COUNT(*) AS n
                   FROM tags t JOIN book_tags bt ON bt.tag_id = t.id
                   GROUP BY t.id ORDER BY n DESC, t.name
                   LIMIT ?"
                  limit)))
      (if rows
          (format nil "~D tag~:P:~%~{  ~A (~A)~^~%~}"
                  (length rows)
                  (loop for (name count) in rows nconc (list name count)))
          "No tags found."))))

(defun books-list-collections ()
  "Return all collections with book counts."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT c.name, COUNT(*) AS n
                   FROM collections c
                   JOIN book_collections bc ON bc.collection_id = c.id
                   GROUP BY c.id ORDER BY c.name")))
      (if rows
          (format nil "~D collection~:P:~%~{  ~A (~A)~^~%~}"
                  (length rows)
                  (loop for (name count) in rows nconc (list name count)))
          "No collections found."))))

(defun books-list-series (&key (limit 200))
  "Return all series with book counts, sorted alphabetically."
  (with-books-db (db)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT s.name, COUNT(*) AS n
                   FROM series s JOIN book_series bs ON bs.series_id = s.id
                   GROUP BY s.id ORDER BY s.name
                   LIMIT ?"
                  limit)))
      (if rows
          (format nil "~D series:~%~{  ~A (~A)~^~%~}"
                  (length rows)
                  (loop for (name count) in rows nconc (list name count)))
          "No series found."))))

(defun books-status ()
  "Return a status plist for the books skill."
  (let ((path (books-db-path))
        (enabled (books-enabled-p)))
    (if (and enabled (probe-file path))
        (sqlite:with-open-database (db path)
          (list :enabled t
                :db-path  path
                :books    (sqlite:execute-single db "SELECT COUNT(*) FROM books")
                :authors  (sqlite:execute-single db "SELECT COUNT(*) FROM authors")
                :tags     (sqlite:execute-single db "SELECT COUNT(*) FROM tags")
                :series   (sqlite:execute-single db "SELECT COUNT(*) FROM series")))
        (list :enabled enabled :db-path path :books nil))))
