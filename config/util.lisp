;;;; config/util.lisp
;;;;
;;;; Shared utility functions available to all Crichton packages.
;;;; Loaded early (after config/loader) so every module can use these.

(in-package #:crichton/config)

;;; --- Timestamp ---

(defun iso8601-now ()
  "Return current UTC time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

;;; --- Safe keyword interning ---

(defun safe-intern-keyword (key)
  "Intern KEY as a keyword after validating length and character set.
   Rejects keys that could exhaust the keyword package via untrusted JSON."
  (let ((ukey (string-upcase key)))
    (when (or (> (length ukey) 64)
              (zerop (length ukey))
              (not (every (lambda (c)
                            (or (alpha-char-p c) (digit-char-p c)
                                (char= c #\-) (char= c #\_)))
                          ukey)))
      (error "Invalid JSON key for keyword interning: ~S" key))
    (intern ukey :keyword)))

;;; --- Plist/JSON codecs ---

(defun plist-to-json-bytes (plist)
  "Serialize a plist to JSON bytes.  Keys are converted from keywords to
   lowercase strings."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht) v))
    (sb-ext:string-to-octets
     (let ((*print-pretty* nil))
       (with-output-to-string (s)
         (shasht:write-json ht s)))
     :external-format :utf-8)))

(defun json-bytes-to-plist (bytes)
  "Deserialize JSON bytes to a plist.  String keys become uppercase keywords
   after validation."
  (let* ((json-string (sb-ext:octets-to-string bytes :external-format :utf-8))
         (ht (shasht:read-json json-string)))
    (when (hash-table-p ht)
      (let (result)
        (maphash (lambda (k v)
                   (push v result)
                   (push (safe-intern-keyword k) result))
                 ht)
        result))))

;;; --- File utilities ---

(defun delete-file-if-exists (path)
  "Delete PATH if it exists.  Returns T if deleted, NIL if not found."
  (when (probe-file path)
    (delete-file path)
    t))

;;; --- String dispatch ---

(defmacro string-case (keyform &body clauses)
  "Like CASE but dispatches on STRING-EQUAL.  Each clause is
   (string-or-strings &body body).  Use OTHERWISE or T for the default clause."
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond
         ,@(loop for (match . body) in clauses
                 collect (cond
                           ((member match '(otherwise t))
                            `(t ,@body))
                           ((stringp match)
                            `((string-equal ,key ,match) ,@body))
                           ((and (listp match) (every #'stringp match))
                            `((or ,@(mapcar (lambda (m) `(string-equal ,key ,m)) match))
                              ,@body))
                           (t (error "Invalid string-case match: ~S" match))))))))
