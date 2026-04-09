;;;; skills/builtins/raindrop.lisp
;;;;
;;;; Built-in skill: Raindrop.io bookmark management.
;;;; CRUD operations on bookmarks (raindrops), collections, and tags
;;;; via the Raindrop.io REST API (https://developer.raindrop.io).
;;;;
;;;; Authentication uses an OAuth2 test token stored in the credential
;;;; store as "raindrop-api" with field :token.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Configuration ---

(defvar *raindrop-api-base* "https://api.raindrop.io/rest/v1"
  "Base URL for the Raindrop.io REST API v1.")

(defvar *raindrop-credential-name* "raindrop-api"
  "Credential store name for the Raindrop.io API token.")

(defvar *raindrop-user-agent* "Crichton/0.3"
  "User-Agent header sent with Raindrop.io API requests.")

;;; --- Authentication ---

(defun raindrop-token ()
  "Resolve the Raindrop.io API token from the credential store.
   Offers a :USE-VALUE restart for interactive supply."
  (restart-case
      (handler-case
          (crichton/credentials:resolve-credential *raindrop-credential-name* :token)
        (error (c)
          (error "Cannot resolve Raindrop.io API token (~A): ~A"
                 *raindrop-credential-name* c)))
    (:use-value (token)
      :report (lambda (s) (format s "Supply Raindrop.io API token"))
      :interactive (lambda ()
                     (format *query-io* "~&Enter Raindrop.io token: ")
                     (list (read-line *query-io*)))
      token)))

(defun raindrop-headers (token)
  "Build HTTP headers for authenticated Raindrop.io API requests."
  (list (cons "Authorization" (format nil "Bearer ~A" token))
        (cons "Content-Type" "application/json")
        (cons "User-Agent" *raindrop-user-agent*)))

;;; --- HTTP helpers ---

(defun raindrop-get (path &key query-params)
  "Make a GET request to the Raindrop.io API.
   PATH is appended to the base URL (e.g. \"/collections\").
   QUERY-PARAMS is an alist of (key . value) pairs appended as query string.
   Returns the parsed JSON response as a hash-table.
   Retries transient failures automatically."
  (let* ((token (raindrop-token))
         (url (build-raindrop-url path query-params)))
    (log:info "Raindrop GET: ~A" url)
    (multiple-value-bind (body status)
        (http-get-with-retry url :headers (raindrop-headers token))
      (unless (= status 200)
        (error "Raindrop.io API returned HTTP ~D for GET ~A" status path))
      (shasht:read-json body))))

(defun raindrop-post (path body-ht)
  "Make a POST request to the Raindrop.io API.
   BODY-HT is a hash-table serialized as JSON in the request body.
   Returns the parsed JSON response."
  (let* ((token (raindrop-token))
         (url (format nil "~A~A" *raindrop-api-base* path))
         (json-body (shasht:write-json body-ht nil)))
    (log:info "Raindrop POST: ~A" url)
    (multiple-value-bind (body status)
        (http-post-with-retry url
          :headers (raindrop-headers token)
          :content json-body)
      (unless (= status 200)
        (error "Raindrop.io API returned HTTP ~D for POST ~A" status path))
      (shasht:read-json body))))

(defun raindrop-put (path body-ht)
  "Make a PUT request to the Raindrop.io API.
   BODY-HT is a hash-table serialized as JSON in the request body.
   Returns the parsed JSON response."
  (let* ((token (raindrop-token))
         (url (format nil "~A~A" *raindrop-api-base* path))
         (json-body (shasht:write-json body-ht nil)))
    (log:info "Raindrop PUT: ~A" url)
    (multiple-value-bind (body status)
        (http-put-with-retry url
          :headers (raindrop-headers token)
          :content json-body)
      (unless (= status 200)
        (error "Raindrop.io API returned HTTP ~D for PUT ~A" status path))
      (shasht:read-json body))))

(defun raindrop-delete (path &key body-ht)
  "Make a DELETE request to the Raindrop.io API.
   Optional BODY-HT for requests that need a JSON body (e.g. tag removal).
   Returns the parsed JSON response."
  (let* ((token (raindrop-token))
         (url (format nil "~A~A" *raindrop-api-base* path))
         (json-body (when body-ht (shasht:write-json body-ht nil))))
    (log:info "Raindrop DELETE: ~A" url)
    (multiple-value-bind (body status)
        (http-delete-with-retry url
          :headers (raindrop-headers token)
          :content json-body)
      (unless (or (= status 200) (= status 204))
        (error "Raindrop.io API returned HTTP ~D for DELETE ~A" status path))
      (when (and body (plusp (length body)))
        (shasht:read-json body)))))

(defun %url-encode-param (string)
  "Percent-encode a query parameter value (RFC 3986).
   Encodes everything except unreserved characters: A-Z a-z 0-9 - _ . ~"
  (with-output-to-string (out)
    (loop for ch across string
          do (if (or (alphanumericp ch)
                     (member ch '(#\- #\_ #\. #\~)))
                 (write-char ch out)
                 (format out "%~2,'0X" (char-code ch))))))

(defun build-raindrop-url (path query-params)
  "Build a full Raindrop.io API URL with optional query parameters."
  (let ((base (format nil "~A~A" *raindrop-api-base* path)))
    (if (null query-params)
        base
        (let ((parts (loop for (k . v) in query-params
                           when v
                           collect (format nil "~A=~A"
                                           (princ-to-string k)
                                           (%url-encode-param
                                            (princ-to-string v))))))
          (if parts
              (format nil "~A?~{~A~^&~}" base parts)
              base)))))

;;; --- JSON helpers ---

(defun jht (&rest pairs)
  "Build a hash-table from key-value pairs for JSON serialization.
   Keys should be strings."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun jref (ht &rest keys)
  "Navigate nested hash-tables by successive keys."
  (reduce (lambda (h k)
            (when (and h (hash-table-p h))
              (gethash k h)))
          keys :initial-value ht))

;;; --- Raindrop (bookmark) operations ---

(defun raindrop-create (url &key title excerpt tags collection-id)
  "Create a new bookmark (raindrop) in Raindrop.io.
   URL is required. Other fields are optional.
   Returns the created raindrop as a plist."
  (let ((body (jht "link" url
                    "pleaseParse" (jht))))
    (when title (setf (gethash "title" body) title))
    (when excerpt (setf (gethash "excerpt" body) excerpt))
    (when tags (setf (gethash "tags" body) (coerce tags 'vector)))
    (when collection-id
      (setf (gethash "collection" body) (jht "$id" collection-id)))
    (let ((resp (raindrop-post "/raindrop" body)))
      (raindrop-to-plist (gethash "item" resp)))))

(defun raindrop-get-one (id)
  "Get a single raindrop by ID. Returns a plist."
  (let ((resp (raindrop-get (format nil "/raindrop/~D" id))))
    (raindrop-to-plist (gethash "item" resp))))

(defun raindrop-update (id &key title excerpt tags link collection-id important)
  "Update an existing raindrop. Only non-NIL fields are updated.
   Returns the updated raindrop as a plist."
  (let ((body (make-hash-table :test #'equal)))
    (when title (setf (gethash "title" body) title))
    (when excerpt (setf (gethash "excerpt" body) excerpt))
    (when tags (setf (gethash "tags" body) (coerce tags 'vector)))
    (when link (setf (gethash "link" body) link))
    (when collection-id
      (setf (gethash "collection" body) (jht "$id" collection-id)))
    (when important
      (setf (gethash "important" body)
            (cond ((string-equal important "true") t)
                  ((string-equal important "false") nil)
                  (t important))))
    (let ((resp (raindrop-put (format nil "/raindrop/~D" id) body)))
      (raindrop-to-plist (gethash "item" resp)))))

(defun raindrop-remove (id)
  "Move a raindrop to Trash. Returns T on success."
  (let ((resp (raindrop-delete (format nil "/raindrop/~D" id))))
    (and resp (gethash "result" resp))))

(defun raindrop-list (collection-id &key search (page 0) (per-page 25) sort)
  "List raindrops from a collection. Use collection-id 0 for all.
   Returns (values items-as-plists total-count)."
  (let ((params (list (cons "page" page)
                      (cons "perpage" per-page))))
    (when search (push (cons "search" search) params))
    (when sort (push (cons "sort" sort) params))
    (let ((resp (raindrop-get (format nil "/raindrops/~D" collection-id)
                              :query-params params)))
      (values
       (mapcar #'raindrop-to-plist
               (coerce (gethash "items" resp #()) 'list))
       (gethash "count" resp 0)))))

(defun raindrop-search (query &key (collection-id 0) (page 0) (per-page 25))
  "Search bookmarks across all collections (or a specific one).
   Returns (values items-as-plists total-count)."
  (raindrop-list collection-id :search query :page page :per-page per-page
                                :sort "score"))

;;; --- Collection operations ---

(defun raindrop-collections ()
  "Get all root collections. Returns a list of plists."
  (let ((resp (raindrop-get "/collections")))
    (mapcar #'collection-to-plist
            (coerce (gethash "items" resp #()) 'list))))

(defun raindrop-child-collections ()
  "Get all child (nested) collections. Returns a list of plists."
  (let ((resp (raindrop-get "/collections/childrens")))
    (mapcar #'collection-to-plist
            (coerce (gethash "items" resp #()) 'list))))

(defun raindrop-all-collections ()
  "Get all collections (root + children). Returns a list of plists."
  (append (raindrop-collections) (raindrop-child-collections)))

(defun raindrop-create-collection (title &key parent-id public-p)
  "Create a new collection. Returns the created collection as a plist."
  (let ((body (jht "title" title)))
    (when parent-id
      (setf (gethash "parent" body) (jht "$id" parent-id)))
    (when public-p
      (setf (gethash "public" body) t))
    (let ((resp (raindrop-post "/collection" body)))
      (collection-to-plist (gethash "item" resp)))))

;;; --- Tag operations ---

(defun raindrop-tags (&optional collection-id)
  "List all tags, optionally scoped to a collection. Returns a list of plists."
  (let* ((path (if collection-id
                   (format nil "/tags/~D" collection-id)
                   "/tags/0"))
         (resp (raindrop-get path)))
    (mapcar (lambda (item)
              (list :tag (gethash "_id" item)
                    :count (gethash "count" item 0)))
            (coerce (gethash "items" resp #()) 'list))))

;;; --- Conversion helpers ---

(defun raindrop-to-plist (item)
  "Convert a Raindrop.io raindrop JSON hash-table to a plist."
  (when (and item (hash-table-p item))
    (list :id (gethash "_id" item)
          :title (gethash "title" item "")
          :link (gethash "link" item "")
          :excerpt (gethash "excerpt" item "")
          :note (gethash "note" item "")
          :type (gethash "type" item "link")
          :tags (coerce (gethash "tags" item #()) 'list)
          :domain (gethash "domain" item "")
          :created (gethash "created" item "")
          :last-update (gethash "lastUpdate" item "")
          :collection-id (let ((coll (gethash "collection" item)))
                           (when (hash-table-p coll)
                             (gethash "$id" coll)))
          :important (gethash "important" item nil))))

(defun collection-to-plist (item)
  "Convert a Raindrop.io collection JSON hash-table to a plist."
  (when (and item (hash-table-p item))
    (list :id (gethash "_id" item)
          :title (gethash "title" item "")
          :count (gethash "count" item 0)
          :public (gethash "public" item nil)
          :parent-id (let ((parent (gethash "parent" item)))
                       (when (hash-table-p parent)
                         (gethash "$id" parent)))
          :created (gethash "created" item "")
          :last-update (gethash "lastUpdate" item ""))))

;;; --- Formatted output ---

(defun format-raindrop (plist &optional (stream *standard-output*))
  "Format a single raindrop plist for display."
  (format stream "  ~A~%" (getf plist :title))
  (format stream "    ~A~%" (getf plist :link))
  (when (getf plist :tags)
    (format stream "    Tags: ~{~A~^, ~}~%" (getf plist :tags)))
  (when (plusp (length (getf plist :excerpt)))
    (format stream "    ~A~%"
            (let ((exc (getf plist :excerpt)))
              (if (> (length exc) 120)
                  (format nil "~A..." (subseq exc 0 117))
                  exc)))))

(defun format-raindrop-list (items &key (stream *standard-output*) total)
  "Format a list of raindrop plists for display."
  (format stream "~D bookmark~:P~@[ (of ~D total)~]~%"
          (length items) (when (and total (/= total (length items))) total))
  (dolist (item items)
    (format-raindrop item stream)))

(defun format-collection (plist &optional (stream *standard-output*))
  "Format a single collection plist for display."
  (format stream "  ~A (id: ~D, ~D item~:P)~%"
          (getf plist :title) (getf plist :id) (getf plist :count)))

;;; --- Public interface (programmatic) ---

(defun raindrop-save (url &key title excerpt tags collection-id)
  "Save a bookmark to Raindrop.io. Returns the created raindrop plist."
  (raindrop-create url :title title :excerpt excerpt
                       :tags tags :collection-id collection-id))

(defun raindrop-find (query &key (collection-id 0) (max-items 25))
  "Search bookmarks. Returns a plist with :query, :count, :items."
  (multiple-value-bind (items total)
      (raindrop-search query :collection-id collection-id :per-page max-items)
    (list :query query
          :count (length items)
          :total total
          :items items)))

(defun raindrop-list-collections ()
  "List all collections. Returns a plist with :count, :collections."
  (let ((colls (raindrop-all-collections)))
    (list :count (length colls)
          :collections colls)))

(defun raindrop-list-tags (&optional collection-id)
  "List all tags. Returns a plist with :count, :tags."
  (let ((tags (raindrop-tags collection-id)))
    (list :count (length tags)
          :tags tags)))

;;; --- Public interface (formatted) ---

(defun raindrop-save-report (url &key title excerpt tags collection-id
                                      (stream *standard-output*))
  "Save a bookmark and display the result."
  (let ((result (raindrop-save url :title title :excerpt excerpt
                                   :tags tags :collection-id collection-id)))
    (format stream "~&Saved bookmark:~%")
    (format-raindrop result stream)
    result))

(defun raindrop-find-report (query &key (collection-id 0) (max-items 25)
                                        (stream *standard-output*))
  "Search bookmarks and display results."
  (let ((result (raindrop-find query :collection-id collection-id
                                     :max-items max-items)))
    (format stream "~&Search: ~A~%" query)
    (format-raindrop-list (getf result :items) :stream stream
                                               :total (getf result :total))
    result))

(defun raindrop-collections-report (&key (stream *standard-output*))
  "List collections and display them."
  (let ((result (raindrop-list-collections)))
    (format stream "~&~D collection~:P:~%"
            (getf result :count))
    (dolist (coll (getf result :collections))
      (format-collection coll stream))
    result))

(defun raindrop-tags-report (&key collection-id (stream *standard-output*))
  "List tags and display them."
  (let ((result (raindrop-list-tags collection-id)))
    (format stream "~&~D tag~:P:~%" (getf result :count))
    (dolist (tag (getf result :tags))
      (format stream "  ~A (~D)~%" (getf tag :tag) (getf tag :count)))
    result))
