;;;; llm/anthropic.lisp
;;;;
;;;; Anthropic Claude provider implementation.
;;;; Uses the Messages API (v1/messages) via dexador + shasht.

(in-package #:crichton/llm)

(defparameter *anthropic-api-base* "https://api.anthropic.com"
  "Base URL for the Anthropic API.")

(defparameter *anthropic-api-version* "2023-06-01"
  "Anthropic API version header value.")

(defparameter *anthropic-default-max-tokens* 4096
  "Default max_tokens for Anthropic requests.")

;;; --- Provider class ---

(defclass anthropic-provider (llm-provider)
  ((api-key :initarg :api-key :reader anthropic-api-key
            :type string)
   (api-base :initarg :api-base :reader anthropic-api-base
             :initform *anthropic-api-base*
             :type string)))

(defun make-anthropic-provider (&key api-key (model nil) (api-base nil))
  "Create an Anthropic provider. MODEL defaults to config or claude-sonnet-4-20250514."
  (make-instance 'anthropic-provider
    :provider-id :anthropic
    :model (or model
               (crichton/config:config-section-get :llm :model)
               "claude-sonnet-4-20250514")
    :api-key api-key
    :api-base (or api-base *anthropic-api-base*)))

;;; --- Request building ---

(defun content-block-to-anthropic (block)
  "Convert a single content block plist to an Anthropic API hash-table."
  (let ((ht (make-hash-table :test #'equal))
        (type (getf block :type)))
    (ecase type
      (:text
       (setf (gethash "type" ht) "text"
             (gethash "text" ht) (getf block :text)))
      (:tool-use
       (setf (gethash "type" ht) "tool_use"
             (gethash "id" ht) (getf block :id)
             (gethash "name" ht) (getf block :name)
             (gethash "input" ht) (let ((input (getf block :input)))
                                    (if (hash-table-p input) input
                                        (plist-to-hash input)))))
      (:tool-result
       (setf (gethash "type" ht) "tool_result"
             (gethash "tool_use_id" ht) (getf block :tool-use-id)
             (gethash "content" ht) (getf block :content))
       (when (getf block :is-error)
         (setf (gethash "is_error" ht) t))))
    ht))

(defun plist-to-hash (plist)
  "Convert a keyword plist to a hash-table with string keys (lowercase, hyphens to underscores)."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (substitute #\_ #\-
                              (string-downcase (symbol-name k)))
                            ht)
                   v))
    ht))

(defun content-to-anthropic (content)
  "Convert message content (string or block list) to Anthropic API format.
   String → string (Anthropic accepts both).
   Block list → vector of block hash-tables."
  (if (stringp content)
      content
      (coerce (mapcar #'content-block-to-anthropic content) 'vector)))

(defun messages-to-anthropic (messages)
  "Convert normalized message plists to Anthropic API format (list of hash-tables)."
  (mapcar (lambda (msg)
            (let ((ht (make-hash-table :test #'equal)))
              (setf (gethash "role" ht)
                    (string-downcase (symbol-name (getf msg :role))))
              (setf (gethash "content" ht)
                    (content-to-anthropic (getf msg :content)))
              ht))
          messages))

(defun tool-def-to-anthropic (tool)
  "Convert a tool definition plist to an Anthropic API hash-table.
   TOOL is (:name \"...\" :description \"...\" :input-schema <hash-table-or-plist>)."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) (getf tool :name)
          (gethash "description" ht) (getf tool :description)
          (gethash "input_schema" ht) (getf tool :input-schema))
    ht))

(defun tool-choice-to-anthropic (choice)
  "Convert a tool-choice value to Anthropic API format.
   :auto → {\"type\":\"auto\"}
   :any → {\"type\":\"any\"}  
   :none → {\"type\":\"none\"}
   (:tool \"name\") → {\"type\":\"tool\",\"name\":\"name\"}"
  (let ((ht (make-hash-table :test #'equal)))
    (cond
      ((eq choice :auto) (setf (gethash "type" ht) "auto"))
      ((eq choice :any) (setf (gethash "type" ht) "any"))
      ((eq choice :none) (setf (gethash "type" ht) "none"))
      ((and (listp choice) (eq :tool (first choice)))
       (setf (gethash "type" ht) "tool"
             (gethash "name" ht) (second choice)))
      (t (setf (gethash "type" ht) "auto")))
    ht))

(defun build-anthropic-request (messages &key system max-tokens temperature
                                              model tools tool-choice)
  "Build the JSON request body as a hash-table for the Anthropic Messages API."
  (let ((body (make-hash-table :test #'equal)))
    (setf (gethash "model" body) model)
    (setf (gethash "max_tokens" body) (or max-tokens *anthropic-default-max-tokens*))
    (setf (gethash "messages" body) (coerce (messages-to-anthropic messages) 'vector))
    (when system
      (setf (gethash "system" body) system))
    (when temperature
      (setf (gethash "temperature" body) temperature))
    (when tools
      (setf (gethash "tools" body)
            (coerce (mapcar #'tool-def-to-anthropic tools) 'vector)))
    (when tool-choice
      (setf (gethash "tool_choice" body) (tool-choice-to-anthropic tool-choice)))
    body))

(defun anthropic-headers (api-key)
  "Build the HTTP headers for an Anthropic API request."
  (list (cons "x-api-key" api-key)
        (cons "anthropic-version" *anthropic-api-version*)
        (cons "content-type" "application/json")))

;;; --- Response parsing ---

(defun parse-anthropic-content-block (block)
  "Parse a single Anthropic response content block into a plist."
  (let ((type (gethash "type" block)))
    (cond
      ((string= type "text")
       (list :type :text :text (gethash "text" block)))
      ((string= type "tool_use")
       (list :type :tool-use
             :id (gethash "id" block)
             :name (gethash "name" block)
             :input (gethash "input" block)))
      (t
       (list :type :text :text (format nil "[unknown block type: ~A]" type))))))

(defun parse-anthropic-response (json)
  "Parse an Anthropic Messages API response (hash-table) into a standard plist.
   :content is a list of content block plists (not a raw string)."
  (let* ((content-blocks (gethash "content" json))
         (blocks (when (and content-blocks (vectorp content-blocks))
                   (loop for block across content-blocks
                         when (hash-table-p block)
                           collect (parse-anthropic-content-block block))))
         (stop-reason-str (gethash "stop_reason" json))
         (stop-reason (cond
                        ((string= stop-reason-str "end_turn") :end-turn)
                        ((string= stop-reason-str "max_tokens") :max-tokens)
                        ((string= stop-reason-str "tool_use") :tool-use)
                        ((string= stop-reason-str "stop_sequence") :stop-sequence)
                        (t :unknown)))
         (usage-ht (gethash "usage" json))
         (input-tokens (when usage-ht (gethash "input_tokens" usage-ht)))
         (output-tokens (when usage-ht (gethash "output_tokens" usage-ht))))
    (list :content blocks
          :stop-reason stop-reason
          :usage (list :input-tokens (or input-tokens 0)
                       :output-tokens (or output-tokens 0))
          :id (gethash "id" json)
          :model (gethash "model" json))))

(defun classify-anthropic-error (status body-str provider)
  "Signal the appropriate condition for an Anthropic API error."
  (let ((msg (or (handler-case
                     (let ((json (shasht:read-json body-str)))
                       (when (hash-table-p json)
                         (let ((err (gethash "error" json)))
                           (when (hash-table-p err)
                             (gethash "message" err)))))
                   (error () nil))
                 body-str)))
    (cond
      ((= status 401)
       (error 'llm-auth-error :provider provider :status status
                              :body body-str :message msg))
      ((= status 429)
       (error 'llm-rate-limit-error :provider provider :status status
                                    :body body-str :message msg))
      (t
       (error 'llm-api-error :provider provider :status status
                             :body body-str
                             :message (format nil "HTTP ~D: ~A" status msg))))))

;;; --- send-message implementation ---

(defmethod send-message ((provider anthropic-provider) messages
                         &key system max-tokens temperature tools tool-choice)
  (let* ((normalized (normalize-messages messages))
         (extracted-system nil)
         (final-messages normalized))
    (multiple-value-bind (sys rest) (extract-system-message normalized)
      (setf extracted-system (or system sys))
      (setf final-messages rest))
    (let* ((body (build-anthropic-request
                  final-messages
                  :system extracted-system
                  :max-tokens max-tokens
                  :temperature temperature
                  :model (provider-model provider)
                  :tools tools
                  :tool-choice tool-choice))
           (json-body (shasht:write-json body nil))
           (url (format nil "~A/v1/messages" (anthropic-api-base provider))))
      (log:info "Anthropic request: model=~A max_tokens=~A"
                (provider-model provider)
                (or max-tokens *anthropic-default-max-tokens*))
      (multiple-value-bind (response-body status)
          (handler-case
              (dex:post url
                :headers (anthropic-headers (anthropic-api-key provider))
                :content json-body)
            (error (c)
              (error 'llm-error :provider provider
                                :message (format nil "HTTP request failed: ~A" c))))
        (unless (= status 200)
          (classify-anthropic-error status response-body provider))
        (let* ((json (shasht:read-json response-body))
               (result (parse-anthropic-response json)))
          (log:info "Anthropic response: tokens=~A+~A stop=~A"
                    (getf (getf result :usage) :input-tokens)
                    (getf (getf result :usage) :output-tokens)
                    (getf result :stop-reason))
          result)))))
