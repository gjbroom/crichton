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
                                              model tools tool-choice stream)
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
    (when stream
      (setf (gethash "stream" body) t))
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

;;; --- SSE parsing ---

(defun read-sse-line (stream)
  "Read a single line from STREAM, stripping trailing CR if present.
   Returns the line string or NIL at EOF."
  (let ((line (read-line stream nil nil)))
    (when line
      (let ((len (length line)))
        (if (and (plusp len) (char= (char line (1- len)) #\Return))
            (subseq line 0 (1- len))
            line)))))

(defun parse-sse-events (stream on-sse-event)
  "Read SSE events from STREAM and call ON-SSE-EVENT for each complete event.
   ON-SSE-EVENT receives two arguments: event-type (string) and data (string).
   SSE format: lines prefixed with 'event:', 'data:', or ':' (comments).
   Empty lines dispatch the accumulated event."
  (let ((event-type nil)
        (data-parts nil))
    (loop for line = (read-sse-line stream)
          while line
          do (cond
               ;; Empty line → dispatch event
               ((string= line "")
                (when data-parts
                  (let ((data (format nil "~{~A~^~%~}" (nreverse data-parts))))
                    (funcall on-sse-event (or event-type "message") data)))
                (setf event-type nil
                      data-parts nil))
               ;; Comment line (including : ping)
               ((char= (char line 0) #\:)
                nil)
               ;; event: type
               ((and (>= (length line) 7)
                     (string= line "event: " :end1 7))
                (setf event-type (subseq line 7)))
               ;; data: payload
               ((and (>= (length line) 6)
                     (string= line "data: " :end1 6))
                (push (subseq line 6) data-parts))
               ;; data without space after colon (edge case per SSE spec)
               ((and (>= (length line) 5)
                     (string= line "data:" :end1 5))
                (push (subseq line 5) data-parts))))))

;;; --- stream-message implementation ---

(defun parse-stop-reason (reason-str)
  "Convert an Anthropic stop_reason string to a keyword."
  (cond
    ((null reason-str) :unknown)
    ((string= reason-str "end_turn") :end-turn)
    ((string= reason-str "max_tokens") :max-tokens)
    ((string= reason-str "tool_use") :tool-use)
    ((string= reason-str "stop_sequence") :stop-sequence)
    (t :unknown)))

(defmethod stream-message ((provider anthropic-provider) messages on-event
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
                  :tool-choice tool-choice
                  :stream t))
           (json-body (shasht:write-json body nil))
           (url (format nil "~A/v1/messages" (anthropic-api-base provider))))
      (log:info "Anthropic streaming request: model=~A max_tokens=~A"
                (provider-model provider)
                (or max-tokens *anthropic-default-max-tokens*))
      (multiple-value-bind (response-stream status)
          (handler-case
              (dex:post url
                :headers (anthropic-headers (anthropic-api-key provider))
                :content json-body
                :want-stream t
                :force-binary t
                :read-timeout 120)
            (error (c)
              (error 'llm-error :provider provider
                                :message (format nil "HTTP request failed: ~A" c))))
        (unless (= status 200)
          (let ((body-str (let ((buf (make-string-output-stream)))
                            (loop for line = (read-line response-stream nil nil)
                                  while line do (write-line line buf))
                            (close response-stream)
                            (get-output-stream-string buf))))
            (classify-anthropic-error status body-str provider)))
        (let ((text-accum (make-string-output-stream))
              (content-blocks nil)
              (msg-id nil)
              (msg-model nil)
              (stop-reason nil)
              (input-tokens 0)
              (output-tokens 0)
              ;; Tool use accumulation
              (tool-blocks (make-hash-table))  ; index → plist
              (tool-json-parts (make-hash-table)) ; index → list of strings
              (current-block-type nil))
          (unwind-protect
               (let ((char-stream
                       (flexi-streams:make-flexi-stream
                        response-stream :external-format :utf-8)))
                 (parse-sse-events
                  char-stream
                  (lambda (event-type data)
                    (handler-case
                        (let ((json (shasht:read-json data)))
                          (cond
                            ;; message_start: extract id, model, usage
                            ((string= event-type "message_start")
                             (let ((msg (gethash "message" json)))
                               (when msg
                                 (setf msg-id (gethash "id" msg))
                                 (setf msg-model (gethash "model" msg))
                                 (let ((usage (gethash "usage" msg)))
                                   (when usage
                                     (setf input-tokens
                                           (or (gethash "input_tokens" usage) 0)))))))

                            ;; content_block_start: track block type
                            ((string= event-type "content_block_start")
                             (let* ((index (gethash "index" json))
                                    (cb (gethash "content_block" json))
                                    (btype (when cb (gethash "type" cb))))
                               (setf current-block-type btype)
                               (when (and cb (string= btype "tool_use"))
                                 (setf (gethash index tool-blocks)
                                       (list :type :tool-use
                                             :id (gethash "id" cb)
                                             :name (gethash "name" cb)
                                             :input nil))
                                 (setf (gethash index tool-json-parts) nil))))

                            ;; content_block_delta: text or tool input
                            ((string= event-type "content_block_delta")
                             (let* ((delta (gethash "delta" json))
                                    (dtype (when delta (gethash "type" delta))))
                               (cond
                                 ((and delta (string= dtype "text_delta"))
                                  (let ((text (gethash "text" delta)))
                                    (when text
                                      (write-string text text-accum)
                                      (funcall on-event
                                               (list :type :delta :text text)))))
                                 ((and delta (string= dtype "input_json_delta"))
                                  (let ((partial (gethash "partial_json" delta))
                                        (idx (gethash "index" json)))
                                    (when partial
                                      (push partial
                                            (gethash idx tool-json-parts))))))))

                            ;; content_block_stop: finalize blocks
                            ((string= event-type "content_block_stop")
                             (let ((idx (gethash "index" json)))
                               (cond
                                 ;; Tool use block: parse accumulated JSON
                                 ((gethash idx tool-blocks)
                                  (let* ((parts (nreverse
                                                 (gethash idx tool-json-parts)))
                                         (full-json
                                           (format nil "~{~A~}" parts))
                                         (input
                                           (if (plusp (length full-json))
                                               (handler-case
                                                   (shasht:read-json full-json)
                                                 (error ()
                                                   (make-hash-table
                                                    :test #'equal)))
                                               (make-hash-table
                                                :test #'equal)))
                                         (block (gethash idx tool-blocks)))
                                    (setf (getf block :input) input)
                                    (push block content-blocks)))
                                 ;; Text block
                                 ((string= current-block-type "text")
                                  nil))))

                            ;; message_delta: stop reason + output tokens
                            ((string= event-type "message_delta")
                             (let ((delta (gethash "delta" json))
                                   (usage (gethash "usage" json)))
                               (when delta
                                 (setf stop-reason
                                       (parse-stop-reason
                                        (gethash "stop_reason" delta))))
                               (when usage
                                 (setf output-tokens
                                       (or (gethash "output_tokens" usage)
                                           0)))))

                            ;; message_stop: emit done event
                            ((string= event-type "message_stop")
                             (funcall on-event
                                      (list :type :done
                                            :stop-reason stop-reason
                                            :usage (list :input-tokens
                                                         input-tokens
                                                         :output-tokens
                                                         output-tokens)
                                            :id msg-id
                                            :model msg-model)))))
                      (error (c)
                        (log:warn "Error processing SSE event ~A: ~A"
                                  event-type c))))))
            (close response-stream))
          ;; Build the final text block if we accumulated any text
          (let ((full-text (get-output-stream-string text-accum)))
            (when (plusp (length full-text))
              (push (list :type :text :text full-text) content-blocks)))
          ;; Return blocks in order (they were pushed in reverse)
          (setf content-blocks (nreverse content-blocks))
          (let ((result (list :content content-blocks
                              :stop-reason (or stop-reason :end-turn)
                              :usage (list :input-tokens input-tokens
                                           :output-tokens output-tokens)
                              :id msg-id
                              :model msg-model)))
            (log:info "Anthropic streaming response: tokens=~A+~A stop=~A"
                      input-tokens output-tokens (or stop-reason :end-turn))
            result))))))
