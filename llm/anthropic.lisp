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

(defparameter *sse-stream-timeout* 120
  "Seconds of silence on an SSE stream before the connection is force-closed.
This is a wall-clock idle timeout: if no bytes arrive for this many seconds
the watchdog closes the underlying socket fd, which unblocks any SSL_read
unconditionally.  SO_RCVTIMEO alone is insufficient because it resets on
every partial read, and bt:interrupt-thread is unreliable for threads blocked
inside OpenSSL.  Override with [llm] sse-stream-timeout = N in config.toml.")

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

;;; --- Retry / backoff ---

(defparameter *max-rate-limit-retries* 5
  "Maximum number of automatic retries on rate limit (429) responses.")

(defparameter *rate-limit-backoff-base* 1
  "Base delay in seconds for exponential backoff; doubles each retry.")

(defmacro with-rate-limit-retry (&body body)
  "Execute BODY, retrying on llm-rate-limit-error with exponential backoff.
   Sleeps (* *rate-limit-backoff-base* 2^attempt) seconds between retries,
   or the retry-after value from the error if present.
   Re-signals after *max-rate-limit-retries* attempts are exhausted."
  (let ((attempt (gensym "ATTEMPT"))
        (c (gensym "COND"))
        (delay (gensym "DELAY")))
    `(loop for ,attempt from 0
           do (handler-case
                  (return (progn ,@body))
                (llm-rate-limit-error (,c)
                  (when (>= ,attempt *max-rate-limit-retries*)
                    (error ,c))
                  (let ((,delay (or (llm-rate-limit-retry-after ,c)
                                    (* *rate-limit-backoff-base*
                                       (expt 2 ,attempt)))))
                    (log:warn "Rate limited; retrying in ~,1Fs (attempt ~D/~D)"
                              ,delay (1+ ,attempt) *max-rate-limit-retries*)
                    (sleep ,delay)))))))

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
      (log:info "Anthropic request: model=~A max_tokens=~A body=~Dchars sys=~Dchars tools=~D msgs=~D"
                (provider-model provider)
                (or max-tokens *anthropic-default-max-tokens*)
                (length json-body)
                (length (or extracted-system ""))
                (length (or tools #()))
                (length final-messages))
      (with-rate-limit-retry
        (multiple-value-bind (response-body status)
            (handler-case
                (dex:post url
                  :headers (anthropic-headers (anthropic-api-key provider))
                  :content json-body
                  :connect-timeout 10
                  :read-timeout 300)
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
            result))))))

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

;;; --- Stream accumulator state ---

(defclass stream-state ()
  ((text-accum      :initform (make-string-output-stream)
                    :accessor sstate-text-accum)
   (content-blocks  :initform nil
                    :accessor sstate-content-blocks)
   (msg-id          :initform nil :accessor sstate-msg-id)
   (msg-model       :initform nil :accessor sstate-msg-model)
   (stop-reason     :initform nil :accessor sstate-stop-reason)
   (input-tokens    :initform 0   :accessor sstate-input-tokens)
   (output-tokens   :initform 0   :accessor sstate-output-tokens)
   (current-block-type :initform nil :accessor sstate-current-block-type)
   (tool-blocks     :initform (make-hash-table)
                    :accessor sstate-tool-blocks)
   (tool-json-parts :initform (make-hash-table)
                    :accessor sstate-tool-json-parts)))

;;; --- SSE event dispatch ---

(defun sse-event-keyword (event-type)
  "Convert an SSE event-type string to a dispatch keyword."
  (intern (substitute #\- #\_ (string-upcase event-type)) :keyword))

(defgeneric handle-sse-event (state event json on-event)
  (:documentation "Handle a single parsed SSE event, updating STATE.
   EVENT is a keyword like :MESSAGE-START.  ON-EVENT is the user callback."))

(defmethod handle-sse-event (state event json on-event)
  "Default: ignore unknown SSE event types."
  (declare (ignore state json on-event event)))

(defmethod handle-sse-event ((s stream-state) (event (eql :message-start))
                             json on-event)
  (declare (ignore on-event))
  (let ((msg (gethash "message" json)))
    (when msg
      (setf (sstate-msg-id s) (gethash "id" msg))
      (setf (sstate-msg-model s) (gethash "model" msg))
      (let ((usage (gethash "usage" msg)))
        (when usage
          (setf (sstate-input-tokens s)
                (or (gethash "input_tokens" usage) 0)))))))

(defmethod handle-sse-event ((s stream-state) (event (eql :content-block-start))
                             json on-event)
  (declare (ignore on-event))
  (let* ((index (gethash "index" json))
         (cb (gethash "content_block" json))
         (btype (when cb (gethash "type" cb))))
    (setf (sstate-current-block-type s) btype)
    (when (and cb (string= btype "tool_use"))
      (setf (gethash index (sstate-tool-blocks s))
            (list :type :tool-use
                  :id (gethash "id" cb)
                  :name (gethash "name" cb)
                  :input nil))
      (setf (gethash index (sstate-tool-json-parts s)) nil))))

(defmethod handle-sse-event ((s stream-state) (event (eql :content-block-delta))
                             json on-event)
  (let* ((delta (gethash "delta" json))
         (dtype (when delta (gethash "type" delta))))
    (cond
      ((and delta (string= dtype "text_delta"))
       (let ((text (gethash "text" delta)))
         (when text
           (write-string text (sstate-text-accum s))
           (funcall on-event (list :type :delta :text text)))))
      ((and delta (string= dtype "input_json_delta"))
       (let ((partial (gethash "partial_json" delta))
             (idx (gethash "index" json)))
         (when partial
           (push partial (gethash idx (sstate-tool-json-parts s)))))))))

(defun finalize-tool-block (state idx)
  "Finalize a tool-use content block at IDX, parsing accumulated JSON fragments."
  (let* ((parts (nreverse (gethash idx (sstate-tool-json-parts state))))
         (full-json (format nil "~{~A~}" parts))
         (input (if (plusp (length full-json))
                    (handler-case (shasht:read-json full-json)
                      (error () (make-hash-table :test #'equal)))
                    (make-hash-table :test #'equal)))
         (block (gethash idx (sstate-tool-blocks state))))
    (setf (getf block :input) input)
    (push block (sstate-content-blocks state))))

(defun finalize-text-block (state)
  "Finalize the current text content block, draining the text accumulator."
  (let ((text-so-far (get-output-stream-string (sstate-text-accum state))))
    (when (plusp (length text-so-far))
      (push (list :type :text :text text-so-far)
            (sstate-content-blocks state)))))

(defmethod handle-sse-event ((s stream-state) (event (eql :content-block-stop))
                             json on-event)
  (declare (ignore on-event))
  (let ((idx (gethash "index" json)))
    (cond
      ((gethash idx (sstate-tool-blocks s))
       (finalize-tool-block s idx))
      ((string= (sstate-current-block-type s) "text")
       (finalize-text-block s)))))

(defmethod handle-sse-event ((s stream-state) (event (eql :message-delta))
                             json on-event)
  (declare (ignore on-event))
  (let ((delta (gethash "delta" json))
        (usage (gethash "usage" json)))
    (when delta
      (setf (sstate-stop-reason s)
            (parse-stop-reason (gethash "stop_reason" delta))))
    (when usage
      (setf (sstate-output-tokens s)
            (or (gethash "output_tokens" usage) 0)))))

(defmethod handle-sse-event ((s stream-state) (event (eql :message-stop))
                             json on-event)
  (declare (ignore json))
  (funcall on-event
           (list :type :done
                 :stop-reason (sstate-stop-reason s)
                 :usage (list :input-tokens (sstate-input-tokens s)
                              :output-tokens (sstate-output-tokens s))
                 :id (sstate-msg-id s)
                 :model (sstate-msg-model s))))

;;; --- Stream result assembly ---

(defun stream-state-result (state)
  "Build the final response plist from accumulated stream state.
   Signals LLM-ERROR if no content blocks were received — this indicates the
   HTTP connection was dropped before any SSE events arrived (e.g. a stale
   pooled connection returning immediate EOF), not a legitimate empty reply."
  (let ((full-text (get-output-stream-string (sstate-text-accum state))))
    (when (plusp (length full-text))
      (push (list :type :text :text full-text)
            (sstate-content-blocks state))))
  (let* ((blocks (nreverse (sstate-content-blocks state)))
         (stop (or (sstate-stop-reason state) :end-turn))
         (result (list :content blocks
                       :stop-reason stop
                       :usage (list :input-tokens (sstate-input-tokens state)
                                    :output-tokens (sstate-output-tokens state))
                       :id (sstate-msg-id state)
                       :model (sstate-msg-model state))))
    (when (null blocks)
      (error 'llm-error
             :message "Streaming response contained no content blocks — connection may have been dropped"))
    (log:info "Anthropic streaming response: tokens=~A+~A stop=~A"
              (sstate-input-tokens state) (sstate-output-tokens state) stop)
    result))

;;; --- Streaming HTTP helpers ---

(defun extract-dex-error-body (condition)
  "Extract a string body from a dexador HTTP-REQUEST-FAILED condition."
  (let ((body (dex:response-body condition)))
    (cond
      ((stringp body) body)
      ((streamp body)
       (let ((octets (flexi-streams:with-output-to-sequence (out)
                       (loop for byte = (read-byte body nil nil)
                             while byte do (write-byte byte out)))))
         (ignore-errors (close body))
         (flexi-streams:octets-to-string octets :external-format :utf-8)))
      (t (format nil "~A" body)))))

(defun drain-stream-to-string (stream)
  "Read all lines from STREAM into a string, then close it."
  (let ((buf (make-string-output-stream)))
    (loop for line = (read-line stream nil nil)
          while line do (write-line line buf))
    (close stream)
    (get-output-stream-string buf)))

(defun anthropic-streaming-post (provider json-body)
  "POST JSON-BODY to the Anthropic Messages API with streaming enabled.
   Returns the response stream.  Signals LLM conditions on failure."
  (let ((url (format nil "~A/v1/messages" (anthropic-api-base provider))))
    (multiple-value-bind (response-stream status)
        (handler-case
            (dex:post url
              :headers (anthropic-headers (anthropic-api-key provider))
              :content json-body
              :want-stream t
              :force-binary t
              :use-connection-pool nil  ; don't pool streaming conns — interrupted streams
                                        ; leave the socket in an indeterminate state
              :read-timeout 120)
          (dex:http-request-failed (c)
            (classify-anthropic-error
             (dex:response-status c)
             (extract-dex-error-body c)
             provider))
          (error (c)
            (error 'llm-error :provider provider
                              :message (format nil "HTTP request failed: ~A" c))))
      (unless (= status 200)
        (classify-anthropic-error
         status (drain-stream-to-string response-stream) provider))
      response-stream)))

(defun dispatch-sse-event (state on-event event-type data)
  "Parse DATA as JSON and dispatch to the appropriate HANDLE-SSE-EVENT method.
   Logs and swallows errors from malformed individual events so one bad event
   does not abort the stream.  OPERATION-CANCELLED is re-raised so timeouts
   propagate correctly past this error-swallowing boundary."
  (handler-case
      (let ((json (shasht:read-json data)))
        (handle-sse-event state (sse-event-keyword event-type) json on-event))
    (crichton/skills:operation-cancelled (c) (error c))
    (error (c)
      (log:warn "Error processing SSE event ~A: ~A" event-type c))))

(defun process-sse-stream (response-stream state on-event
                           &key (timeout *sse-stream-timeout*))
  "Read SSE events from RESPONSE-STREAM, dispatching to STATE methods.
Ensures RESPONSE-STREAM is closed on exit.

A stream-closing watchdog fires after TIMEOUT seconds of silence: it closes
RESPONSE-STREAM directly, which unblocks any in-progress SSL_read
unconditionally.  This is more reliable than bt:interrupt-thread (which
depends on signal delivery through the SSL layer) and more precise than
SO_RCVTIMEO (which resets on every partial read, allowing indefinite stalls).

On timeout the error is re-raised as OPERATION-CANCELLED so callers that
catch broad ERROR conditions (e.g. dispatch-sse-event) re-raise it rather
than swallowing it.

Timeout layering: this watchdog (120s idle) is the primary mechanism for
SSE streams. The outer with-timeout in run-agent-loop (300s elapsed) is a
safety net covering non-streaming send-message calls and the case where
this watchdog itself fails to fire."
  (let ((stream-done (list nil))      ; set t by unwind-protect; prevents late watchdog fire
        (watchdog-fired (list nil)))  ; set t by watchdog; used to classify the stream error
    (let ((watchdog
           (bt:make-thread
            (lambda ()
              (sleep timeout)
              (unless (car stream-done)
                (setf (car watchdog-fired) t)
                (log:warn "SSE stream idle for ~As — force-closing connection" timeout)
                (ignore-errors (close response-stream :abort t))))
            :name "sse-stream-watchdog")))
      (unwind-protect
           (let ((char-stream (flexi-streams:make-flexi-stream
                                response-stream :external-format :utf-8)))
             (handler-case
                 (parse-sse-events
                  char-stream
                  (lambda (event-type data)
                    (dispatch-sse-event state on-event event-type data)))
               (error (c)
                 (if (car watchdog-fired)
                     (error 'operation-cancelled
                            :message (format nil "SSE stream timed out after ~As of silence" timeout))
                     (error c)))))
        (setf (car stream-done) t)
        (when (bt:thread-alive-p watchdog)
          (ignore-errors (bt:destroy-thread watchdog)))
        (ignore-errors (close response-stream))))))

;;; --- stream-message method ---

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
           (json-body (shasht:write-json body nil)))
      (let ((tools-json-size (if tools
                                  (length (shasht:write-json
                                           (coerce (mapcar #'tool-def-to-anthropic tools) 'vector)
                                           nil))
                                  0))
            (msgs-json-size (length (shasht:write-json
                                     (coerce (messages-to-anthropic final-messages) 'vector)
                                     nil))))
        (log:info "Anthropic streaming request: model=~A max_tokens=~A body=~Dchars sys=~Dchars tools=~Dchars(~D) msgs=~Dchars(~D)"
                  (provider-model provider)
                  (or max-tokens *anthropic-default-max-tokens*)
                  (length json-body)
                  (length (or extracted-system ""))
                  tools-json-size (length (or tools '()))
                  msgs-json-size (length final-messages)))
      (with-rate-limit-retry
        (let ((response-stream (anthropic-streaming-post provider json-body))
              (state (make-instance 'stream-state)))
          (process-sse-stream response-stream state on-event)
          (stream-state-result state))))))
