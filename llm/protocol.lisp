;;;; llm/protocol.lisp
;;;;
;;;; CLOS protocol for LLM providers.
;;;; Generic interface so new providers slot in without refactoring.

(in-package #:crichton/llm)

;;; --- Conditions ---

(define-condition llm-error (error)
  ((provider :initarg :provider :reader llm-error-provider
             :documentation "The llm-provider instance that raised the error.")
   (message :initarg :message :reader llm-error-message
            :documentation "Human-readable description of the error."))
  (:documentation "Base condition for all LLM provider errors.")
  (:report (lambda (c s)
             (format s "LLM error (~A): ~A"
                     (if (slot-boundp c 'provider)
                         (provider-id (llm-error-provider c))
                         "unknown")
                     (llm-error-message c)))))

(define-condition llm-api-error (llm-error)
  ((status :initarg :status :reader llm-api-error-status
           :documentation "HTTP status code returned by the provider.")
   (body :initarg :body :reader llm-api-error-body
         :documentation "Raw response body from the provider, for diagnostics."))
  (:documentation "Condition for LLM errors that carry an HTTP status code.")
  (:report (lambda (c s)
             (format s "LLM API error (~A) HTTP ~D: ~A"
                     (if (slot-boundp c 'provider)
                         (provider-id (llm-error-provider c))
                         "unknown")
                     (llm-api-error-status c)
                     (llm-error-message c)))))

(define-condition llm-auth-error (llm-api-error) ()
  (:documentation "Condition signaled when the provider rejects the API key (HTTP 401/403).")
  (:report (lambda (c s)
             (format s "LLM authentication failed (~A): ~A"
                     (if (slot-boundp c 'provider)
                         (provider-id (llm-error-provider c))
                         "unknown")
                     (llm-error-message c)))))

(define-condition llm-rate-limit-error (llm-api-error)
  ((retry-after :initarg :retry-after :reader llm-rate-limit-retry-after
                :initform nil
                :documentation "Suggested retry delay in seconds, or NIL if unknown."))
  (:documentation "Condition signaled when the provider returns HTTP 429.
The daemon's with-llm-error-handling macro retries up to 3 times using
retry-after (with a fallback backoff) before propagating the error.")
  (:report (lambda (c s)
             (format s "LLM rate limited (~A): ~A"
                     (if (slot-boundp c 'provider)
                         (provider-id (llm-error-provider c))
                         "unknown")
                     (llm-error-message c)))))

(define-condition llm-feature-not-supported (llm-error)
  ((feature :initarg :feature :reader llm-unsupported-feature
            :documentation "Keyword naming the unsupported feature (e.g. :streaming, :list-models)."))
  (:documentation "Condition signaled by default method implementations for optional protocol features.")
  (:report (lambda (c s)
             (format s "LLM feature ~A not supported by ~A"
                     (llm-unsupported-feature c)
                     (if (slot-boundp c 'provider)
                         (provider-id (llm-error-provider c))
                         "unknown")))))

;;; --- Base class ---

(defclass llm-provider ()
  ((provider-id :initarg :provider-id :reader provider-id
                :type keyword
                :documentation "Stable keyword identifier for this provider (e.g. :anthropic).")
   (model :initarg :model :accessor provider-model
          :type string
          :documentation "Model name string used in API requests (e.g. \"claude-sonnet-4-6\")."))
  (:documentation "Abstract base class for LLM provider backends.
Concrete providers subclass this and specialize send-message (required)
and optionally stream-message and list-models."))

;;; --- Generic protocol ---

(defgeneric send-message (provider messages &key system max-tokens temperature
                                                tools tool-choice)
  (:documentation
   "Send messages to the LLM and return a response plist.
    MESSAGES is a list of plists: ((:role :user :content \"...\") ...)
    Returns a plist:
      (:content \"response text\"
       :stop-reason :end-turn|:max-tokens|:tool-use
       :usage (:input-tokens N :output-tokens N)
       :id \"msg_...\")"))

(defgeneric stream-message (provider messages on-event &key system max-tokens
                                                            temperature tools tool-choice)
  (:documentation
   "Stream a response from the LLM, calling ON-EVENT for each chunk.
    ON-EVENT receives plists like (:type :delta :text \"...\") or (:type :done ...).
    Not all providers support streaming."))

(defgeneric list-models (provider)
  (:documentation "Return a list of available model name strings."))

;;; --- Default methods ---

(defmethod stream-message ((p llm-provider) messages on-event &key &allow-other-keys)
  "Default: signals llm-feature-not-supported. Providers that support streaming override this."
  (declare (ignore messages on-event))
  (error 'llm-feature-not-supported :feature :streaming :provider p))

(defmethod list-models ((p llm-provider))
  "Default: signals llm-feature-not-supported. Providers that support model enumeration override this."
  (error 'llm-feature-not-supported :feature :list-models :provider p))

;;; --- Message helpers ---

(defun normalize-messages (messages)
  "Normalize a list of message plists. Ensures :role is a keyword and :content exists."
  (mapcar (lambda (msg)
            (let ((role (getf msg :role))
                  (content (getf msg :content)))
              (unless role (error "Message missing :role"))
              (unless content (error "Message missing :content (role=~A, msg=~S)" role msg))
              (list :role (if (keywordp role) role
                              (intern (string-upcase (string role)) :keyword))
                    :content content)))
          messages))

(defun extract-system-message (messages)
  "Split out any leading :system role messages from MESSAGES.
   Returns (values system-string remaining-messages).
   Anthropic requires system as a separate parameter."
  (let ((system-parts nil)
        (rest messages))
    (loop while (and rest (eq :system (getf (first rest) :role)))
          do (push (getf (first rest) :content) system-parts)
             (pop rest))
    (values (when system-parts
              (format nil "~{~A~^~%~%~}" (nreverse system-parts)))
            rest)))

(defun response-text (response)
  "Extract the text content from a provider response plist.
   Works for both string :content and content block lists."
  (let ((content (getf response :content)))
    (if (stringp content)
        content
        (blocks-text content))))

;;; --- Content block helpers ---
;;; With tool use, message :content can be either a string or a list of
;;; content block plists. These helpers normalize between the two forms.

(defun content-blocks (content)
  "Normalize CONTENT to a list of content block plists.
   String → ((:type :text :text \"...\")).
   List of plists → returned as-is.
   NIL → NIL."
  (cond
    ((null content) nil)
    ((stringp content) (list (list :type :text :text content)))
    ((listp content) content)
    (t (list (list :type :text :text (princ-to-string content))))))

(defun blocks-text (blocks)
  "Extract concatenated text from a list of content block plists.
   Ignores non-text blocks (tool_use, tool_result)."
  (with-output-to-string (out)
    (dolist (block (content-blocks blocks))
      (when (eq :text (getf block :type))
        (let ((text (getf block :text)))
          (when text (write-string text out)))))))

(defun blocks-tool-uses (blocks)
  "Extract all tool_use blocks from a list of content block plists.
   Returns a list of plists with :type :tool-use :id :name :input."
  (remove-if-not (lambda (block)
                   (eq :tool-use (getf block :type)))
                 (content-blocks blocks)))

(defun make-tool-result-block (tool-use-id content &key (is-error nil))
  "Build a tool_result content block plist."
  (let ((block (list :type :tool-result
                     :tool-use-id tool-use-id
                     :content content)))
    (when is-error
      (setf (getf block :is-error) t))
    block))

;;; --- Usage metering hook ---
;;; Records every send-message call into the general-purpose meter system.
;;; Works for any provider — the :around method fires regardless of backend.

(defun %spawn-usage-persist (label model input-tokens output-tokens)
  "Fire-and-forget thread: persist usage metrics without blocking the caller.
   Runs record-usage in a background thread so storage lock contention from
   flush-all-storage (scheduled every 60s) cannot block the LLM call path."
  (bt:make-thread
   (lambda ()
     (handler-case
         (crichton/skills:record-usage "llm" model input-tokens output-tokens)
       (error (c)
         (log:warn "Failed to record ~A usage: ~A" label c))))
   :name "llm-usage-persist"))

(defmethod send-message :around ((provider llm-provider) messages &key &allow-other-keys)
  "Around method that fires a background usage-persist thread after every send-message call.
Runs for all providers; token counts come from the result's :usage plist."
  (let ((result (call-next-method)))
    (let* ((usage (getf result :usage))
           (model (or (getf result :model)
                      (provider-model provider)
                      "unknown"))
           (input-tokens (or (getf usage :input-tokens) 0))
           (output-tokens (or (getf usage :output-tokens) 0)))
      (%spawn-usage-persist "LLM" model input-tokens output-tokens))
    result))

(defmethod stream-message :around ((provider llm-provider) messages on-event
                                   &key &allow-other-keys)
  "Around method that fires a background usage-persist thread after every stream-message call."
  (declare (ignore messages on-event))
  (let ((result (call-next-method)))
    (let* ((usage (getf result :usage))
           (model (or (getf result :model)
                      (provider-model provider)
                      "unknown"))
           (input-tokens (or (getf usage :input-tokens) 0))
           (output-tokens (or (getf usage :output-tokens) 0)))
      (%spawn-usage-persist "LLM streaming" model input-tokens output-tokens))
    result))
