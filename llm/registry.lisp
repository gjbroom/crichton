;;;; llm/registry.lisp
;;;;
;;;; Provider selection and construction from config + credential store.

(in-package #:crichton/llm)

(defvar *llm-provider* nil
  "The active LLM provider instance. Set during daemon initialization.")

(defun resolve-api-key (credential-name &optional (field :api-key))
  "Resolve an API key from the credential store."
  (handler-case
      (crichton/credentials:resolve-credential credential-name field)
    (error (c)
      (error 'llm-error :provider nil
                         :message (format nil "Cannot resolve API key ~S: ~A"
                                         credential-name c)))))

(defun make-llm-provider-from-config ()
  "Create an LLM provider based on the current configuration.
   Reads :llm config section for :provider, :model, and :api-key-credential.
   The API key is resolved from the credential store."
  (let* ((llm-config (crichton/config:config-get :llm))
         (provider-raw (or (getf llm-config :provider) :anthropic))
         (provider-id (if (keywordp provider-raw)
                          provider-raw
                          (intern (string-upcase (string provider-raw)) :keyword)))
         (model (getf llm-config :model))
         (cred-name (or (getf llm-config :api-key-credential)
                        (default-credential-name provider-id))))
    (ecase provider-id
      (:anthropic
       (let ((api-key (resolve-api-key cred-name)))
         (make-anthropic-provider :api-key api-key :model model))))))

(defun default-credential-name (provider-id)
  "Return the conventional credential store name for a provider."
  (ecase provider-id
    (:anthropic "anthropic-api-key")))

(defun ensure-llm-provider ()
  "Ensure the LLM provider is initialized. Creates from config if needed."
  (unless *llm-provider*
    (setf *llm-provider* (make-llm-provider-from-config)))
  *llm-provider*)

(defun chat (text &key (system nil) (max-tokens nil) (temperature nil))
  "Convenience function: send a single user message and return the response text.
   Uses the configured LLM provider."
  (let* ((provider (ensure-llm-provider))
         (messages (list (list :role :user :content text)))
         (response (send-message provider messages
                                 :system system
                                 :max-tokens max-tokens
                                 :temperature temperature)))
    (values (response-text response)
            response)))
