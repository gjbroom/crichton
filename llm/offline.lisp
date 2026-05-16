;;;; llm/offline.lisp
;;;;
;;;; Offline LLM provider. All LLM calls signal llm-offline-error.
;;;; Activate by setting [llm] provider = "offline" in config.toml.
;;;; Implement send-message/stream-message methods one by one as local
;;;; inference backends are added (e.g. llamafile, ollama).

(in-package #:crichton/llm)

(defclass offline-provider (llm-provider) ()
  (:documentation "Stub LLM provider for offline mode. All LLM calls signal llm-offline-error."))

(defun make-offline-provider ()
  (make-instance 'offline-provider :provider-id :offline :model "none"))

(defmethod send-message ((p offline-provider) messages &key &allow-other-keys)
  (declare (ignore messages))
  (error 'llm-offline-error :provider p
         :message "send-message not implemented in offline mode"))

(defmethod stream-message ((p offline-provider) messages on-event &key &allow-other-keys)
  (declare (ignore messages on-event))
  (error 'llm-offline-error :provider p
         :message "stream-message not implemented in offline mode"))

(defmethod list-models ((p offline-provider))
  nil)
