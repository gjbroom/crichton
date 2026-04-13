;;;; skills/retry-infrastructure.lisp
;;;;
;;;; Unified retry and rate limiting infrastructure for network operations.
;;;; Eliminates code duplication across different skill modules.
;;;; Provides both client-side retry (exponential backoff) and server-side
;;;; rate limiting with configurable parameters and error classification.

(in-package #:crichton/skills)

;;; --- Retry configuration ---

(defparameter *default-max-retries* 3
  "Default maximum number of automatic retries on transient failures.")

(defparameter *default-backoff-base* 1
  "Default base delay in seconds for exponential backoff; doubles each retry.")

(defparameter *default-max-backoff* 30
  "Default maximum backoff delay in seconds.")

;;; --- Error classification ---

(defgeneric transient-error-p (condition context)
  (:documentation "Return T if CONDITION represents a transient error worth retrying in CONTEXT."))

(defmethod transient-error-p ((condition error) (context symbol))
  "Default method - check for common transient network errors."
  (or
   ;; Network connection failures
   (typep condition 'usocket:socket-error)
   ;; dexador timeout and server errors
   (and (typep condition 'dex:http-request-failed)
        (let ((status (dex:response-status condition)))
          ;; Retry on 5xx server errors, 429 rate limit, 503 service unavailable
          (or (>= status 500)
              (= status 429)
              (= status 503))))
   ;; General timeout/connection strings
   (let ((msg (format nil "~A" condition)))
     (or (search "timeout" msg :test #'char-equal)
         (search "connection" msg :test #'char-equal)
         (search "network" msg :test #'char-equal)))))

;;; Context-specific error classification
(defmethod transient-error-p ((condition error) (context (eql :weather)))
  "Weather API specific error classification."
  (cond
    ;; 404 for weather is non-retryable (city not found)
    ((and (typep condition 'dex:http-request-failed)
          (= (dex:response-status condition) 404))
     nil)
    ;; Use default classification otherwise
    (t (call-next-method))))

(defmethod transient-error-p ((condition error) (context (eql :rss)))
  "RSS feed specific error classification."
  (or
   ;; XML parsing errors might be transient (incomplete download)
   (typep condition 'xmls::xml-parse-error)
   ;; Use default network classification
   (call-next-method)))

(defmethod transient-error-p ((condition error) (context (eql :github)))
  "GitHub API specific error classification."
  (cond
    ;; 404/403 are usually permanent (repo not found, no access)
    ((and (typep condition 'dex:http-request-failed)
          (member (dex:response-status condition) '(404 403)))
     nil)
    ;; Use default classification otherwise
    (t (call-next-method))))

;;; --- Unified retry macro ---

(defmacro with-retry ((&key (max-retries '*default-max-retries*)
                            (backoff-base '*default-backoff-base*)
                            (max-backoff '*default-max-backoff*)
                            (context nil)
                            (on-retry nil))
                      &body body)
  "Execute BODY with automatic retry on transient errors.
   
   Parameters:
   - MAX-RETRIES: Maximum retry attempts (default *default-max-retries*)
   - BACKOFF-BASE: Base delay for exponential backoff (default *default-backoff-base*)  
   - MAX-BACKOFF: Maximum delay cap (default *default-max-backoff*)
   - CONTEXT: Symbol for context-specific error classification
   - ON-RETRY: Function called before each retry with (condition attempt delay)
   
   Uses exponential backoff: delay = min(max-backoff, base * 2^attempt)
   Calls transient-error-p with CONTEXT to determine if errors are retryable."
  (let ((attempt (gensym "ATTEMPT"))
        (c (gensym "COND"))
        (delay (gensym "DELAY"))
        (max-ret (gensym "MAX-RETRIES"))
        (base (gensym "BASE"))
        (max-back (gensym "MAX-BACKOFF")))
    `(let ((,max-ret ,max-retries)
           (,base ,backoff-base)
           (,max-back ,max-backoff))
       (loop for ,attempt from 0
             do (handler-case
                    (return (progn ,@body))
                  (error (,c)
                    (if (and (< ,attempt ,max-ret)
                             (transient-error-p ,c ,context))
                        (let ((,delay (min ,max-back 
                                           (* ,base (expt 2 ,attempt)))))
                          ,@(when on-retry 
                              `((funcall ,on-retry ,c ,attempt ,delay)))
                          (log:warn "~@[~A: ~]Transient error; retrying in ~,1Fs (attempt ~D/~D): ~A"
                                    ,context ,delay (1+ ,attempt) ,max-ret ,c)
                          (sleep ,delay))
                        (error ,c))))))))

;;; --- HTTP request helpers with retry ---

(defun http-get-with-retry (url &key headers (context nil) (max-retries *default-max-retries*)
                                     (connect-timeout 10) (read-timeout 30))
  "Make HTTP GET request with automatic retry on transient failures."
  (with-retry (:context context :max-retries max-retries)
    (dex:get url 
             :headers headers
             :connect-timeout connect-timeout
             :read-timeout read-timeout)))

(defun http-post-with-retry (url &key headers content (context nil) (max-retries *default-max-retries*)
                                      (connect-timeout 10) (read-timeout 30))
  "Make HTTP POST request with automatic retry on transient failures."
  (with-retry (:context context :max-retries max-retries)
    (dex:post url
              :headers headers
              :content content
              :connect-timeout connect-timeout
              :read-timeout read-timeout)))

(defun http-put-with-retry (url &key headers content (context nil) (max-retries *default-max-retries*)
                                     (connect-timeout 10) (read-timeout 30))
  "Make HTTP PUT request with automatic retry on transient failures."
  (with-retry (:context context :max-retries max-retries)
    (dex:put url
             :headers headers
             :content content
             :connect-timeout connect-timeout
             :read-timeout read-timeout)))

(defun http-delete-with-retry (url &key headers content (context nil) (max-retries *default-max-retries*)
                                        (connect-timeout 10) (read-timeout 30))
  "Make HTTP DELETE request with automatic retry on transient failures."
  (with-retry (:context context :max-retries max-retries)
    (dex:delete url
                :headers headers
                :content content
                :connect-timeout connect-timeout
                :read-timeout read-timeout)))

;;; --- JSON parsing with retry ---

(defun parse-json-with-retry (json-string &key (context nil))
  "Parse JSON string with retry on transient parsing failures."
  (with-retry (:context context :max-retries 2 :backoff-base 0.1)
    (handler-case
        (shasht:read-json json-string)
      (error (c)
        (error "JSON parsing failed: ~A" c)))))

;;; --- Rate limiting infrastructure ---

(defclass rate-limiter ()
  ((requests-per-window :initarg :requests-per-window
                        :reader rate-limiter-requests-per-window
                        :initform 60
                        :type integer)
   (window-seconds :initarg :window-seconds
                   :reader rate-limiter-window-seconds
                   :initform 60
                   :type integer)
   (clients :initform (make-hash-table :test #'equal)
            :accessor rate-limiter-clients
            :type hash-table)
   (lock :initform (bt:make-lock "rate-limiter")
         :reader rate-limiter-lock)))

(defclass client-bucket ()
  ((requests :initform nil
             :accessor client-bucket-requests
             :type list)
   (blocked-until :initform 0
                  :accessor client-bucket-blocked-until
                  :type integer))
  (:documentation "Sliding window rate limit bucket for a single client."))

(defmethod check-rate-limit ((limiter rate-limiter) client-id)
  "Check if CLIENT-ID is within rate limits. Returns (values allowed-p retry-after-seconds)."
  (bt:with-lock-held ((rate-limiter-lock limiter))
    (let* ((now (get-universal-time))
           (window-start (- now (rate-limiter-window-seconds limiter)))
           (bucket (or (gethash client-id (rate-limiter-clients limiter))
                       (setf (gethash client-id (rate-limiter-clients limiter))
                             (make-instance 'client-bucket)))))
      
      ;; Check if client is in penalty box
      (when (> (client-bucket-blocked-until bucket) now)
        (return-from check-rate-limit 
          (values nil (- (client-bucket-blocked-until bucket) now))))
      
      ;; Remove old requests outside the window
      (setf (client-bucket-requests bucket)
            (remove-if (lambda (timestamp) (< timestamp window-start))
                       (client-bucket-requests bucket)))
      
      ;; Check if within limit
      (if (< (length (client-bucket-requests bucket))
             (rate-limiter-requests-per-window limiter))
          (progn
            ;; Allow request and record timestamp
            (push now (client-bucket-requests bucket))
            (values t 0))
          (progn
            ;; Rate limited - calculate penalty
            (let ((penalty-seconds (* 2 (rate-limiter-window-seconds limiter))))
              (setf (client-bucket-blocked-until bucket) (+ now penalty-seconds))
              (log:warn "Rate limited client ~A for ~Ds (~D requests in ~Ds)"
                        client-id penalty-seconds
                        (length (client-bucket-requests bucket))
                        (rate-limiter-window-seconds limiter))
              (values nil penalty-seconds)))))))

(defmethod cleanup-rate-limiter ((limiter rate-limiter))
  "Remove old client buckets to prevent memory leaks."
  (bt:with-lock-held ((rate-limiter-lock limiter))
    (let* ((now (get-universal-time))
           (cleanup-before (- now (* 2 (rate-limiter-window-seconds limiter)))))
      (loop for client-id being the hash-keys of (rate-limiter-clients limiter)
            using (hash-value bucket)
            when (and (null (client-bucket-requests bucket))
                      (< (client-bucket-blocked-until bucket) cleanup-before))
              do (remhash client-id (rate-limiter-clients limiter))))))

;;; --- Timeout protection ---

(define-condition operation-cancelled (error)
  ((message :initarg :message :reader operation-cancelled-message))
  (:report (lambda (c s) (write-string (operation-cancelled-message c) s)))
  (:documentation
   "Signalled by WITH-TIMEOUT when the operation exceeds its time budget.
    Deliberately a distinct subclass of ERROR so callers can distinguish a
    cancellation from ordinary errors — and so broad (error (c) ...) catchers
    that should NOT swallow cancellations can specifically re-raise this type."))

(defmacro with-timeout ((seconds &key (error-message "Operation timed out")) &body body)
  "Execute BODY with a timeout of SECONDS.
Signals OPERATION-CANCELLED if the budget is exceeded.
Uses BT:INTERRUPT-THREAD to deliver the cancellation.  Callers that catch
broad ERROR conditions (e.g. SSE event dispatch) must re-raise
OPERATION-CANCELLED rather than swallowing it.

The watchdog checks a cancellation flag before firing so that a failed
BT:DESTROY-THREAD (which is silently swallowed) cannot cause a stale
watchdog to interrupt a later, unrelated operation on the same thread."
  (let ((thread (gensym "THREAD"))
        (result (gensym "RESULT"))
        (condition (gensym "CONDITION"))
        (cancelled (gensym "CANCELLED"))
        (timeout-thread (gensym "TIMEOUT-THREAD")))
    `(let ((,result nil)
           (,condition nil)
           (,cancelled (list nil))
           (,thread (bt:current-thread)))
       (let ((,timeout-thread
              (bt:make-thread
               (lambda ()
                 (sleep ,seconds)
                 (unless (car ,cancelled)
                   (when (bt:thread-alive-p ,thread)
                     (bt:interrupt-thread
                      ,thread
                      (lambda ()
                        (error 'operation-cancelled :message ,error-message))))))
               :name "timeout-watchdog")))
         (unwind-protect
              (handler-case
                  (setf ,result (progn ,@body))
                (error (c) (setf ,condition c)))
           (setf (car ,cancelled) t)
           (when (bt:thread-alive-p ,timeout-thread)
             (ignore-errors (bt:destroy-thread ,timeout-thread))))
         (if ,condition
             (error ,condition)
             ,result)))))

;;; --- Configuration helpers ---

(defun get-retry-config (service)
  "Get retry configuration for SERVICE from config file."
  (list :max-retries (crichton/config:config-section-get service :max-retries *default-max-retries*)
        :backoff-base (crichton/config:config-section-get service :backoff-base *default-backoff-base*)
        :max-backoff (crichton/config:config-section-get service :max-backoff *default-max-backoff*)))