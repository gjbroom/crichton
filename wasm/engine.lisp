;;;; wasm/engine.lisp
;;;;
;;;; Higher-level Lisp interface to wasmtime.
;;;; Wraps the CFFI bindings with proper resource management.
;;;; Includes host function support via the linker API.

(in-package #:crichton/wasm)

(declaim (notinline crichton/runner:skill-context-id))

;;; --- Error handling ---

(defun wasmtime-error-to-string (err)
  "Convert a wasmtime_error_t* to a Lisp string, then delete the error."
  (cffi:with-foreign-object (msg '(:struct wasm-byte-vec))
    (wasmtime-error-message err msg)
    (let* ((size (cffi:foreign-slot-value msg '(:struct wasm-byte-vec) 'size))
           (data (cffi:foreign-slot-value msg '(:struct wasm-byte-vec) 'data))
           (str (cffi:foreign-string-to-lisp data :count size)))
      (wasm-byte-vec-delete msg)
      (wasmtime-error-delete err)
      str)))

(defun trap-to-string (trap)
  "Convert a wasm_trap_t* to a Lisp string, then delete the trap."
  (cffi:with-foreign-object (msg '(:struct wasm-byte-vec))
    (wasm-trap-message trap msg)
    (let* ((size (cffi:foreign-slot-value msg '(:struct wasm-byte-vec) 'size))
           (data (cffi:foreign-slot-value msg '(:struct wasm-byte-vec) 'data))
           (str (cffi:foreign-string-to-lisp data :count size)))
      (wasm-byte-vec-delete msg)
      (wasm-trap-delete trap)
      str)))

(defun check-wasmtime-error (err &optional trap-ptr context)
  "Signal an error if ERR is non-null. Also checks TRAP-PTR if provided."
  (declare (ignore context))
  (unless (cffi:null-pointer-p err)
    (error "Wasmtime error: ~A" (wasmtime-error-to-string err)))
  (when (and trap-ptr (not (cffi:null-pointer-p (cffi:mem-ref trap-ptr :pointer))))
    (error "WASM trap: ~A"
           (trap-to-string (cffi:mem-ref trap-ptr :pointer)))))

;;; --- WAT compilation ---

(defun wat-to-wasm (wat-string)
  "Compile WAT text to WASM binary bytes. Returns foreign pointer and length.
   Caller must free the result with wasm-byte-vec-delete."
  (ensure-wasmtime-loaded)
  (cffi:with-foreign-object (wasm-bytes '(:struct wasm-byte-vec))
    (let ((err (wasmtime-wat2wasm wat-string (length wat-string) wasm-bytes)))
      (unless (cffi:null-pointer-p err)
        (error "WAT parse error: ~A" (wasmtime-error-to-string err)))
      (let ((size (cffi:foreign-slot-value wasm-bytes '(:struct wasm-byte-vec) 'size))
            (data (cffi:foreign-slot-value wasm-bytes '(:struct wasm-byte-vec) 'data)))
        (values data size wasm-bytes)))))

;;; --- Continuation-passing WASM lifecycle helpers ---

(defun call-with-wasm-bytes (source-type source k)
  "Convert SOURCE to a foreign byte range and call K with (ptr len cleanup-fn).
SOURCE-TYPE is :WAT (a string) or :WASM (an octet vector).  The foreign memory
is only valid within K's dynamic extent."
  (ecase source-type
    (:wat
     (cffi:with-foreign-object (wasm-bytes '(:struct wasm-byte-vec))
       (let ((wat-err (wasmtime-wat2wasm source (length source) wasm-bytes)))
         (unless (cffi:null-pointer-p wat-err)
           (error "WAT parse error: ~A" (wasmtime-error-to-string wat-err)))
         (unwind-protect
              (let ((data (cffi:foreign-slot-value
                           wasm-bytes '(:struct wasm-byte-vec) 'data))
                    (size (cffi:foreign-slot-value
                           wasm-bytes '(:struct wasm-byte-vec) 'size)))
                (funcall k data size))
           (wasm-byte-vec-delete wasm-bytes)))))
    (:wasm
     (let ((len (length source)))
       (cffi:with-foreign-object (wasm-ptr :uint8 len)
         (loop for i below len
               do (setf (cffi:mem-aref wasm-ptr :uint8 i)
                        (aref source i)))
         (funcall k wasm-ptr len))))))

(defun call-with-compiled-module (engine ptr len k)
  "Compile WASM bytes at foreign PTR (LEN bytes) into a module, call K with it.
Deletes the module on unwind."
  (let ((module nil))
    (unwind-protect
         (cffi:with-foreign-object (module-ptr :pointer)
           (let ((mod-err (wasmtime-module-new engine ptr len module-ptr)))
             (unless (cffi:null-pointer-p mod-err)
               (error "Module compile error: ~A"
                      (wasmtime-error-to-string mod-err)))
             (setf module (cffi:mem-ref module-ptr :pointer))
             (funcall k module)))
      (when module (wasmtime-module-delete module)))))

(defun call-with-instantiated-instance (context module k &key linker)
  "Instantiate MODULE and call K with the instance struct pointer.
When LINKER is provided, uses wasmtime-linker-instantiate; otherwise uses
wasmtime-instance-new with no imports."
  (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                              (trap-ptr :pointer))
    (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
    (let ((err (if linker
                   (wasmtime-linker-instantiate linker context module
                                                instance trap-ptr)
                   (wasmtime-instance-new context module
                                          (cffi:null-pointer) 0
                                          instance trap-ptr))))
      (check-wasmtime-error err trap-ptr "instantiate"))
    (funcall k context instance)))

(defun call-with-wasm-environment (source-type source fn
                                   &key host-fn-registrar)
  "Set up a complete WASM environment and call FN with (context instance).
SOURCE-TYPE is :WAT or :WASM.  When HOST-FN-REGISTRAR is provided, it is
called with (linker engine) to register host functions before instantiation.
Handles engine/store/linker/module creation and full cleanup on unwind."
  (ensure-wasmtime-loaded)
  (when host-fn-registrar (assert-wasmtime-abi))
  (let ((engine (wasm-engine-new))
        (store nil)
        (linker nil)
        (functypes nil))
    (when (cffi:null-pointer-p engine)
      (error "Failed to create wasmtime engine"))
    (unwind-protect
         (progn
           (setf store (wasmtime-store-new engine
                                           (cffi:null-pointer)
                                           (cffi:null-pointer)))
           (when (cffi:null-pointer-p store)
             (error "Failed to create wasmtime store"))
           (when host-fn-registrar
             (setf linker (wasmtime-linker-new engine))
             (when (cffi:null-pointer-p linker)
               (error "Failed to create wasmtime linker"))
             (setf functypes (funcall host-fn-registrar linker engine)))
           (let ((context (wasmtime-store-context store)))
             (call-with-wasm-bytes source-type source
               (lambda (ptr len)
                 (call-with-compiled-module engine ptr len
                   (lambda (module)
                     (call-with-instantiated-instance context module fn
                       :linker linker)))))))
      (dolist (ft functypes)
        (when ft (wasm-functype-delete ft)))
      (when linker (wasmtime-linker-delete linker))
      (when store (wasmtime-store-delete store))
      (wasm-engine-delete engine))))

;;; --- Simple module execution (no imports) ---

(defmacro with-wasm-simple-module ((context instance) wat-string &body body)
  "Set up a WASM environment with no imports and execute BODY.
Compiles WAT-STRING, instantiates the module with no imports, and binds
CONTEXT and INSTANCE for use in BODY.  Handles engine/store/module lifecycle
and full cleanup on unwind."
  `(call-with-wasm-environment :wat ,wat-string
     (lambda (,context ,instance) ,@body)))

(defun run-wasm-module (wat-string export-name)
  "Compile WAT, instantiate with no imports, call the named export.
   Expects the export to return a single i32. Returns the i32 value."
  (with-wasm-simple-module (context instance) wat-string
    (call-export-by-name context instance export-name nil 1)))

;;; --- Functype construction ---

(defun make-functype (param-kinds result-kinds)
  "Build a wasm_functype_t* from lists of kind constants (e.g. +wasm-i32+).
   Caller must eventually call wasm-functype-delete on the result.
   Note: wasm_valtype_vec_new takes ownership of the data array,
   so we must heap-allocate it (not stack-allocate with with-foreign-object)."
  (let ((nparams (length param-kinds))
        (nresults (length result-kinds)))
    (cffi:with-foreign-objects ((params-vec '(:struct wasm-valtype-vec))
                                (results-vec '(:struct wasm-valtype-vec)))
      (if (zerop nparams)
          (wasm-valtype-vec-new-empty params-vec)
          (let ((param-arr (cffi:foreign-alloc :pointer :count nparams)))
            (loop for kind in param-kinds
                  for i from 0
                  do (setf (cffi:mem-aref param-arr :pointer i)
                           (wasm-valtype-new kind)))
            (wasm-valtype-vec-new params-vec nparams param-arr)))
      (if (zerop nresults)
          (wasm-valtype-vec-new-empty results-vec)
          (let ((result-arr (cffi:foreign-alloc :pointer :count nresults)))
            (loop for kind in result-kinds
                  for i from 0
                  do (setf (cffi:mem-aref result-arr :pointer i)
                           (wasm-valtype-new kind)))
            (wasm-valtype-vec-new results-vec nresults result-arr)))
      (wasm-functype-new params-vec results-vec))))

;;; --- defhost-callback macro ---

(defmacro defhost-callback (name arg-bindings &body body)
  "Define a CFFI callback and a testable -impl function for a WASM host
function.  ARG-BINDINGS is a list of symbols bound to successive i32 args.
If the first form in BODY is a string it becomes the docstring for NAME-impl.
Generates:
  (defun NAME-impl (caller results nresults ARG-BINDINGS...) ...)
  (cffi:defcallback NAME ...)
The -impl function contains the real logic and is callable from Lisp tests.
The callback is a thin wrapper that validates nargs, extracts i32 values,
delegates to -impl, and catches errors."
  (let* ((docstring (when (and body (stringp (first body))) (first body)))
         (real-body (if docstring (rest body) body))
         (nargs-required (length arg-bindings))
         (impl-name (intern (format nil "~A-IMPL" (symbol-name name)))))
    `(progn
       (defun ,impl-name (caller results nresults ,@arg-bindings)
         ,@(when docstring (list docstring))
         (declare (ignorable caller results nresults))
         (block ,name
           ,@real-body))
       (cffi:defcallback ,name :pointer
           ((env :pointer) (caller :pointer)
            (args :pointer) (nargs :size)
            (results :pointer) (nresults :size))
         (declare (ignore env))
         (handler-case
             (progn
               (unless (>= nargs ,nargs-required)
                 (log:debug "~A: expected nargs >= ~D, got ~A"
                            ',name ,nargs-required nargs)
                 (return-from ,name (cffi:null-pointer)))
               (let* (,@(loop for sym in arg-bindings
                              for i from 0
                              collect `(,sym (wasmtime-val-i32
                                             (cffi:mem-aptr args
                                                            '(:struct wasmtime-val)
                                                            ,i)))))
                 (,impl-name caller results nresults ,@arg-bindings)))
           (error (c)
             (log:error "Host ~A callback error: ~A" ',name c)))
         (cffi:null-pointer)))))

;;; --- Host function: log ---
;;;
;;; WASM signature: (i32 level, i32 ptr, i32 len) -> ()
;;; Reads a UTF-8 string from WASM linear memory and logs it via log4cl.

(defun read-wasm-string (caller ptr len)
  "Read LEN bytes from WASM linear memory at offset PTR using CALLER.
    Returns a Lisp string. Includes bounds validation and error handling.
    Returns empty string if bounds check fails."
  (handler-case
      (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
        (unless (wasmtime-caller-export-get caller "memory" 6 ext)
          (error "WASM module does not export 'memory'"))
        (let* ((mem-ptr (wasmtime-extern-memory-ptr ext))
               (ctx (wasmtime-caller-context caller))
               (base (wasmtime-memory-data ctx mem-ptr))
               (mem-size (wasmtime-memory-data-size ctx mem-ptr)))
          ;; Validate base pointer is not null
          (when (cffi:null-pointer-p base)
            (log:error "read-wasm-string: base pointer is null")
            (return-from read-wasm-string ""))
          ;; Validate bounds: ptr + len <= mem-size
          (when (> (+ ptr len) mem-size)
            (log:error "read-wasm-string: out of bounds - offset ~A + len ~A > size ~A"
                       ptr len mem-size)
            (return-from read-wasm-string ""))
          ;; Validate ptr is non-negative
          (when (< ptr 0)
            (log:error "read-wasm-string: negative offset ~A" ptr)
            (return-from read-wasm-string ""))
          ;; Validate len is non-negative
          (when (< len 0)
            (log:error "read-wasm-string: negative length ~A" len)
            (return-from read-wasm-string ""))
          ;; Perform safe memory read with error handling
          (cffi:foreign-string-to-lisp (cffi:inc-pointer base ptr)
                                       :count len
                                       :encoding :utf-8)))
    (error (c)
      (log:error "read-wasm-string: memory access violation: ~A" c)
      "")))

(defhost-callback host-log-callback (level ptr len)
  "WASM host: log(level, ptr, len) → void.
Reads a UTF-8 string from linear memory and emits it via log4cl.
Levels: 0=debug 1=info 2=warn 3=error.  Validates all arguments before reading."
  ;; Validate level is a reasonable integer (0-3 standard, but be permissive)
  (unless (and (>= level 0) (<= level 255))
    (log:debug "host-log-callback: level ~A out of range [0, 255]" level)
    (return-from host-log-callback (cffi:null-pointer)))
  ;; Validate ptr and len: both should be non-negative and reasonable
  ;; Negative values (from signed i32) are invalid memory offsets
  (unless (>= ptr 0)
    (log:debug "host-log-callback: negative ptr ~A" ptr)
    (return-from host-log-callback (cffi:null-pointer)))
  (unless (>= len 0)
    (log:debug "host-log-callback: negative len ~A" len)
    (return-from host-log-callback (cffi:null-pointer)))
  ;; Sanity cap: len > 1MB indicates likely corruption
  (when (> len 1048576)
    (log:debug "host-log-callback: len ~A exceeds 1MB cap" len)
    (return-from host-log-callback (cffi:null-pointer)))
  ;; Read string from WASM memory and log
  (let ((msg (read-wasm-string caller ptr len)))
    (case level
      (0 (log:debug "~A" msg))
      (1 (log:info  "~A" msg))
      (2 (log:warn  "~A" msg))
      (3 (log:error "~A" msg))
      (t (log:info  "[level=~A] ~A" level msg)))))

;;; --- Helper: register a host function in the linker ---

(defun define-host-fn (linker engine functype module-name module-name-len
                       func-name func-name-len callback)
  "Register a host function in the linker. Signals error if it fails."
  (declare (ignore engine))
  (let ((def-err (wasmtime-linker-define-func
                  linker
                  module-name module-name-len
                  func-name func-name-len
                  functype callback
                  (cffi:null-pointer)
                  (cffi:null-pointer))))
    (unless (cffi:null-pointer-p def-err)
      (error "Linker define error: ~A" (wasmtime-error-to-string def-err)))))

;;; --- Standard host function registration ---

(defun register-standard-host-fns (linker engine)
  "Register all 8 standard host functions in LINKER.  Returns a list of
functype pointers that the caller must delete with wasm-functype-delete."
  (let* ((log-ft (make-functype (list +wasm-i32+ +wasm-i32+ +wasm-i32+) '()))
         (kv-get-ft (make-functype (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
         (kv-set-ft (make-functype (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                   (list +wasm-i32+)))
         (kv-delete-ft (make-functype (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
         (kv-exists-ft (make-functype (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
         (kv-list-ft (make-functype (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
         (http-ft (make-functype (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+
                                       +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                 (list +wasm-i32+)))
         (secret-ft (make-functype (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+))))
    (define-host-fn linker engine log-ft
                    "env" 3 "log" 3
                    (cffi:callback host-log-callback))
    (define-host-fn linker engine kv-get-ft
                    "env" 3 "kv_get" 6
                    (cffi:callback host-kv-get-callback))
    (define-host-fn linker engine kv-set-ft
                    "env" 3 "kv_set" 6
                    (cffi:callback host-kv-set-callback))
    (define-host-fn linker engine kv-delete-ft
                    "env" 3 "kv_delete" 9
                    (cffi:callback host-kv-delete-callback))
    (define-host-fn linker engine kv-exists-ft
                    "env" 3 "kv_exists" 9
                    (cffi:callback host-kv-exists-callback))
    (define-host-fn linker engine kv-list-ft
                    "env" 3 "kv_list" 7
                    (cffi:callback host-kv-list-callback))
    (define-host-fn linker engine http-ft
                    "env" 3 "http_request" 12
                    (cffi:callback host-http-request-callback))
    (define-host-fn linker engine secret-ft
                    "env" 3 "get_secret" 10
                    (cffi:callback host-get-secret-callback))
    (list log-ft kv-get-ft kv-set-ft kv-delete-ft
          kv-exists-ft kv-list-ft http-ft secret-ft)))

;;; --- WASM host environment macro ---

(defmacro with-wasm-host-environment ((context instance) (source-type source-form)
                                      &body body)
  "Set up a complete WASM host environment and execute BODY.  SOURCE-TYPE is
either :WAT (a WAT string) or :WASM (a WASM byte vector).  Binds CONTEXT and
INSTANCE for use in BODY.  Handles engine/store/linker/module creation, host
function registration, instantiation, and full cleanup on unwind."
  `(call-with-wasm-environment ,source-type ,source-form
     (lambda (,context ,instance) ,@body)
     :host-fn-registrar #'register-standard-host-fns))

;;; --- Linker-based module execution (with host functions) ---

(defun run-wasm-with-host-fns (wat-string export-name
                                &key (args nil) (nresults 1))
  "Compile WAT, register host functions via linker, instantiate, call export.
ARGS is a list of i32 values to pass.  Returns i32 result (or NIL if nresults=0)."
  (with-wasm-host-environment (context instance) (:wat wat-string)
    (call-export-by-name context instance export-name args nresults)))

(defun run-wasm-bytes-with-host-fns (wasm-bytes export-name
                                      &key (args nil) (nresults 1))
  "Like run-wasm-with-host-fns but accepts pre-compiled WASM bytes (octet vector)
instead of a WAT string.  Returns i32 result (or NIL if nresults=0)."
  (with-wasm-host-environment (context instance) (:wasm wasm-bytes)
    (call-export-by-name context instance export-name args nresults)))

;;; --- Host function: kv_get ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len) -> (i32)
;;; Returns: 0=found, -1=not found, -2=error

(defhost-callback host-kv-get-callback (key-ptr key-len)
  "WASM host: kv_get(key_ptr, key_len) → i32.
Reads key from linear memory, looks up the value in the skill-scoped KV store.
Returns 0=found, -1=not found, -2=error."
  (let ((key (read-wasm-string caller key-ptr key-len)))
    (handler-case
        (let* ((ctx (crichton/runner:current-skill-context))
               (skill-id (crichton/runner:skill-context-id ctx))
               (value (crichton/skills:kv-get skill-id key)))
          (if value
              (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 0)
              (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1)))
      (error (c)
        (log:error "kv_get error: ~A" c)
        (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -2)))))

;;; --- Host function: kv_set ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len, i32 val_ptr, i32 val_len) -> (i32)
;;; Returns: 0=success, -1=quota exceeded, -2=other error

(defhost-callback host-kv-set-callback (key-ptr key-len val-ptr val-len)
  "WASM host: kv_set(key_ptr, key_len, val_ptr, val_len) → i32.
Reads key and value strings from linear memory, writes to skill-scoped KV store.
Returns 0=success, -1=quota exceeded, -2=other error."
  (let ((key (read-wasm-string caller key-ptr key-len))
        (val (read-wasm-string caller val-ptr val-len)))
    (handler-case
        (let* ((ctx (crichton/runner:current-skill-context))
               (skill-id (crichton/runner:skill-context-id ctx)))
          (crichton/skills:kv-set skill-id key val)
          (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 0))
      (crichton/skills:kv-quota-exceeded (c)
        (log:warn "kv_set quota exceeded: ~A" c)
        (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1))
      (error (c)
        (log:error "kv_set error: ~A" c)
        (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -2)))))

;;; --- Host function: kv_delete ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len) -> (i32)
;;; Returns: 0=deleted, 1=not found, -1=error

(defhost-callback host-kv-delete-callback (key-ptr key-len)
  "WASM host: kv_delete(key_ptr, key_len) → i32.
Removes the key from the skill-scoped KV store.
Returns 0=deleted, 1=key not found, -1=error."
  (let ((key (read-wasm-string caller key-ptr key-len)))
    (handler-case
        (let* ((ctx (crichton/runner:current-skill-context))
               (skill-id (crichton/runner:skill-context-id ctx))
               (deleted (crichton/skills:kv-delete skill-id key)))
          (if deleted
              (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 0)
              (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 1)))
      (error (c)
        (log:error "kv_delete error: ~A" c)
        (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1)))))

;;; --- Host function: kv_exists ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len) -> (i32)
;;; Returns: 1=exists, 0=not exists, -1=error

(defhost-callback host-kv-exists-callback (key-ptr key-len)
  "WASM host: kv_exists(key_ptr, key_len) → i32.
Checks whether the key is present in the skill-scoped KV store.
Returns 1=exists, 0=not found, -1=error."
  (let ((key (read-wasm-string caller key-ptr key-len)))
    (handler-case
        (let* ((ctx (crichton/runner:current-skill-context))
               (skill-id (crichton/runner:skill-context-id ctx))
               (exists (crichton/skills:kv-exists-p skill-id key)))
          (if exists
              (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 1)
              (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 0)))
      (error (c)
        (log:error "kv_exists error: ~A" c)
        (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1)))))

;;; --- Host function: kv_list ---
;;;
;;; WASM signature: (i32 prefix_ptr, i32 prefix_len) -> (i32)
;;; Returns: count of matching keys, or -1 on error

(defhost-callback host-kv-list-callback (prefix-ptr prefix-len)
  "WASM host: kv_list(prefix_ptr, prefix_len) → i32.
Lists keys in the skill-scoped KV store matching the given prefix.
An empty prefix (prefix_len=0) returns all keys.
Returns the count of matching keys, or -1 on error."
  (let ((prefix (if (zerop prefix-len)
                    nil
                    (read-wasm-string caller prefix-ptr prefix-len))))
    (handler-case
        (let* ((ctx (crichton/runner:current-skill-context))
               (skill-id (crichton/runner:skill-context-id ctx))
               (keys (crichton/skills:kv-list skill-id prefix)))
          (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0))
                (length keys)))
      (error (c)
        (log:error "kv_list error: ~A" c)
        (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1)))))

;;; --- Host function: http_request ---
;;;
;;; WASM signature: (i32 method_ptr, i32 method_len, i32 url_ptr, i32 url_len,
;;;                  i32 headers_ptr, i32 headers_len, i32 body_ptr, i32 body_len)
;;;                -> (i32 status_code or -1 on error)

(defhost-callback host-http-request-callback
    (method-ptr method-len url-ptr url-len
     headers-ptr headers-len body-ptr body-len)
  "WASM host: http_request(method, url, headers, body) → i32 status code.
All four arguments are (ptr, len) pairs pointing into linear memory.
Not yet fully implemented — currently returns 403 for all requests pending
allowlist validation.  Will eventually enforce network egress policy."
  (let ((method (read-wasm-string caller method-ptr method-len))
        (url (read-wasm-string caller url-ptr url-len))
        (headers (read-wasm-string caller headers-ptr headers-len))
        (body (read-wasm-string caller body-ptr body-len)))
    ;; TODO: validate against allowlist and make HTTP request
    (declare (ignore method url headers body))
    (log:debug "http_request called (not yet implemented)")
    ;; Return 403 (forbidden) for now
    (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 403)))

;;; --- Host function: get_secret ---
;;;
;;; WASM signature: (i32 name_ptr, i32 name_len) -> (i32 value_ptr or -1 if not found)

(defhost-callback host-get-secret-callback (name-ptr name-len)
  "WASM host: get_secret(name_ptr, name_len) → i32 value_ptr or -1.
Resolves a named credential for the running skill.
Not yet fully implemented — currently returns -1 pending RPC credential
resolution.  Will eventually call back to the daemon's credential mediator."
  (let ((name (read-wasm-string caller name-ptr name-len)))
    ;; TODO: contact daemon via RPC to resolve credential
    (declare (ignore name))
    (log:debug "get_secret called (not yet implemented)")
    ;; Return -1 (not found) for now
    (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1)))

;;; --- JSON-through-memory ABI ---
;;;
;;; For skills that need structured I/O (e.g. RSS filter), the host:
;;;   1. Serializes a Lisp structure to JSON bytes
;;;   2. Calls the WASM module's exported alloc(size) -> ptr
;;;   3. Copies JSON bytes into WASM linear memory at the returned ptr
;;;   4. Allocates an output buffer via alloc
;;;   5. Calls the target function with (params_ptr, params_len, out_ptr, out_len) -> i32
;;;   6. Reads JSON result from the output buffer
;;;   7. Calls exported dealloc(ptr, size) to free both buffers
;;;   8. Parses the JSON result back to a Lisp structure

(defconstant +json-output-buffer-size+ 65536
  "Default output buffer size for JSON ABI calls (64 KiB).")

;;; NOTE on wasmtime value lifetimes:
;;; wasmtime_func_t and wasmtime_memory_t are value types embedded inside
;;; wasmtime_extern_t.  Pointers to them are only valid while the containing
;;; extern struct is alive.  Never cache such pointers across scopes — always
;;; look up exports and use them within the same with-foreign-object scope.

(defun with-wasm-memory (context instance fn)
  "Look up WASM linear memory and call FN with (base mem-size).
   The base pointer is only valid within FN's dynamic extent."
  (cffi:with-foreign-object (mem-ext '(:struct wasmtime-extern))
    (unless (wasmtime-instance-export-get context instance "memory" 6 mem-ext)
      (error "WASM module does not export 'memory'"))
    (let* ((mem-ptr (wasmtime-extern-memory-ptr mem-ext))
           (base (wasmtime-memory-data context mem-ptr))
           (mem-size (wasmtime-memory-data-size context mem-ptr)))
      (when (cffi:null-pointer-p base)
        (error "WASM memory base pointer is null"))
      (funcall fn base mem-size))))

(defun call-export-by-name (context instance name args nresults)
  "Look up export NAME, call it with i32 ARGS, return i32 result.
   The export handle is kept alive for the duration of the call."
  (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
    (unless (wasmtime-instance-export-get
             context instance name (length name) ext)
      (error "Export ~S not found" name))
    (let* ((func-ptr (wasmtime-extern-func-ptr ext))
           (nargs (length args)))
      (cffi:with-foreign-objects ((wasm-args '(:struct wasmtime-val) (max nargs 1))
                                  (wasm-results '(:struct wasmtime-val) (max nresults 1))
                                  (call-trap :pointer))
        (loop for val in args
              for i from 0
              do (setf (wasmtime-val-i32
                        (cffi:mem-aptr wasm-args '(:struct wasmtime-val) i))
                       val))
        (setf (cffi:mem-ref call-trap :pointer) (cffi:null-pointer))
        (let ((call-err (wasmtime-func-call
                         context func-ptr
                         (if (zerop nargs) (cffi:null-pointer) wasm-args)
                         nargs
                         (if (zerop nresults) (cffi:null-pointer) wasm-results)
                         nresults
                         call-trap)))
          (check-wasmtime-error call-err call-trap name)
          (if (zerop nresults)
              nil
              (wasmtime-val-i32 wasm-results)))))))

(defun write-bytes-to-wasm-memory (context instance bytes wasm-ptr)
  "Write BYTES (an octet vector) into WASM linear memory at WASM-PTR offset."
  (with-wasm-memory context instance
    (lambda (base mem-size)
      (let ((len (length bytes)))
        (when (> (+ wasm-ptr len) mem-size)
          (error "Write would exceed WASM memory: offset ~D + len ~D > size ~D"
                 wasm-ptr len mem-size))
        (loop for i below len
              do (setf (cffi:mem-aref base :uint8 (+ wasm-ptr i))
                       (aref bytes i)))))))

(defun read-bytes-from-wasm-memory (context instance wasm-ptr len)
  "Read LEN bytes from WASM linear memory at WASM-PTR offset.
   Returns a Lisp string (UTF-8 decoded)."
  (with-wasm-memory context instance
    (lambda (base mem-size)
      (when (> (+ wasm-ptr len) mem-size)
        (error "Read would exceed WASM memory: offset ~D + len ~D > size ~D"
               wasm-ptr len mem-size))
      (cffi:foreign-string-to-lisp (cffi:inc-pointer base wasm-ptr)
                                   :count len :encoding :utf-8))))

(defun plist-like-p (list)
  "Return T if LIST looks like a plist: non-empty, even length, keyword first elements."
  (and (consp list)
       (keywordp (first list))
       (evenp (length list))))

(defun normalize-for-json (value)
  "Recursively normalize VALUE for JSON serialization.
   Converts plists to hash-tables (with downcased, underscored string keys)
   so shasht serializes them as JSON objects instead of arrays."
  (cond
    ((hash-table-p value)
     (let ((ht (make-hash-table :test #'equal)))
       (maphash (lambda (k v)
                  (setf (gethash k ht) (normalize-for-json v)))
                value)
       ht))
    ((plist-like-p value)
     (let ((ht (make-hash-table :test #'equal)))
       (loop for (k v) on value by #'cddr
             do (setf (gethash (substitute #\_ #\-
                                           (string-downcase (symbol-name k)))
                               ht)
                      (normalize-for-json v)))
       ht))
    ((and (listp value) (not (null value)))
     (mapcar #'normalize-for-json value))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'normalize-for-json value))
    (t value)))

(defun json-call-with-instance (context instance export-name params
                                 &key (output-buffer-size +json-output-buffer-size+))
  "Execute a JSON ABI call on an already-instantiated WASM module.
   PARAMS is a Lisp data structure (hash-table, plist, etc.) to serialize as
   JSON.  Returns the parsed JSON result.  All export handles are looked up
   fresh for each call to avoid dangling pointers."
  (let* ((normalized (normalize-for-json params))
         (json-string (with-output-to-string (s)
                        (shasht:write-json normalized s)))
         (json-bytes (sb-ext:string-to-octets json-string :external-format :utf-8))
         (json-len (length json-bytes))
         (in-ptr 0)
         (out-ptr 0))
    (unwind-protect
         (progn
           ;; Allocate input buffer and write JSON bytes
           (let ((ptr (call-export-by-name context instance "alloc"
                                           (list json-len) 1)))
             (when (zerop ptr)
               (error "WASM alloc(~D) returned null pointer" json-len))
             (setf in-ptr ptr))
           (write-bytes-to-wasm-memory context instance json-bytes in-ptr)
           ;; Allocate output buffer
           (let ((ptr (call-export-by-name context instance "alloc"
                                           (list output-buffer-size) 1)))
             (when (zerop ptr)
               (error "WASM alloc(~D) returned null pointer" output-buffer-size))
             (setf out-ptr ptr))
           ;; Call the target function
           (let ((rc (call-export-by-name context instance export-name
                                          (list in-ptr json-len
                                                out-ptr output-buffer-size)
                                          1)))
             (unless (zerop rc)
               (error "WASM JSON function ~S returned non-zero: ~D" export-name rc))
             ;; Read and parse result JSON
             (let ((result-str (read-bytes-from-wasm-memory
                                context instance out-ptr output-buffer-size)))
               (let ((trimmed (string-right-trim '(#\Nul) result-str)))
                 (if (zerop (length trimmed))
                     nil
                     (shasht:read-json trimmed))))))
      ;; Cleanup: dealloc both buffers (ignore errors)
      (when (plusp in-ptr)
        (handler-case
            (call-export-by-name context instance "dealloc"
                                 (list in-ptr json-len) 0)
          (error (c) (log:warn "dealloc failed (non-fatal): ~A" c))))
      (when (plusp out-ptr)
        (handler-case
            (call-export-by-name context instance "dealloc"
                                 (list out-ptr output-buffer-size) 0)
          (error (c) (log:warn "dealloc failed (non-fatal): ~A" c)))))))

(defun run-wasm-json-call (wat-string export-name params
                           &key (output-buffer-size +json-output-buffer-size+))
  "Compile WAT, register host functions via linker, instantiate, and call
EXPORT-NAME using the JSON-through-memory ABI.  PARAMS is a Lisp data
structure to serialize as JSON input.  Returns the parsed JSON result."
  (with-wasm-host-environment (context instance) (:wat wat-string)
    (json-call-with-instance context instance export-name params
                             :output-buffer-size output-buffer-size)))

(defun run-wasm-bytes-json-call (wasm-bytes export-name params
                                 &key (output-buffer-size +json-output-buffer-size+))
  "Like run-wasm-json-call but accepts pre-compiled WASM bytes (octet vector)
instead of a WAT string."
  (with-wasm-host-environment (context instance) (:wasm wasm-bytes)
    (json-call-with-instance context instance export-name params
                             :output-buffer-size output-buffer-size)))

;;; --- Test WAT for host functions ---

(defparameter *test-host-log-wat*
  "(module
     (import \"env\" \"log\" (func $log (param i32 i32 i32)))
     (memory (export \"memory\") 1)
     (data (i32.const 0) \"hello from wasm\")
     (func (export \"main\") (result i32)
       (call $log
         (i32.const 1)   ;; level: info
         (i32.const 0)   ;; ptr: 0
         (i32.const 15)) ;; len: 15
       (i32.const 42)))"
  "WAT module that calls the host log function then returns 42.")
