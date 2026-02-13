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

;;; --- Simple module execution (no imports) ---

(defun run-wasm-module (wat-string export-name)
  "Compile WAT, instantiate with no imports, call the named export.
   Expects the export to return a single i32. Returns the i32 value."
  (ensure-wasmtime-loaded)
  (let ((engine (wasm-engine-new))
        (store nil)
        (module nil))
    (when (cffi:null-pointer-p engine)
      (error "Failed to create wasmtime engine"))
    (unwind-protect
         (progn
           (setf store (wasmtime-store-new engine (cffi:null-pointer) (cffi:null-pointer)))
           (when (cffi:null-pointer-p store)
             (error "Failed to create wasmtime store"))
           (let ((context (wasmtime-store-context store)))
             (cffi:with-foreign-object (wasm-bytes '(:struct wasm-byte-vec))
               (let ((wat-err (wasmtime-wat2wasm wat-string (length wat-string) wasm-bytes)))
                 (unless (cffi:null-pointer-p wat-err)
                   (error "WAT parse error: ~A" (wasmtime-error-to-string wat-err)))
                 (unwind-protect
                      (let ((wasm-data (cffi:foreign-slot-value wasm-bytes '(:struct wasm-byte-vec) 'data))
                            (wasm-size (cffi:foreign-slot-value wasm-bytes '(:struct wasm-byte-vec) 'size)))
                        (cffi:with-foreign-object (module-ptr :pointer)
                          (let ((mod-err (wasmtime-module-new engine wasm-data wasm-size module-ptr)))
                            (unless (cffi:null-pointer-p mod-err)
                              (error "Module compile error: ~A" (wasmtime-error-to-string mod-err)))
                            (setf module (cffi:mem-ref module-ptr :pointer))
                            (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                                                       (trap-ptr :pointer))
                              (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
                              (let ((inst-err (wasmtime-instance-new
                                              context module
                                              (cffi:null-pointer) 0
                                              instance trap-ptr)))
                                (check-wasmtime-error inst-err trap-ptr "instantiate"))
                              (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
                                (unless (wasmtime-instance-export-get
                                         context instance
                                         export-name (length export-name)
                                         ext)
                                  (error "Export ~S not found" export-name))
                                (let ((func-ptr (wasmtime-extern-func-ptr ext)))
                                  (cffi:with-foreign-objects ((results '(:struct wasmtime-val) 1)
                                                             (call-trap :pointer))
                                    (setf (cffi:mem-ref call-trap :pointer) (cffi:null-pointer))
                                    (let ((call-err (wasmtime-func-call
                                                     context func-ptr
                                                     (cffi:null-pointer) 0
                                                     results 1
                                                     call-trap)))
                                      (check-wasmtime-error call-err call-trap "func_call")
                                      (wasmtime-val-i32 results)))))))))
                   (wasm-byte-vec-delete wasm-bytes))))))
      (when module (wasmtime-module-delete module))
      (when store (wasmtime-store-delete store))
      (wasm-engine-delete engine))))

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

(cffi:defcallback host-log-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env results nresults))
  (handler-case
      (progn
        ;; Validate argument count
        (unless (>= nargs 3)
          (log:debug "host-log-callback: expected nargs >= 3, got ~A" nargs)
          (return-from host-log-callback (cffi:null-pointer)))
        
        ;; Extract and validate level (should be 0-3, allow larger values for extension)
        (let* ((level (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (ptr   (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (len   (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 2))))
          
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
          
          ;; Validate caller is non-null before calling read-wasm-string
          (when (cffi:null-pointer-p caller)
            (log:debug "host-log-callback: caller is null pointer")
            (return-from host-log-callback (cffi:null-pointer)))
          
          ;; Read string from WASM memory and log
          (let ((msg (read-wasm-string caller ptr len)))
            (case level
              (0 (log:debug "~A" msg))
              (1 (log:info  "~A" msg))
              (2 (log:warn  "~A" msg))
              (3 (log:error "~A" msg))
              (t (log:info  "[level=~A] ~A" level msg))))))
    (error (c)
      (log:error "Host log callback error: ~A" c)))
  (cffi:null-pointer))

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

;;; --- Linker-based module execution (with host functions) ---

(defun run-wasm-with-host-fns (wat-string export-name
                                &key (args nil) (nresults 1))
  "Compile WAT, register host functions via linker, instantiate, call export.
    ARGS is a list of i32 values to pass. Returns i32 result (or NIL if nresults=0)."
  (ensure-wasmtime-loaded)
  (assert-wasmtime-abi)
  (let ((engine (wasm-engine-new))
      (store nil)
      (module nil)
      (linker nil)
      (log-functype nil)
      (kv-get-functype nil)
      (kv-set-functype nil)
      (kv-delete-functype nil)
      (kv-exists-functype nil)
      (kv-list-functype nil)
      (http-functype nil)
      (secret-functype nil))
  (when (cffi:null-pointer-p engine)
    (error "Failed to create wasmtime engine"))
  (unwind-protect
       (progn
         (setf store (wasmtime-store-new engine (cffi:null-pointer) (cffi:null-pointer)))
         (when (cffi:null-pointer-p store)
           (error "Failed to create wasmtime store"))
         (setf linker (wasmtime-linker-new engine))
         (when (cffi:null-pointer-p linker)
           (error "Failed to create wasmtime linker"))
         (let ((context (wasmtime-store-context store)))
           ;; Register host functions
           ;; env.log: (i32, i32, i32) -> ()
           (setf log-functype (make-functype
                               (list +wasm-i32+ +wasm-i32+ +wasm-i32+)
                               '()))
           (define-host-fn linker engine log-functype
                          "env" 3 "log" 3
                          (cffi:callback host-log-callback))
           
           ;; env.kv_get: (i32, i32) -> (i32)
           (setf kv-get-functype (make-functype
                                  (list +wasm-i32+ +wasm-i32+)
                                  (list +wasm-i32+)))
           (define-host-fn linker engine kv-get-functype
                          "env" 3 "kv_get" 6
                          (cffi:callback host-kv-get-callback))
           
           ;; env.kv_set: (i32, i32, i32, i32) -> (i32)
           (setf kv-set-functype (make-functype
                                  (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                  (list +wasm-i32+)))
           (define-host-fn linker engine kv-set-functype
                          "env" 3 "kv_set" 6
                          (cffi:callback host-kv-set-callback))
           
           ;; env.kv_delete: (i32, i32) -> (i32)
           (setf kv-delete-functype (make-functype
                                     (list +wasm-i32+ +wasm-i32+)
                                     (list +wasm-i32+)))
           (define-host-fn linker engine kv-delete-functype
                          "env" 3 "kv_delete" 9
                          (cffi:callback host-kv-delete-callback))
           
           ;; env.kv_exists: (i32, i32) -> (i32)
           (setf kv-exists-functype (make-functype
                                     (list +wasm-i32+ +wasm-i32+)
                                     (list +wasm-i32+)))
           (define-host-fn linker engine kv-exists-functype
                          "env" 3 "kv_exists" 9
                          (cffi:callback host-kv-exists-callback))
           
           ;; env.kv_list: (i32, i32) -> (i32)
           (setf kv-list-functype (make-functype
                                   (list +wasm-i32+ +wasm-i32+)
                                   (list +wasm-i32+)))
           (define-host-fn linker engine kv-list-functype
                          "env" 3 "kv_list" 7
                          (cffi:callback host-kv-list-callback))
           
           ;; env.http_request: (i32×8) -> (i32)
           (setf http-functype (make-functype
                                (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+
                                      +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                (list +wasm-i32+)))
           (define-host-fn linker engine http-functype
                          "env" 3 "http_request" 12
                          (cffi:callback host-http-request-callback))
           
           ;; env.get_secret: (i32, i32) -> (i32)
           (setf secret-functype (make-functype
                                  (list +wasm-i32+ +wasm-i32+)
                                  (list +wasm-i32+)))
           (define-host-fn linker engine secret-functype
                          "env" 3 "get_secret" 10
                          (cffi:callback host-get-secret-callback))
           ;; Compile WAT → module
           (cffi:with-foreign-object (wasm-bytes '(:struct wasm-byte-vec))
               (let ((wat-err (wasmtime-wat2wasm wat-string (length wat-string) wasm-bytes)))
                 (unless (cffi:null-pointer-p wat-err)
                   (error "WAT parse error: ~A" (wasmtime-error-to-string wat-err)))
                 (unwind-protect
                      (let ((wasm-data (cffi:foreign-slot-value wasm-bytes '(:struct wasm-byte-vec) 'data))
                            (wasm-size (cffi:foreign-slot-value wasm-bytes '(:struct wasm-byte-vec) 'size)))
                        (cffi:with-foreign-object (module-ptr :pointer)
                          (let ((mod-err (wasmtime-module-new engine wasm-data wasm-size module-ptr)))
                            (unless (cffi:null-pointer-p mod-err)
                              (error "Module compile error: ~A" (wasmtime-error-to-string mod-err)))
                            (setf module (cffi:mem-ref module-ptr :pointer))
                            ;; Instantiate via linker
                            (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                                                       (trap-ptr :pointer))
                              (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
                              (let ((inst-err (wasmtime-linker-instantiate
                                              linker context module
                                              instance trap-ptr)))
                                (check-wasmtime-error inst-err trap-ptr "linker_instantiate"))
                              ;; Get and call the export
                              (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
                                (unless (wasmtime-instance-export-get
                                         context instance
                                         export-name (length export-name)
                                         ext)
                                  (error "Export ~S not found" export-name))
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
                                      (check-wasmtime-error call-err call-trap "func_call")
                                      (if (zerop nresults)
                                          nil
                                          (wasmtime-val-i32 wasm-results))))))))))
                                          (wasm-byte-vec-delete wasm-bytes))))))
                                          (when log-functype (wasm-functype-delete log-functype))
                                          (when kv-get-functype (wasm-functype-delete kv-get-functype))
                                          (when kv-set-functype (wasm-functype-delete kv-set-functype))
                                          (when kv-delete-functype (wasm-functype-delete kv-delete-functype))
                                          (when kv-exists-functype (wasm-functype-delete kv-exists-functype))
                                          (when kv-list-functype (wasm-functype-delete kv-list-functype))
                                          (when http-functype (wasm-functype-delete http-functype))
                                          (when secret-functype (wasm-functype-delete secret-functype))
                                          (when linker (wasmtime-linker-delete linker))
                                          (when module (wasmtime-module-delete module))
                                          (when store (wasmtime-store-delete store))
                                          (wasm-engine-delete engine))))

                                          ;;; --- Test WAT for host log function ---

                                          ;;; --- Binary WASM module execution (with host functions) ---

                                          (defun run-wasm-bytes-with-host-fns (wasm-bytes export-name
                                       &key (args nil) (nresults 1))
  "Like run-wasm-with-host-fns but accepts pre-compiled WASM bytes (octet vector)
    instead of a WAT string. Returns i32 result (or NIL if nresults=0)."
  (ensure-wasmtime-loaded)
  (assert-wasmtime-abi)
  (let ((engine (wasm-engine-new))
       (store nil)
       (module nil)
       (linker nil)
       (log-functype nil)
       (kv-get-functype nil)
       (kv-set-functype nil)
       (kv-delete-functype nil)
       (kv-exists-functype nil)
       (kv-list-functype nil)
       (http-functype nil)
       (secret-functype nil)
       (wasm-len (length wasm-bytes)))
   (when (cffi:null-pointer-p engine)
     (error "Failed to create wasmtime engine"))
   (unwind-protect
        (progn
          (setf store (wasmtime-store-new engine (cffi:null-pointer) (cffi:null-pointer)))
          (when (cffi:null-pointer-p store)
            (error "Failed to create wasmtime store"))
          (setf linker (wasmtime-linker-new engine))
          (when (cffi:null-pointer-p linker)
            (error "Failed to create wasmtime linker"))
          (let ((context (wasmtime-store-context store)))
            ;; Register host functions
            ;; env.log: (i32, i32, i32) -> ()
            (setf log-functype (make-functype
                                (list +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                '()))
            (define-host-fn linker engine log-functype
                           "env" 3 "log" 3
                           (cffi:callback host-log-callback))
            
            ;; env.kv_get: (i32, i32) -> (i32)
            (setf kv-get-functype (make-functype
                                   (list +wasm-i32+ +wasm-i32+)
                                   (list +wasm-i32+)))
            (define-host-fn linker engine kv-get-functype
                           "env" 3 "kv_get" 6
                           (cffi:callback host-kv-get-callback))
            
            ;; env.kv_set: (i32, i32, i32, i32) -> (i32)
            (setf kv-set-functype (make-functype
                                   (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                   (list +wasm-i32+)))
            (define-host-fn linker engine kv-set-functype
                           "env" 3 "kv_set" 6
                           (cffi:callback host-kv-set-callback))
            
            ;; env.kv_delete: (i32, i32) -> (i32)
            (setf kv-delete-functype (make-functype
                                      (list +wasm-i32+ +wasm-i32+)
                                      (list +wasm-i32+)))
            (define-host-fn linker engine kv-delete-functype
                           "env" 3 "kv_delete" 9
                           (cffi:callback host-kv-delete-callback))
            
            ;; env.kv_exists: (i32, i32) -> (i32)
            (setf kv-exists-functype (make-functype
                                      (list +wasm-i32+ +wasm-i32+)
                                      (list +wasm-i32+)))
            (define-host-fn linker engine kv-exists-functype
                           "env" 3 "kv_exists" 9
                           (cffi:callback host-kv-exists-callback))
            
            ;; env.kv_list: (i32, i32) -> (i32)
            (setf kv-list-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+)
                                    (list +wasm-i32+)))
            (define-host-fn linker engine kv-list-functype
                           "env" 3 "kv_list" 7
                           (cffi:callback host-kv-list-callback))
            
            ;; env.http_request: (i32×8) -> (i32)
            (setf http-functype (make-functype
                                 (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+
                                       +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                 (list +wasm-i32+)))
            (define-host-fn linker engine http-functype
                           "env" 3 "http_request" 12
                           (cffi:callback host-http-request-callback))
            
            ;; env.get_secret: (i32, i32) -> (i32)
            (setf secret-functype (make-functype
                                   (list +wasm-i32+ +wasm-i32+)
                                   (list +wasm-i32+)))
            (define-host-fn linker engine secret-functype
                           "env" 3 "get_secret" 10
                           (cffi:callback host-get-secret-callback))
            ;; Copy WASM bytes to foreign memory and compile
             (cffi:with-foreign-object (wasm-ptr :uint8 wasm-len)
               (loop for i below wasm-len
                     do (setf (cffi:mem-aref wasm-ptr :uint8 i)
                              (aref wasm-bytes i)))
               (cffi:with-foreign-object (module-ptr :pointer)
                 (let ((mod-err (wasmtime-module-new engine wasm-ptr wasm-len module-ptr)))
                   (unless (cffi:null-pointer-p mod-err)
                     (error "Module compile error: ~A" (wasmtime-error-to-string mod-err)))
                   (setf module (cffi:mem-ref module-ptr :pointer))
                   ;; Instantiate via linker
                   (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                                              (trap-ptr :pointer))
                     (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
                     (let ((inst-err (wasmtime-linker-instantiate
                                     linker context module
                                     instance trap-ptr)))
                       (check-wasmtime-error inst-err trap-ptr "linker_instantiate"))
                     ;; Get and call the export
                     (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
                       (unless (wasmtime-instance-export-get
                                context instance
                                export-name (length export-name)
                                ext)
                         (error "Export ~S not found" export-name))
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
                             (check-wasmtime-error call-err call-trap "func_call")
                             (if (zerop nresults)
                                 nil
                                 (wasmtime-val-i32 wasm-results))))))))))))
                                 (when log-functype (wasm-functype-delete log-functype))
                                 (when kv-get-functype (wasm-functype-delete kv-get-functype))
                                 (when kv-set-functype (wasm-functype-delete kv-set-functype))
                                 (when kv-delete-functype (wasm-functype-delete kv-delete-functype))
                                 (when kv-exists-functype (wasm-functype-delete kv-exists-functype))
                                 (when kv-list-functype (wasm-functype-delete kv-list-functype))
                                 (when http-functype (wasm-functype-delete http-functype))
                                 (when secret-functype (wasm-functype-delete secret-functype))
                                 (when linker (wasmtime-linker-delete linker))
                                 (when module (wasmtime-module-delete module))
                                 (when store (wasmtime-store-delete store))
                                 (wasm-engine-delete engine))))

                                 ;;; --- Host function: kv_get ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len) -> (i32)
;;; Returns: 0=found, -1=not found, -2=error

(cffi:defcallback host-kv-get-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 2)
          (log:debug "host-kv-get-callback: expected nargs >= 2, got ~A" nargs)
          (return-from host-kv-get-callback (cffi:null-pointer)))
        
        (let* ((key-ptr (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (key-len (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (key (read-wasm-string caller key-ptr key-len)))
          
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
    (error (c)
      (log:error "Host kv-get callback error: ~A" c)))
  (cffi:null-pointer))

;;; --- Host function: kv_set ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len, i32 val_ptr, i32 val_len) -> (i32)
;;; Returns: 0=success, -1=quota exceeded, -2=other error

(cffi:defcallback host-kv-set-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 4)
          (log:debug "host-kv-set-callback: expected nargs >= 4, got ~A" nargs)
          (return-from host-kv-set-callback (cffi:null-pointer)))
        
        (let* ((key-ptr (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (key-len (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (val-ptr (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 2)))
               (val-len (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 3)))
               (key (read-wasm-string caller key-ptr key-len))
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
    (error (c)
      (log:error "Host kv-set callback error: ~A" c)))
  (cffi:null-pointer))

;;; --- Host function: kv_delete ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len) -> (i32)
;;; Returns: 0=deleted, 1=not found, -1=error

(cffi:defcallback host-kv-delete-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 2)
          (log:debug "host-kv-delete-callback: expected nargs >= 2, got ~A" nargs)
          (return-from host-kv-delete-callback (cffi:null-pointer)))
        
        (let* ((key-ptr (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (key-len (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (key (read-wasm-string caller key-ptr key-len)))
          
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
    (error (c)
      (log:error "Host kv-delete callback error: ~A" c)))
  (cffi:null-pointer))

;;; --- Host function: kv_exists ---
;;;
;;; WASM signature: (i32 key_ptr, i32 key_len) -> (i32)
;;; Returns: 1=exists, 0=not exists, -1=error

(cffi:defcallback host-kv-exists-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 2)
          (log:debug "host-kv-exists-callback: expected nargs >= 2, got ~A" nargs)
          (return-from host-kv-exists-callback (cffi:null-pointer)))
        
        (let* ((key-ptr (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (key-len (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (key (read-wasm-string caller key-ptr key-len)))
          
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
    (error (c)
      (log:error "Host kv-exists callback error: ~A" c)))
  (cffi:null-pointer))

;;; --- Host function: kv_list ---
;;;
;;; WASM signature: (i32 prefix_ptr, i32 prefix_len) -> (i32)
;;; Returns: count of matching keys, or -1 on error

(cffi:defcallback host-kv-list-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 2)
          (log:debug "host-kv-list-callback: expected nargs >= 2, got ~A" nargs)
          (return-from host-kv-list-callback (cffi:null-pointer)))
        
        (let* ((prefix-ptr (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (prefix-len (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (prefix (if (zerop prefix-len)
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
    (error (c)
      (log:error "Host kv-list callback error: ~A" c)))
  (cffi:null-pointer))

;;; --- Host function: http_request ---
;;;
;;; WASM signature: (i32 method_ptr, i32 method_len, i32 url_ptr, i32 url_len,
;;;                  i32 headers_ptr, i32 headers_len, i32 body_ptr, i32 body_len)
;;;                -> (i32 status_code or -1 on error)

(cffi:defcallback host-http-request-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 8)
          (log:debug "host-http-request-callback: expected nargs >= 8, got ~A" nargs)
          (return-from host-http-request-callback (cffi:null-pointer)))
        
        (let* ((method (read-wasm-string caller
                         (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0))
                         (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1))))
               (url (read-wasm-string caller
                     (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 2))
                     (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 3))))
               (headers (read-wasm-string caller
                          (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 4))
                          (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 5))))
               (body (read-wasm-string caller
                      (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 6))
                      (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 7)))))
          
          ;; TODO: validate against allowlist and make HTTP request
          (declare (ignore method url headers body))
          (log:debug "http_request called (not yet implemented)")
          
          ;; Return 403 (forbidden) for now
          (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) 403)))
    (error (c)
      (log:error "Host http-request callback error: ~A" c)))
  (cffi:null-pointer))

;;; --- Host function: get_secret ---
;;;
;;; WASM signature: (i32 name_ptr, i32 name_len) -> (i32 value_ptr or -1 if not found)

(cffi:defcallback host-get-secret-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env nresults))
  (handler-case
      (progn
        (unless (>= nargs 2)
          (log:debug "host-get-secret-callback: expected nargs >= 2, got ~A" nargs)
          (return-from host-get-secret-callback (cffi:null-pointer)))
        
        (let* ((name (read-wasm-string caller
                       (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0))
                       (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))))
          
          ;; TODO: contact daemon via RPC to resolve credential
          (declare (ignore name))
          (log:debug "get_secret called (not yet implemented)")
          
          ;; Return -1 (not found) for now
          (setf (wasmtime-val-i32 (cffi:mem-aptr results '(:struct wasmtime-val) 0)) -1)))
    (error (c)
      (log:error "Host get-secret callback error: ~A" c)))
  (cffi:null-pointer))

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

(defun get-wasm-memory (context instance)
  "Get the WASM linear memory base pointer and size from INSTANCE.
Returns (values base-pointer memory-size memory-ptr)."
  (cffi:with-foreign-object (mem-ext '(:struct wasmtime-extern))
    (unless (wasmtime-instance-export-get context instance "memory" 6 mem-ext)
      (error "WASM module does not export 'memory'"))
    (let* ((mem-ptr (wasmtime-extern-memory-ptr mem-ext))
           (base (wasmtime-memory-data context mem-ptr))
           (mem-size (wasmtime-memory-data-size context mem-ptr)))
      (when (cffi:null-pointer-p base)
        (error "WASM memory base pointer is null"))
      (values base mem-size mem-ptr))))

(defun get-wasm-export-func (context instance name)
  "Get a function export from INSTANCE by NAME. Returns the func pointer."
  (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
    (unless (wasmtime-instance-export-get
             context instance name (length name) ext)
      (error "Export ~S not found" name))
    (wasmtime-extern-func-ptr ext)))

(defun call-wasm-alloc (context alloc-func size)
  "Call the WASM alloc(size) -> ptr function. Returns the i32 pointer.
Signals an error if alloc returns 0 (null)."
  (cffi:with-foreign-objects ((args '(:struct wasmtime-val) 1)
                              (results '(:struct wasmtime-val) 1)
                              (trap-ptr :pointer))
    (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)) size)
    (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
    (let ((err (wasmtime-func-call context alloc-func args 1 results 1 trap-ptr)))
      (check-wasmtime-error err trap-ptr "alloc"))
    (let ((ptr (wasmtime-val-i32 results)))
      (when (zerop ptr)
        (error "WASM alloc(~D) returned null pointer" size))
      ptr)))

(defun call-wasm-dealloc (context dealloc-func ptr size)
  "Call the WASM dealloc(ptr, size) function. Ignores errors from dealloc."
  (handler-case
      (cffi:with-foreign-objects ((args '(:struct wasmtime-val) 2)
                                  (trap-ptr :pointer))
        (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)) ptr)
        (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)) size)
        (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
        (let ((err (wasmtime-func-call context dealloc-func args 2
                                       (cffi:null-pointer) 0 trap-ptr)))
          (check-wasmtime-error err trap-ptr "dealloc")))
    (error (c)
      (log:warn "dealloc failed (non-fatal): ~A" c))))

(defun write-bytes-to-wasm-memory (context instance bytes wasm-ptr)
  "Write BYTES (an octet vector) into WASM linear memory at WASM-PTR offset."
  (multiple-value-bind (base mem-size) (get-wasm-memory context instance)
    (let ((len (length bytes)))
      (when (> (+ wasm-ptr len) mem-size)
        (error "Write would exceed WASM memory: offset ~D + len ~D > size ~D"
               wasm-ptr len mem-size))
      (loop for i below len
            do (setf (cffi:mem-aref base :uint8 (+ wasm-ptr i))
                     (aref bytes i))))))

(defun read-bytes-from-wasm-memory (context instance wasm-ptr len)
  "Read LEN bytes from WASM linear memory at WASM-PTR offset.
Returns a Lisp string (UTF-8 decoded)."
  (multiple-value-bind (base mem-size) (get-wasm-memory context instance)
    (when (> (+ wasm-ptr len) mem-size)
      (error "Read would exceed WASM memory: offset ~D + len ~D > size ~D"
             wasm-ptr len mem-size))
    (cffi:foreign-string-to-lisp (cffi:inc-pointer base wasm-ptr)
                                 :count len :encoding :utf-8)))

(defun call-wasm-json-func (context func-ptr params-ptr params-len out-ptr out-len)
  "Call a WASM function with JSON ABI signature:
  (i32 params_ptr, i32 params_len, i32 out_ptr, i32 out_len) -> i32
Returns the i32 return code."
  (cffi:with-foreign-objects ((args '(:struct wasmtime-val) 4)
                              (results '(:struct wasmtime-val) 1)
                              (trap-ptr :pointer))
    (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)) params-ptr)
    (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)) params-len)
    (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 2)) out-ptr)
    (setf (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 3)) out-len)
    (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
    (let ((err (wasmtime-func-call context func-ptr args 4 results 1 trap-ptr)))
      (check-wasmtime-error err trap-ptr "json_func_call"))
    (wasmtime-val-i32 results)))

(defun json-call-with-instance (context instance export-name params
                                &key (output-buffer-size +json-output-buffer-size+))
  "Execute a JSON ABI call on an already-instantiated WASM module.
PARAMS is a Lisp data structure (hash-table, plist, etc.) to serialize as JSON.
Returns the parsed JSON result."
  (let* ((json-string (with-output-to-string (s)
                        (shasht:write-json params s)))
         (json-bytes (sb-ext:string-to-octets json-string :external-format :utf-8))
         (json-len (length json-bytes))
         (alloc-func (get-wasm-export-func context instance "alloc"))
         (dealloc-func (get-wasm-export-func context instance "dealloc"))
         (target-func (get-wasm-export-func context instance export-name))
         (in-ptr 0)
         (out-ptr 0))
    (unwind-protect
         (progn
           (setf in-ptr (call-wasm-alloc context alloc-func json-len))
           (write-bytes-to-wasm-memory context instance json-bytes in-ptr)
           (setf out-ptr (call-wasm-alloc context alloc-func output-buffer-size))
           (let ((rc (call-wasm-json-func context target-func
                                          in-ptr json-len
                                          out-ptr output-buffer-size)))
             (unless (zerop rc)
               (error "WASM JSON function ~S returned non-zero: ~D" export-name rc))
             (let ((result-str (read-bytes-from-wasm-memory
                                context instance out-ptr output-buffer-size)))
               (let ((trimmed (string-right-trim '(#\Nul) result-str)))
                 (if (zerop (length trimmed))
                     nil
                     (shasht:read-json trimmed))))))
      (when (plusp in-ptr)
        (call-wasm-dealloc context dealloc-func in-ptr json-len))
      (when (plusp out-ptr)
        (call-wasm-dealloc context dealloc-func out-ptr output-buffer-size)))))

(defun run-wasm-json-call (wat-string export-name params
                           &key (output-buffer-size +json-output-buffer-size+))
  "Compile WAT, register host functions via linker, instantiate, and call
EXPORT-NAME using the JSON-through-memory ABI.
PARAMS is a Lisp data structure to serialize as JSON input.
Returns the parsed JSON result."
  (ensure-wasmtime-loaded)
  (assert-wasmtime-abi)
  (let ((engine (wasm-engine-new))
        (store nil)
        (module nil)
        (linker nil)
        (log-functype nil)
        (kv-get-functype nil)
        (kv-set-functype nil)
        (kv-delete-functype nil)
        (kv-exists-functype nil)
        (kv-list-functype nil)
        (http-functype nil)
        (secret-functype nil))
    (when (cffi:null-pointer-p engine)
      (error "Failed to create wasmtime engine"))
    (unwind-protect
         (progn
           (setf store (wasmtime-store-new engine (cffi:null-pointer) (cffi:null-pointer)))
           (when (cffi:null-pointer-p store)
             (error "Failed to create wasmtime store"))
           (setf linker (wasmtime-linker-new engine))
           (when (cffi:null-pointer-p linker)
             (error "Failed to create wasmtime linker"))
           (let ((context (wasmtime-store-context store)))
             (setf log-functype (make-functype
                                 (list +wasm-i32+ +wasm-i32+ +wasm-i32+) '()))
             (define-host-fn linker engine log-functype
                            "env" 3 "log" 3
                            (cffi:callback host-log-callback))
             (setf kv-get-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-get-functype
                            "env" 3 "kv_get" 6
                            (cffi:callback host-kv-get-callback))
             (setf kv-set-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                    (list +wasm-i32+)))
             (define-host-fn linker engine kv-set-functype
                            "env" 3 "kv_set" 6
                            (cffi:callback host-kv-set-callback))
             (setf kv-delete-functype (make-functype
                                       (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-delete-functype
                            "env" 3 "kv_delete" 9
                            (cffi:callback host-kv-delete-callback))
             (setf kv-exists-functype (make-functype
                                       (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-exists-functype
                            "env" 3 "kv_exists" 9
                            (cffi:callback host-kv-exists-callback))
             (setf kv-list-functype (make-functype
                                     (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-list-functype
                            "env" 3 "kv_list" 7
                            (cffi:callback host-kv-list-callback))
             (setf http-functype (make-functype
                                  (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+
                                        +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                  (list +wasm-i32+)))
             (define-host-fn linker engine http-functype
                            "env" 3 "http_request" 12
                            (cffi:callback host-http-request-callback))
             (setf secret-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine secret-functype
                            "env" 3 "get_secret" 10
                            (cffi:callback host-get-secret-callback))
             (cffi:with-foreign-object (wasm-bytes '(:struct wasm-byte-vec))
               (let ((wat-err (wasmtime-wat2wasm wat-string (length wat-string) wasm-bytes)))
                 (unless (cffi:null-pointer-p wat-err)
                   (error "WAT parse error: ~A" (wasmtime-error-to-string wat-err)))
                 (unwind-protect
                      (let ((wasm-data (cffi:foreign-slot-value
                                        wasm-bytes '(:struct wasm-byte-vec) 'data))
                            (wasm-size (cffi:foreign-slot-value
                                        wasm-bytes '(:struct wasm-byte-vec) 'size)))
                        (cffi:with-foreign-object (module-ptr :pointer)
                          (let ((mod-err (wasmtime-module-new
                                         engine wasm-data wasm-size module-ptr)))
                            (unless (cffi:null-pointer-p mod-err)
                              (error "Module compile error: ~A"
                                     (wasmtime-error-to-string mod-err)))
                            (setf module (cffi:mem-ref module-ptr :pointer))
                            (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                                                       (trap-ptr :pointer))
                              (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
                              (let ((inst-err (wasmtime-linker-instantiate
                                              linker context module
                                              instance trap-ptr)))
                                (check-wasmtime-error inst-err trap-ptr "linker_instantiate"))
                              (json-call-with-instance
                               context instance export-name params
                               :output-buffer-size output-buffer-size)))))
                   (wasm-byte-vec-delete wasm-bytes))))))
      (when log-functype (wasm-functype-delete log-functype))
      (when kv-get-functype (wasm-functype-delete kv-get-functype))
      (when kv-set-functype (wasm-functype-delete kv-set-functype))
      (when kv-delete-functype (wasm-functype-delete kv-delete-functype))
      (when kv-exists-functype (wasm-functype-delete kv-exists-functype))
      (when kv-list-functype (wasm-functype-delete kv-list-functype))
      (when http-functype (wasm-functype-delete http-functype))
      (when secret-functype (wasm-functype-delete secret-functype))
      (when linker (wasmtime-linker-delete linker))
      (when module (wasmtime-module-delete module))
      (when store (wasmtime-store-delete store))
      (wasm-engine-delete engine))))

(defun run-wasm-bytes-json-call (wasm-bytes export-name params
                                 &key (output-buffer-size +json-output-buffer-size+))
  "Like run-wasm-json-call but accepts pre-compiled WASM bytes (octet vector)
instead of a WAT string."
  (ensure-wasmtime-loaded)
  (assert-wasmtime-abi)
  (let ((engine (wasm-engine-new))
        (store nil)
        (module nil)
        (linker nil)
        (log-functype nil)
        (kv-get-functype nil)
        (kv-set-functype nil)
        (kv-delete-functype nil)
        (kv-exists-functype nil)
        (kv-list-functype nil)
        (http-functype nil)
        (secret-functype nil)
        (wasm-len (length wasm-bytes)))
    (when (cffi:null-pointer-p engine)
      (error "Failed to create wasmtime engine"))
    (unwind-protect
         (progn
           (setf store (wasmtime-store-new engine (cffi:null-pointer) (cffi:null-pointer)))
           (when (cffi:null-pointer-p store)
             (error "Failed to create wasmtime store"))
           (setf linker (wasmtime-linker-new engine))
           (when (cffi:null-pointer-p linker)
             (error "Failed to create wasmtime linker"))
           (let ((context (wasmtime-store-context store)))
             (setf log-functype (make-functype
                                 (list +wasm-i32+ +wasm-i32+ +wasm-i32+) '()))
             (define-host-fn linker engine log-functype
                            "env" 3 "log" 3
                            (cffi:callback host-log-callback))
             (setf kv-get-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-get-functype
                            "env" 3 "kv_get" 6
                            (cffi:callback host-kv-get-callback))
             (setf kv-set-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                    (list +wasm-i32+)))
             (define-host-fn linker engine kv-set-functype
                            "env" 3 "kv_set" 6
                            (cffi:callback host-kv-set-callback))
             (setf kv-delete-functype (make-functype
                                       (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-delete-functype
                            "env" 3 "kv_delete" 9
                            (cffi:callback host-kv-delete-callback))
             (setf kv-exists-functype (make-functype
                                       (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-exists-functype
                            "env" 3 "kv_exists" 9
                            (cffi:callback host-kv-exists-callback))
             (setf kv-list-functype (make-functype
                                     (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine kv-list-functype
                            "env" 3 "kv_list" 7
                            (cffi:callback host-kv-list-callback))
             (setf http-functype (make-functype
                                  (list +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+
                                        +wasm-i32+ +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                  (list +wasm-i32+)))
             (define-host-fn linker engine http-functype
                            "env" 3 "http_request" 12
                            (cffi:callback host-http-request-callback))
             (setf secret-functype (make-functype
                                    (list +wasm-i32+ +wasm-i32+) (list +wasm-i32+)))
             (define-host-fn linker engine secret-functype
                            "env" 3 "get_secret" 10
                            (cffi:callback host-get-secret-callback))
             (cffi:with-foreign-object (wasm-ptr :uint8 wasm-len)
               (loop for i below wasm-len
                     do (setf (cffi:mem-aref wasm-ptr :uint8 i)
                              (aref wasm-bytes i)))
               (cffi:with-foreign-object (module-ptr :pointer)
                 (let ((mod-err (wasmtime-module-new engine wasm-ptr wasm-len module-ptr)))
                   (unless (cffi:null-pointer-p mod-err)
                     (error "Module compile error: ~A"
                            (wasmtime-error-to-string mod-err)))
                   (setf module (cffi:mem-ref module-ptr :pointer))
                   (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                                              (trap-ptr :pointer))
                     (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
                     (let ((inst-err (wasmtime-linker-instantiate
                                     linker context module
                                     instance trap-ptr)))
                       (check-wasmtime-error inst-err trap-ptr "linker_instantiate"))
                     (json-call-with-instance
                      context instance export-name params
                      :output-buffer-size output-buffer-size)))))))
      (when log-functype (wasm-functype-delete log-functype))
      (when kv-get-functype (wasm-functype-delete kv-get-functype))
      (when kv-set-functype (wasm-functype-delete kv-set-functype))
      (when kv-delete-functype (wasm-functype-delete kv-delete-functype))
      (when kv-exists-functype (wasm-functype-delete kv-exists-functype))
      (when kv-list-functype (wasm-functype-delete kv-list-functype))
      (when http-functype (wasm-functype-delete http-functype))
      (when secret-functype (wasm-functype-delete secret-functype))
      (when linker (wasmtime-linker-delete linker))
      (when module (wasmtime-module-delete module))
      (when store (wasmtime-store-delete store))
      (wasm-engine-delete engine))))

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
