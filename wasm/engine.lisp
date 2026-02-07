;;;; wasm/engine.lisp
;;;;
;;;; Higher-level Lisp interface to wasmtime.
;;;; Wraps the CFFI bindings with proper resource management.
;;;; Includes host function support via the linker API.

(in-package #:crichton/wasm)

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
   Caller must eventually call wasm-functype-delete on the result."
  (let ((nparams (length param-kinds))
        (nresults (length result-kinds)))
    (cffi:with-foreign-objects ((params-vec '(:struct wasm-valtype-vec))
                                (results-vec '(:struct wasm-valtype-vec)))
      (if (zerop nparams)
          (wasm-valtype-vec-new-empty params-vec)
          (cffi:with-foreign-object (param-arr :pointer nparams)
            (loop for kind in param-kinds
                  for i from 0
                  do (setf (cffi:mem-aref param-arr :pointer i)
                           (wasm-valtype-new kind)))
            (wasm-valtype-vec-new params-vec nparams param-arr)))
      (if (zerop nresults)
          (wasm-valtype-vec-new-empty results-vec)
          (cffi:with-foreign-object (result-arr :pointer nresults)
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
   Returns a Lisp string."
  (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
    (unless (wasmtime-caller-export-get caller "memory" 6 ext)
      (error "WASM module does not export 'memory'"))
    (let* ((mem-ptr (wasmtime-extern-memory-ptr ext))
           (ctx (wasmtime-caller-context caller))
           (base (wasmtime-memory-data ctx mem-ptr))
           (mem-size (wasmtime-memory-data-size ctx mem-ptr)))
      (when (> (+ ptr len) mem-size)
        (error "WASM memory access out of bounds: offset ~A + len ~A > size ~A"
               ptr len mem-size))
      (cffi:foreign-string-to-lisp (cffi:inc-pointer base ptr)
                                   :count len
                                   :encoding :utf-8))))

(cffi:defcallback host-log-callback :pointer
    ((env :pointer) (caller :pointer)
     (args :pointer) (nargs :size)
     (results :pointer) (nresults :size))
  (declare (ignore env results nresults))
  (handler-case
      (when (>= nargs 3)
        (let* ((level (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 0)))
               (ptr   (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 1)))
               (len   (wasmtime-val-i32 (cffi:mem-aptr args '(:struct wasmtime-val) 2)))
               (msg   (read-wasm-string caller ptr len)))
          (case level
            (0 (log:debug "~A" msg))
            (1 (log:info  "~A" msg))
            (2 (log:warn  "~A" msg))
            (3 (log:error "~A" msg))
            (t (log:info  "[level=~A] ~A" level msg)))))
    (error (c)
      (log:error "Host log callback error: ~A" c)))
  (cffi:null-pointer))

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
        (log-functype nil))
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
             ;; Register host "env.log" function: (i32, i32, i32) -> ()
             (setf log-functype (make-functype
                                 (list +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                 '()))
             (let ((def-err (wasmtime-linker-define-func
                             linker
                             "env" 3
                             "log" 3
                             log-functype
                             (cffi:callback host-log-callback)
                             (cffi:null-pointer)
                             (cffi:null-pointer))))
               (unless (cffi:null-pointer-p def-err)
                 (error "Linker define error: ~A" (wasmtime-error-to-string def-err))))
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
             ;; Register host "env.log" function: (i32, i32, i32) -> ()
             (setf log-functype (make-functype
                                 (list +wasm-i32+ +wasm-i32+ +wasm-i32+)
                                 '()))
             (let ((def-err (wasmtime-linker-define-func
                             linker
                             "env" 3
                             "log" 3
                             log-functype
                             (cffi:callback host-log-callback)
                             (cffi:null-pointer)
                             (cffi:null-pointer))))
               (unless (cffi:null-pointer-p def-err)
                 (error "Linker define error: ~A" (wasmtime-error-to-string def-err))))
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
      (when linker (wasmtime-linker-delete linker))
      (when module (wasmtime-module-delete module))
      (when store (wasmtime-store-delete store))
      (wasm-engine-delete engine))))

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
