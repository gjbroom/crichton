;;;; wasm/engine.lisp
;;;;
;;;; Higher-level Lisp interface to wasmtime.
;;;; Wraps the CFFI bindings with proper resource management.

(in-package #:crichton/wasm)

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
  (unless (cffi:null-pointer-p err)
    (error "Wasmtime error: ~A" (wasmtime-error-to-string err)))
  (when (and trap-ptr (not (cffi:null-pointer-p (cffi:mem-ref trap-ptr :pointer))))
    (error "WASM trap: ~A"
           (trap-to-string (cffi:mem-ref trap-ptr :pointer)))))

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
             ;; Compile WAT → WASM → module
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
                            ;; Instantiate (no imports)
                            (cffi:with-foreign-objects ((instance '(:struct wasmtime-instance))
                                                       (trap-ptr :pointer))
                              (setf (cffi:mem-ref trap-ptr :pointer) (cffi:null-pointer))
                              (let ((inst-err (wasmtime-instance-new
                                              context module
                                              (cffi:null-pointer) 0
                                              instance trap-ptr)))
                                (check-wasmtime-error inst-err trap-ptr "instantiate"))
                              ;; Get export
                              (cffi:with-foreign-object (ext '(:struct wasmtime-extern))
                                (unless (wasmtime-instance-export-get
                                         context instance
                                         export-name (length export-name)
                                         ext)
                                  (error "Export ~S not found" export-name))
                                ;; Call the function (0 args, 1 i32 result)
                                (cffi:with-foreign-objects ((results '(:struct wasmtime-val) 1)
                                                           (call-trap :pointer))
                                  (setf (cffi:mem-ref call-trap :pointer) (cffi:null-pointer))
                                  (let* ((func-ptr (cffi:foreign-slot-pointer ext '(:struct wasmtime-extern) 'func-store-id))
                                         (call-err (wasmtime-func-call
                                                    context func-ptr
                                                    (cffi:null-pointer) 0
                                                    results 1
                                                    call-trap)))
                                    (check-wasmtime-error call-err call-trap "func_call")
                                    ;; Extract i32 result
                                    (cffi:foreign-slot-value results '(:struct wasmtime-val) 'i32))))))))
                   (wasm-byte-vec-delete wasm-bytes))))))
      ;; Cleanup
      (when module (wasmtime-module-delete module))
      (when store (wasmtime-store-delete store))
      (wasm-engine-delete engine))))
