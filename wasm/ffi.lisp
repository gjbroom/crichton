;;;; wasm/ffi.lisp
;;;;
;;;; Thin CFFI bindings to the wasmtime C API (v41.0.2).
;;;; Only wraps what we need — engine, store, module, instance, func, val.

(in-package #:crichton/wasm)

;;; --- Library loading ---

(cffi:define-foreign-library libwasmtime
  (:unix "libwasmtime.so"))

(defun ensure-wasmtime-loaded ()
  (unless (cffi:foreign-library-loaded-p 'libwasmtime)
    (let ((vendor-lib (merge-pathnames
                       #p"vendor/wasmtime/lib/"
                       (asdf:system-source-directory :crichton))))
      (pushnew vendor-lib cffi:*foreign-library-directories* :test #'equal))
    (cffi:use-foreign-library libwasmtime)))

;;; --- Struct definitions ---

(cffi:defcstruct wasm-byte-vec
  (size :size)
  (data :pointer))

(cffi:defcstruct wasmtime-instance
  (store-id :uint64)
  (__private :size))

(cffi:defcstruct wasmtime-func
  (store-id :uint64)
  (__private :pointer))

(cffi:defcstruct wasmtime-val
  (kind :uint8)
  (padding :uint8 :count 7)
  (i32 :int32))

(cffi:defcstruct wasmtime-extern
  (kind :uint8)
  (padding :uint8 :count 7)
  (func-store-id :uint64)
  (func-private :pointer))

(cffi:defcstruct wasm-valtype-vec
  (size :size)
  (data :pointer))

;;; --- Constants ---

(defconstant +wasmtime-i32+ 0)
(defconstant +wasmtime-i64+ 1)
(defconstant +wasmtime-extern-func+ 0)
(defconstant +wasm-i32+ 0)
(defconstant +wasm-i64+ 1)

;;; --- Foreign functions ---

;; Engine
(cffi:defcfun "wasm_engine_new" :pointer)
(cffi:defcfun "wasm_engine_delete" :void (engine :pointer))

;; Store
(cffi:defcfun "wasmtime_store_new" :pointer
  (engine :pointer) (data :pointer) (finalizer :pointer))
(cffi:defcfun "wasmtime_store_delete" :void (store :pointer))
(cffi:defcfun "wasmtime_store_context" :pointer (store :pointer))

;; WAT → WASM
(cffi:defcfun "wasmtime_wat2wasm" :pointer
  (wat :string) (wat-len :size) (ret :pointer))

;; Module
(cffi:defcfun "wasmtime_module_new" :pointer
  (engine :pointer) (wasm :pointer) (wasm-len :size) (ret :pointer))
(cffi:defcfun "wasmtime_module_delete" :void (module :pointer))

;; Instance
(cffi:defcfun "wasmtime_instance_new" :pointer
  (context :pointer) (module :pointer)
  (imports :pointer) (nimports :size)
  (instance :pointer) (trap :pointer))

(cffi:defcfun "wasmtime_instance_export_get" :bool
  (context :pointer) (instance :pointer)
  (name :string) (name-len :size)
  (item :pointer))

;; Function call
(cffi:defcfun "wasmtime_func_call" :pointer
  (context :pointer) (func :pointer)
  (args :pointer) (nargs :size)
  (results :pointer) (nresults :size)
  (trap :pointer))

;; Error handling
(cffi:defcfun "wasmtime_error_message" :void
  (error :pointer) (message :pointer))
(cffi:defcfun "wasmtime_error_delete" :void (error :pointer))

;; Trap
(cffi:defcfun "wasm_trap_message" :void
  (trap :pointer) (message :pointer))
(cffi:defcfun "wasm_trap_delete" :void (trap :pointer))

;; Byte vec
(cffi:defcfun "wasm_byte_vec_delete" :void (vec :pointer))

;; Valtype / functype (for host functions later)
(cffi:defcfun "wasm_valtype_new" :pointer (kind :uint8))
(cffi:defcfun "wasm_valtype_delete" :void (vt :pointer))
(cffi:defcfun "wasm_valtype_vec_new" :void
  (out :pointer) (size :size) (data :pointer))
(cffi:defcfun "wasm_valtype_vec_new_empty" :void (out :pointer))
(cffi:defcfun "wasm_functype_new" :pointer
  (params :pointer) (results :pointer))
(cffi:defcfun "wasm_functype_delete" :void (ft :pointer))

;; Linker (for Phase 2 host functions)
(cffi:defcfun "wasmtime_linker_new" :pointer (engine :pointer))
(cffi:defcfun "wasmtime_linker_delete" :void (linker :pointer))
(cffi:defcfun "wasmtime_linker_define_func" :pointer
  (linker :pointer)
  (module-name :string) (module-name-len :size)
  (name :string) (name-len :size)
  (ty :pointer) (cb :pointer)
  (data :pointer) (finalizer :pointer))
(cffi:defcfun "wasmtime_linker_instantiate" :pointer
  (linker :pointer) (context :pointer) (module :pointer)
  (instance :pointer) (trap :pointer))
