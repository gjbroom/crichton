;;;; wasm/ffi.lisp
;;;;
;;;; Thin CFFI bindings to the wasmtime C API (v41.0.2).
;;;; Only wraps what we need — engine, store, module, instance, func, val,
;;;; linker, memory, and caller.

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

;; wasmtime_func_t: store_id(u64) + __private(ptr) = 16 bytes on 64-bit
(cffi:defcstruct wasmtime-func
  (store-id :uint64)
  (__private :pointer))

;; wasmtime_memory_t: store_id(u64) + __private1(u32) + __private2(u32) = 16 bytes
(cffi:defcstruct wasmtime-memory
  (store-id :uint64)
  (__private1 :uint32)
  (__private2 :uint32))

;; wasmtime_global_t: store_id(u64) + 3×u32 = 20 bytes, padded to 24
(cffi:defcstruct wasmtime-global
  (store-id :uint64)
  (__private1 :uint32)
  (__private2 :uint32)
  (__private3 :uint32))

;; wasmtime_table_t: store_id(u64) + __private1(u32) + __private2(u32) = 16 bytes
(cffi:defcstruct wasmtime-table
  (store-id :uint64)
  (__private1 :uint32)
  (__private2 :uint32))

;; wasmtime_anyref_t: store_id(u64) + u32 + u32 + ptr = 24 bytes on 64-bit
(cffi:defcstruct wasmtime-anyref
  (store-id :uint64)
  (__private1 :uint32)
  (__private2 :uint32)
  (__private3 :pointer))

;; wasmtime_externref_t: same layout as anyref = 24 bytes on 64-bit
(cffi:defcstruct wasmtime-externref
  (store-id :uint64)
  (__private1 :uint32)
  (__private2 :uint32)
  (__private3 :pointer))

;; wasmtime_valunion_t: union of all val payloads, 24 bytes on 64-bit
(cffi:defcunion wasmtime-valunion
  (i32 :int32)
  (i64 :int64)
  (f32 :float)
  (f64 :double)
  (anyref (:struct wasmtime-anyref))
  (externref (:struct wasmtime-externref))
  (funcref (:struct wasmtime-func))
  (v128 :uint8 :count 16))

;; wasmtime_val_t: kind(u8) + padding(7) + union(24) = 32 bytes on 64-bit
(cffi:defcstruct wasmtime-val
  (kind :uint8)
  (of (:union wasmtime-valunion)))

;; wasmtime_extern_union_t: union of all extern payloads
(cffi:defcunion wasmtime-extern-union
  (func (:struct wasmtime-func))
  (global (:struct wasmtime-global))
  (table (:struct wasmtime-table))
  (memory (:struct wasmtime-memory))
  (sharedmemory :pointer))

;; wasmtime_extern_t: kind(u8) + padding + union = 32 bytes on 64-bit
(cffi:defcstruct wasmtime-extern
  (kind :uint8)
  (of (:union wasmtime-extern-union)))

(cffi:defcstruct wasm-valtype-vec
  (size :size)
  (data :pointer))

;;; --- Constants ---

(defconstant +wasmtime-i32+ 0)
(defconstant +wasmtime-i64+ 1)
(defconstant +wasmtime-extern-func+ 0)
(defconstant +wasmtime-extern-memory+ 3)
(defconstant +wasm-i32+ 0)
(defconstant +wasm-i64+ 1)

;;; --- ABI assertions ---

(defun assert-wasmtime-abi ()
  "Verify CFFI struct sizes match expected wasmtime ABI on 64-bit."
  (when (= (cffi:foreign-type-size :pointer) 8)
    (assert (= 24 (cffi:foreign-type-size '(:union wasmtime-valunion))))
    (assert (= 32 (cffi:foreign-type-size '(:struct wasmtime-val))))
    (assert (= 32 (cffi:foreign-type-size '(:struct wasmtime-extern)))))
  t)

;;; --- Val accessors ---

(defun wasmtime-val-i32 (val-ptr)
  "Read the i32 field from a wasmtime_val_t pointer."
  (let ((of-ptr (cffi:foreign-slot-pointer val-ptr '(:struct wasmtime-val) 'of)))
    (cffi:foreign-slot-value of-ptr '(:union wasmtime-valunion) 'i32)))

(defun (setf wasmtime-val-i32) (value val-ptr)
  "Write an i32 value into a wasmtime_val_t pointer, setting kind=I32."
  (setf (cffi:foreign-slot-value val-ptr '(:struct wasmtime-val) 'kind) +wasmtime-i32+)
  (let ((of-ptr (cffi:foreign-slot-pointer val-ptr '(:struct wasmtime-val) 'of)))
    (setf (cffi:foreign-slot-value of-ptr '(:union wasmtime-valunion) 'i32) value))
  value)

;;; --- Extern accessors ---

(defun wasmtime-extern-func-ptr (extern-ptr)
  "Return a pointer to the func field of a wasmtime_extern_t."
  (let ((of-ptr (cffi:foreign-slot-pointer extern-ptr '(:struct wasmtime-extern) 'of)))
    (cffi:foreign-slot-pointer of-ptr '(:union wasmtime-extern-union) 'func)))

(defun wasmtime-extern-memory-ptr (extern-ptr)
  "Return a pointer to the memory field of a wasmtime_extern_t.
   Signals an error if the extern kind is not MEMORY."
  (let ((k (cffi:foreign-slot-value extern-ptr '(:struct wasmtime-extern) 'kind)))
    (unless (= k +wasmtime-extern-memory+)
      (error "wasmtime_extern kind ~A is not MEMORY (~A)" k +wasmtime-extern-memory+)))
  (let ((of-ptr (cffi:foreign-slot-pointer extern-ptr '(:struct wasmtime-extern) 'of)))
    (cffi:foreign-slot-pointer of-ptr '(:union wasmtime-extern-union) 'memory)))

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

;; Valtype / functype
(cffi:defcfun "wasm_valtype_new" :pointer (kind :uint8))
(cffi:defcfun "wasm_valtype_delete" :void (vt :pointer))
(cffi:defcfun "wasm_valtype_vec_new" :void
  (out :pointer) (size :size) (data :pointer))
(cffi:defcfun "wasm_valtype_vec_new_empty" :void (out :pointer))
(cffi:defcfun "wasm_functype_new" :pointer
  (params :pointer) (results :pointer))
(cffi:defcfun "wasm_functype_delete" :void (ft :pointer))

;; Linker
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

;; Caller (for host function callbacks)
(cffi:defcfun "wasmtime_caller_export_get" :bool
  (caller :pointer)
  (name :string) (name-len :size)
  (item :pointer))
(cffi:defcfun "wasmtime_caller_context" :pointer
  (caller :pointer))

;; Memory
(cffi:defcfun "wasmtime_memory_data" :pointer
  (context :pointer) (memory :pointer))
(cffi:defcfun "wasmtime_memory_data_size" :size
  (context :pointer) (memory :pointer))
