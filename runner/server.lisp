;;;; runner/server.lisp
;;;;
;;;; WASM skill runner process.
;;;; Listens on a unix domain socket, accepts NDJSON requests,
;;;; dispatches WASM module invocations, returns results.
;;;;
;;;; Invoked as: crichton runner --socket /path/to.sock

(in-package #:crichton/runner)

(defvar *runner-running* nil "T while the runner accept loop is active.")

;;; --- CLI argument parsing ---

(defun parse-runner-args (args)
  "Extract --socket PATH from ARGS. Returns the socket path or signals an error."
  (let ((pos (position "--socket" args :test #'string-equal)))
    (unless (and pos (< (1+ pos) (length args)))
      (error "Usage: crichton runner --socket /path/to.sock"))
    (nth (1+ pos) args)))

;;; --- Request dispatch ---

(defun dispatch-request (msg)
  "Dispatch a parsed NDJSON request and return a response hash-table."
  (let ((id (crichton/rpc:msg-id msg))
        (op (crichton/rpc:msg-op msg)))
    (handler-case
        (crichton/config:string-case op
          ("ping"
           (crichton/rpc:make-ok-response id "pong"))

          ("invoke"
           (handle-invoke id msg))

          ("invoke_json"
           (handle-invoke-json id msg))

          (otherwise
           (crichton/rpc:make-error-response
            id "unknown_op"
            (format nil "Unknown operation: ~A" op))))
      (error (c)
        (crichton/rpc:make-error-response
         id "internal_error"
         (format nil "~A" c))))))

(defun maybe-with-context (manifest fn)
  "Call FN, optionally wrapping it in a skill context from MANIFEST.
   When MANIFEST is non-nil, binds *current-skill* for the duration."
  (if manifest
      (call-with-skill-context
       (make-skill-context-from-manifest manifest)
       fn)
      (funcall fn)))

(defun invoke-wasm-i32 (wat wasm-b64 export-name args-list nresults)
  "Run a WASM module using the i32 ABI from either WAT source or base64 bytes."
  (if wat
      (crichton/wasm:run-wasm-with-host-fns
       wat export-name :args args-list :nresults nresults)
      (crichton/wasm:run-wasm-bytes-with-host-fns
       (crichton/rpc:base64-to-bytes wasm-b64) export-name
       :args args-list :nresults nresults)))

(defun invoke-wasm-json (wat wasm-b64 export-name params)
  "Run a WASM module using the JSON ABI from either WAT source or base64 bytes."
  (if wat
      (crichton/wasm:run-wasm-json-call wat export-name params)
      (crichton/wasm:run-wasm-bytes-json-call
       (crichton/rpc:base64-to-bytes wasm-b64) export-name params)))

(defun handle-invoke (id msg)
  "Handle an 'invoke' request: load WASM, call export, return result.
    Request fields:
      wat       - WAT source string (mutually exclusive with wasm_b64)
      wasm_b64  - base64-encoded WASM bytes (mutually exclusive with wat)
      export    - export function name to call
      args      - list of i32 arguments (optional)
      nresults  - number of expected results (default 1)
      manifest  - (optional) capability manifest (parsed plist)"
  (let* ((wat (crichton/rpc:msg-get msg "wat"))
         (wasm-b64 (crichton/rpc:msg-get msg "wasm_b64"))
         (export-name (crichton/rpc:msg-get msg "export"))
         (raw-args (crichton/rpc:msg-get msg "args"))
         (nresults (or (crichton/rpc:msg-get msg "nresults") 1))
         (manifest (crichton/rpc:msg-get msg "manifest"))
         (args-list (when raw-args (coerce raw-args 'list))))
    (unless export-name
      (error "Missing required field: export"))
    (unless (or wat wasm-b64)
      (error "Missing required field: wat or wasm_b64"))
    (let ((result (maybe-with-context manifest
                    (lambda ()
                      (invoke-wasm-i32 wat wasm-b64 export-name
                                       args-list nresults)))))
      (crichton/rpc:make-ok-response id result))))

(defun handle-invoke-json (id msg)
  "Handle an 'invoke_json' request using the JSON-through-memory ABI.
Request fields:
  wat       - WAT source string (mutually exclusive with wasm_b64)
  wasm_b64  - base64-encoded WASM bytes (mutually exclusive with wat)
  export    - export function name to call
  params    - JSON object to pass as input
  manifest  - (optional) capability manifest"
  (let* ((wat (crichton/rpc:msg-get msg "wat"))
         (wasm-b64 (crichton/rpc:msg-get msg "wasm_b64"))
         (export-name (crichton/rpc:msg-get msg "export"))
         (params (or (crichton/rpc:msg-get msg "params")
                     (make-hash-table :test #'equal)))
         (manifest (crichton/rpc:msg-get msg "manifest")))
    (unless export-name
      (error "Missing required field: export"))
    (unless (or wat wasm-b64)
      (error "Missing required field: wat or wasm_b64"))
    (let ((result (maybe-with-context manifest
                    (lambda ()
                      (invoke-wasm-json wat wasm-b64 export-name params)))))
      (crichton/rpc:make-ok-response id result))))

;;; --- Connection handler ---

(defun handle-connection (socket)
  "Service a single client connection until EOF or protocol error."
  (let ((stream (sb-bsd-sockets:socket-make-stream
                 socket :input t :output t
                 :element-type 'character
                 :buffering :line
                 :external-format :utf-8)))
    (unwind-protect
         (loop
           (let ((msg (handler-case
                          (crichton/rpc:read-message stream)
                        (error (c)
                          (log:error "Protocol error: ~A" c)
                          (return)))))
             (when (null msg)
               (log:info "Runner client disconnected (EOF)")
               (return))
             (let ((response (dispatch-request msg)))
               (handler-case
                   (crichton/rpc:write-message stream response)
                 (error (c)
                   (log:error "Write error: ~A" c)
                   (return))))))
      (close stream)
      (sb-bsd-sockets:socket-close socket))))

;;; --- Socket server ---

(defun remove-stale-socket (path)
  "Remove a stale socket file if it exists."
  (crichton/config:delete-file-if-exists path))

(defun start-server (socket-path)
  "Bind a unix domain socket at SOCKET-PATH and serve requests.
   Blocks until *runner-running* is set to NIL or the process is killed."
  (remove-stale-socket socket-path)
  (let ((server (make-instance 'sb-bsd-sockets:local-socket
                               :type :stream)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind server socket-path)
           #+sbcl (sb-posix:chmod socket-path #o600)
           (sb-bsd-sockets:socket-listen server 1)
           (setf *runner-running* t)
           (log:info "Runner listening on ~A" socket-path)
           (loop while *runner-running*
                 do (handler-case
                        (let ((client (sb-bsd-sockets:socket-accept server)))
                          (log:info "Runner accepted connection")
                          (handle-connection client))
                      (error (c)
                        (when *runner-running*
                          (log:error "Runner accept error: ~A" c)
                          (sleep 0.1))))))
      (sb-bsd-sockets:socket-close server)
      (remove-stale-socket socket-path)
      (log:info "Runner server shut down"))))

;;; --- Entry point ---

(defun main (args)
  "Runner process entry point.
   ARGS is the list of command-line arguments after 'runner'."
  (let ((socket-path (parse-runner-args args)))
    (crichton/wasm:ensure-wasmtime-loaded)
    (start-server socket-path)))
