;;;; daemon/runner-client.lisp
;;;;
;;;; Daemon-side client for the WASM skill runner process.
;;;; Spawns the runner, connects via unix domain socket, provides
;;;; synchronous invoke API.

(in-package #:crichton/daemon)

(defvar *runner-process* nil "The sb-ext:process object for the runner.")
(defvar *runner-socket-path* nil "Path to the runner's unix domain socket.")
(defvar *runner-stream* nil "Character stream connected to the runner.")
(defvar *runner-socket* nil "The raw sb-bsd-sockets socket to the runner.")

(defun default-socket-path ()
  "Return the default runner socket path under *agent-home*."
  (namestring
   (merge-pathnames "runner.sock"
                    (symbol-value (find-symbol "*AGENT-HOME*" :crichton/config)))))

(defvar *crichton-source-dir*
  (namestring (asdf:system-source-directory :crichton))
  "Absolute path to the Crichton ASDF system source directory, captured at load time.")

(defun spawn-runner (&optional socket-path)
  "Spawn the runner process via a fresh SBCL. Returns the socket path."
  (let ((sock-path (or socket-path (default-socket-path))))
    (when (and *runner-process* (sb-ext:process-alive-p *runner-process*))
      (log:warn "Runner already running, killing old process")
      (kill-runner))
    (log:info "Spawning runner via sbcl --eval with socket ~A" sock-path)
    (setf *runner-socket-path* sock-path
          *runner-process*
          (sb-ext:run-program "sbcl"
                              (list "--noinform" "--non-interactive"
                                    "--eval" "(require :asdf)"
                                    "--eval" (format nil "(push #p~S asdf:*central-registry*)"
                                                    *crichton-source-dir*)
                                    "--eval" "(asdf:load-system :crichton)"
                                    "--eval" (format nil "(crichton/runner:main (list \"--socket\" ~S))"
                                                    sock-path))
                              :search t
                              :wait nil
                              :output *standard-output*
                              :error *error-output*))
    (unless (sb-ext:process-alive-p *runner-process*)
      (error "Runner process failed to start"))
    sock-path))

(defun connect-to-runner (&key (retries 50) (delay 0.02))
  "Connect to the runner's unix domain socket with retries.
   Call after spawn-runner. Sets *runner-stream*."
  (unless *runner-socket-path*
    (error "No runner socket path set — call spawn-runner first"))
  (loop for i from 1 to retries
        do (handler-case
               (let ((sock (make-instance 'sb-bsd-sockets:local-socket
                                          :type :stream)))
                 (sb-bsd-sockets:socket-connect sock *runner-socket-path*)
                 (setf *runner-socket* sock
                       *runner-stream*
                       (sb-bsd-sockets:socket-make-stream
                        sock :input t :output t
                        :element-type 'character
                        :buffering :line
                        :external-format :utf-8))
                 (log:info "Connected to runner at ~A" *runner-socket-path*)
                 (return t))
             (error ()
               (when (= i retries)
                 (error "Failed to connect to runner after ~A attempts" retries))
               (sleep delay)))))

(defun ensure-runner (&optional socket-path)
  "Ensure the runner process is running and connected.
   Spawns if needed, connects if needed. Idempotent."
  (unless (and *runner-process*
               (sb-ext:process-alive-p *runner-process*)
               *runner-stream*
               (open-stream-p *runner-stream*))
    (spawn-runner socket-path)
    (connect-to-runner))
  t)

(defun runner-call (request)
  "Send REQUEST (a hash-table) to the runner and return the parsed response.
   Synchronous — blocks until the response is received."
  (unless (and *runner-stream* (open-stream-p *runner-stream*))
    (error "Not connected to runner — call ensure-runner first"))
  (crichton/rpc:write-message *runner-stream* request)
  (let ((response (crichton/rpc:read-message *runner-stream*)))
    (unless response
      (error "Runner disconnected (EOF) — no response received"))
    response))

(defun runner-invoke (wat-or-bytes export-name
                      &key (args nil) (nresults 1) (format :wat))
  "Invoke a WASM module via the runner process.
   WAT-OR-BYTES is either a WAT string (format :wat) or an octet vector (format :bytes).
   Returns the result value on success, or signals an error."
  (ensure-runner)
  (let ((request
          (if (eq format :bytes)
              (crichton/rpc:make-request
               "invoke"
               "wasm_b64" (crichton/rpc:bytes-to-base64 wat-or-bytes)
               "export" export-name
               "args" (coerce args 'vector)
               "nresults" nresults)
              (crichton/rpc:make-request
               "invoke"
               "wat" wat-or-bytes
               "export" export-name
               "args" (coerce args 'vector)
               "nresults" nresults))))
    (let ((response (runner-call request)))
      (if (crichton/rpc:msg-ok-p response)
          (crichton/rpc:msg-result response)
          (let ((err (crichton/rpc:msg-error response)))
            (error "Runner error [~A]: ~A"
                   (gethash "code" err)
                   (gethash "message" err)))))))

(defun kill-runner ()
  "Kill the runner process and clean up."
  (when *runner-stream*
    (ignore-errors (close *runner-stream*))
    (setf *runner-stream* nil))
  (when *runner-socket*
    (ignore-errors (sb-bsd-sockets:socket-close *runner-socket*))
    (setf *runner-socket* nil))
  (when *runner-process*
    (when (sb-ext:process-alive-p *runner-process*)
      (sb-ext:process-kill *runner-process* sb-posix:sigterm)
      (sb-ext:process-wait *runner-process*))
    (sb-ext:process-close *runner-process*)
    (setf *runner-process* nil))
  (when *runner-socket-path*
    (when (probe-file *runner-socket-path*)
      (ignore-errors (delete-file *runner-socket-path*)))
    (setf *runner-socket-path* nil))
  (log:info "Runner killed"))
