;;;; client/package.lisp

(defpackage #:crichton-client
  (:use #:cl)
  (:export #:main
           #:chat-repl
           #:send-chat
           #:connect-daemon))
