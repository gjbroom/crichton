(module
  ;; Import the log host function from Crichton
  ;; Signature: (level: i32, ptr: i32, len: i32) -> ()
  (import "env" "log" (func $log (param i32 i32 i32)))

  ;; Export memory so host functions can read our strings
  ;; 1 page = 64KB, plenty for our message
  (memory (export "memory") 1)

  ;; Embed our message in memory at offset 0
  (data (i32.const 0) "The answer to life, the universe, and everything is: 42")

  ;; Main entry point - returns the answer!
  (func (export "main") (result i32)
    ;; First, log our profound message
    ;; log(INFO, offset=0, length=56)
    (call $log
      (i32.const 1)   ;; level: 1 = INFO
      (i32.const 0)   ;; ptr: message starts at byte 0
      (i32.const 56)) ;; len: message is 56 bytes

    ;; Return the ultimate answer
    (i32.const 42))

  ;; Alternative entry point for those who prefer a function name
  (func (export "get_answer") (result i32)
    (i32.const 42))
)
