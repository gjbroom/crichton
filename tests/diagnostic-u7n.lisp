;;;; tests/diagnostic-u7n.lisp
;;;;
;;;; Runtime diagnostic for cricht-u7n: TUI strips ESC from ANSI sequences.
;;;;
;;;; Load into the running TUI via SLIME (M-x slime-connect 127.0.0.1 4006)
;;;; or via the daemon REPL:
;;;;
;;;;   crichton-client --eval '(load "/home/gjbroom/devel/crichton/tests/diagnostic-u7n.lisp")'
;;;;
;;;; Then run:
;;;;   (crichton/diag-u7n:run)
;;;;
;;;; Each stage prints whether ESC (char 27) is present in its output.

(defpackage #:crichton/diag-u7n
  (:use #:cl)
  (:export #:run #:run-tui-stage))

(in-package #:crichton/diag-u7n)

(defun has-esc-p (str)
  "Return T if STR contains ESC (char 27)."
  (find (code-char 27) str))

(defun esc-count (str)
  "Count ESC chars in STR."
  (count (code-char 27) str))

(defun show (stage str)
  "Print whether STAGE's output contains ESC."
  (format t "~%=== ~A ===~%" stage)
  (format t "  length: ~D chars~%" (length str))
  (format t "  ESC present: ~A (count: ~D)~%"
          (if (has-esc-p str) "YES" "NO")
          (esc-count str))
  ;; Print char codes for first 60 chars
  (format t "  First 60 char codes: ~{~D~^ ~}~%"
          (loop for i from 0 below (min 60 (length str))
                collect (char-code (char str i))))
  str)

(defun run ()
  "Run the full cricht-u7n diagnostic pipeline."
  (format t "~%[cricht-u7n diagnostic] Testing ESC preservation through TUI pipeline~%")
  (format t "~%Known ESC char: ~D (0x1B)~%" (char-code (code-char 27)))

  ;; --- Stage 1: raw ANSI string ---
  (let* ((esc (code-char 27))
         (test-ansi (format nil "~C[36mtest-cyan~C[0m and ~C[1mbold~C[0m" esc esc esc esc))
         (test-md "Hello `code-span` and **bold** and *italic* text"))

    (show "1. Raw ANSI string (baseline)" test-ansi)

    ;; --- Stage 2: shasht JSON round-trip ---
    (let* ((ht (make-hash-table :test #'equal)))
      (setf (gethash "text" ht) test-ansi)
      (let* ((json-str (with-output-to-string (s)
                         (let ((*print-pretty* nil))
                           (shasht:write-json ht s))))
             (decoded (gethash "text" (shasht:read-json json-str))))
        (show "2a. After shasht write-json" json-str)
        (show "2b. After shasht read-json (decoded text field)" decoded)))

    ;; --- Stage 3: render-markdown (where ANSI codes are generated) ---
    (let* ((rendered (tuition:render-markdown test-md :style :dark :width 80)))
      (show "3. After tui:render-markdown (markdown → ANSI)" rendered)

      ;; --- Stage 4: render-styled directly on pre-ANSI string ---
      (let* ((style (tuition:make-style :foreground tuition:*fg-bright-green* :bold t))
             (styled (tuition:render-styled style rendered)))
        (show "4. After render-styled applied to markdown output" styled))

      ;; --- Stage 5: join-vertical ---
      (let* ((label (tuition:render-styled
                     (tuition:make-style :foreground tuition:*fg-bright-green* :bold t)
                     "Crichton (12:00):"))
             (joined (tuition:join-vertical tuition:+left+ label rendered)))
        (show "5. After join-vertical (label + body)" joined)

        ;; --- Stage 6: viewport-set-content + viewport-view ---
        (let* ((vp (tui.viewport:make-viewport :width 80 :height 20)))
          (tui.viewport:viewport-set-content vp joined)
          (let* ((view (tui.viewport:viewport-view vp)))
            (show "6. After viewport-set-content + viewport-view" view)

            ;; --- Stage 7: format nil "~A" (as used in render) ---
            (let* ((formatted (format nil "~A" view)))
              (show "7. After format nil ~A" formatted))

            ;; --- Stage 8: write-string to string output stream ---
            (let* ((out (with-output-to-string (s)
                          (write-string view s))))
              (show "8. After write-string to string-output-stream" out))))))

    ;; --- Extra: test split-string-by-whitespace directly ---
    (let* ((with-ansi (format nil "word1 ~C[36mword2~C[0m word3" (code-char 27) (code-char 27)))
           (split (with-output-to-string (s)
                    (format s "~{[~A]~^ ~}"
                            (tuition::split-string-by-whitespace with-ansi)))))
      (show "E1. split-string-by-whitespace on ANSI string (bracket=word)" split))

    ;; --- Extra: test %tokenize ---
    (let* ((with-ansi (format nil "word1 ~C[36mword2~C[0m word3" (code-char 27) (code-char 27)))
           (tokens (tuition::%tokenize with-ansi))
           (tok-str (with-output-to-string (s)
                      (dolist (tok tokens)
                        (format s "[~A:~S] "
                                (tuition::%rtok-type tok)
                                (tuition::%rtok-text tok))))))
      (show "E2. %tokenize output (type:text pairs)" tok-str)))

  (format t "~%[cricht-u7n diagnostic] Done.~%"))

(defun run-tui-stage ()
  "Run diagnostic inside the TUI process — checks if ESC survives the full
   render path by injecting a test entry into the live TUI model.
   Only works when called from within the TUI's Swank REPL (port 4006)."
  (handler-case
      (let* ((program tuition:*current-program*)
             (model (when program (tuition::program-model program))))
        (if (null model)
            (format t "~%[u7n] No running TUI program found (not in TUI Swank).~%")
            (let* ((esc (code-char 27))
                   (test-text (format nil "Test cyan ~C[36mcoloured~C[0m and `code` and **bold**."
                                      esc esc))
                   (entry (make-instance 'crichton-tui::chat-entry
                                         :role :assistant
                                         :text test-text
                                         :time (get-universal-time)))
                   (width (crichton-tui::model-width model))
                   (rendered (crichton-tui::render-chat-entry% entry width)))
              (format t "~%[u7n TUI stage]~%")
              (format t "  Input text ESC count: ~D~%" (esc-count test-text))
              (format t "  Rendered output ESC count: ~D~%" (esc-count rendered))
              (format t "  Input first 80 char codes: ~{~D~^ ~}~%"
                      (loop for i from 0 below (min 80 (length test-text))
                            collect (char-code (char test-text i))))
              (format t "  Rendered first 80 char codes: ~{~D~^ ~}~%"
                      (loop for i from 0 below (min 80 (length rendered))
                            collect (char-code (char rendered i)))))))
    (error (c)
      (format t "~%[u7n] Error: ~A~%" c))))
