;;;; tui/sixel.lisp
;;;;
;;;; Sixel graphics support for the Crichton TUI client.
;;;; Detects sixel capability and converts images via img2sixel.

(in-package #:crichton-tui)

;;; --- Detection ---

(defun sixel-terminal-p ()
  "Heuristic: check if the terminal likely supports sixel."
  (let ((term (or (sb-ext:posix-getenv "TERM") "")))
    (or (search "foot" term)
        (search "xterm" term)
        (search "mlterm" term)
        (search "wezterm" term)
        (search "yaft" term)
        (search "sixel" term))))

(defun img2sixel-available-p ()
  "Check if img2sixel is available on PATH."
  (ignore-errors
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program '("which" "img2sixel")
                          :output :string
                          :ignore-error-status t)
      (declare (ignore output error-output))
      (zerop exit-code))))

;;; --- Conversion ---

(defun image-to-sixel (path &key (max-width 80) cache)
  "Convert an image file to a sixel string using img2sixel.
Returns the sixel string or NIL on failure.
If CACHE is a hash-table, results are cached by PATH."
  (when cache
    (let ((cached (gethash path cache)))
      (when cached (return-from image-to-sixel cached))))
  (handler-case
      (let* ((width-str (format nil "~D" (* max-width 8)))
             (result (uiop:run-program
                      (list "img2sixel" "-w" width-str (namestring path))
                      :output :string
                      :error-output :string
                      :ignore-error-status t)))
        (when (and result (plusp (length result)))
          (when cache
            (setf (gethash path cache) result))
          result))
    (error () nil)))

;;; --- Extraction from markdown ---

(defun extract-image-refs (text)
  "Extract markdown image references ![alt](path) from TEXT.
Returns a list of (start end path alt) tuples."
  (let ((results nil)
        (pos 0)
        (len (length text)))
    (loop
      (let ((bang (search "![" text :start2 pos)))
        (unless bang
          (return (nreverse results)))
        (let ((close-bracket (position #\] text :start (+ bang 2))))
          (cond
            ((not close-bracket)
             (setf pos (+ bang 2)))
            ((or (>= (1+ close-bracket) len)
                 (not (char= (char text (1+ close-bracket)) #\()))
             (setf pos (1+ close-bracket)))
            (t
             (let ((close-paren (position #\) text :start (+ close-bracket 2))))
               (cond
                 ((not close-paren)
                  (setf pos (+ close-bracket 2)))
                 (t
                  (push (list bang (1+ close-paren)
                              (subseq text (+ close-bracket 2) close-paren)
                              (subseq text (+ bang 2) close-bracket))
                        results)
                  (setf pos (1+ close-paren))))))))))))
