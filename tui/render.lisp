;;;; tui/render.lisp
;;;;
;;;; View / rendering layer for the Crichton TUI client.
;;;; Composes the screen from title bar, chat viewport,
;;;; status bar, and input area using cl-tuition layout primitives.

(in-package #:crichton-tui)

;;; --- Time formatting ---

(defun format-time (universal-time)
  (multiple-value-bind (sec min hour)
      (decode-universal-time universal-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

;;; --- Title bar ---

(defun render-title-bar (model)
  (let* ((width (model-width model))
         (title "Crichton TUI")
         (time-str (format-time (get-universal-time)))
         (session (model-session-id model))
         (right (if session
                    (format nil "session: ~A │ ~A"
                            (subseq session 0 (min 8 (length session)))
                            time-str)
                    time-str))
         (visible-right (tui:visible-length right))
         (padding (max 0 (- width (length title) visible-right)))
         (line (format nil "~A~A~A" title
                       (make-string padding :initial-element #\Space)
                       right)))
    (style *style-title-bar*
           (if (> (tui:visible-length line) width)
               (tui:truncate-text line width)
               line))))

;;; --- Chat entry rendering ---

(defun render-chat-entry (entry width)
  (or (entry-rendered entry)
      (let ((rendered (render-chat-entry% entry width)))
        (setf (entry-rendered entry) rendered)
        rendered)))

(defun render-chat-entry% (entry width)
  (let ((text-width (max 1 (- width 4))))
    (ecase (entry-role entry)
      (:user
       (let ((label (style *style-user-label*
                           (format nil "You (~A):" (format-time (entry-time entry)))))
             (body  (style *style-user-text*
                           (tui:wrap-text (entry-text entry) text-width))))
         (tui:join-vertical tui:+left+ label body)))
      (:assistant
       (let ((label (style *style-assistant-label*
                           (format nil "Crichton (~A):" (format-time (entry-time entry)))))
             (body  (tui:render-markdown (entry-text entry)
                                         :style :dark
                                         :width text-width)))
         (tui:join-vertical tui:+left+ label body)))
      (:error
       (style *style-error*
              (tui:wrap-text (format nil "Error: ~A" (entry-text entry)) text-width)))
      (:notification
       (style *style-notification*
              (tui:wrap-text (entry-text entry) text-width))))))

;;; --- Chat history ---

(defun render-chat-history (model)
  (let* ((width (model-width model))
         (entries (model-messages model))
         (rendered (mapcar (lambda (e) (render-chat-entry e width)) entries))
         (content (format nil "~{~A~^~%~%~}" rendered)))
    (tui.viewport:viewport-set-content (model-viewport model) content)
    (tui.viewport:viewport-view (model-viewport model))))

;;; --- Status bar ---

(defun format-waiting-status (spinner status-text)
  "Build the left-side status string for :waiting state from the optional
SPINNER view string and STATUS-TEXT."
  (let ((spinner-str (when spinner
                       (tui.spinner:spinner-view spinner)))
        (label (if (and status-text (plusp (length status-text)))
                   status-text
                   "Thinking...")))
    (if spinner-str
        (format nil "~A ~A" spinner-str label)
        label)))

(defun render-status-bar (model)
  (let* ((width (model-width model))
         (left (ecase (model-status model)
                 (:waiting (format-waiting-status (model-spinner model)
                                                  (model-status-text model)))
                 (:error   (style *style-error* "Error"))
                 (:idle    (model-status-text model))))
         (right (if (model-confirm-quit model)
                    "Really quit? (y/n)"
                    ""))
         (padding (max 0 (- width
                            (tui:visible-length left)
                            (length right))))
         (line (format nil "~A~A~A"
                       left
                       (make-string padding :initial-element #\Space)
                       right)))
    (style *style-status-bar* (tui:truncate-text line width))))

;;; --- Input area (multiline wrapping) ---

(defun input-content-width (model)
  "Characters available per visual line for input content (excluding prompt)."
  (let* ((prompt (tui.textinput:textinput-prompt (model-input model)))
         (prompt-len (tui:visible-length prompt)))
    (max 1 (- (model-width model) prompt-len))))

(defun input-line-count (model)
  "Number of visual lines the wrapped input currently occupies."
  (let* ((input (model-input model))
         (value (tui.textinput:textinput-value input))
         (cursor-pos (tui.textinput:textinput-cursor-pos input))
         (cw (input-content-width model))
         (text-lines (if (zerop (length value)) 0 (ceiling (length value) cw)))
         (cursor-line (1+ (floor cursor-pos cw))))
    (max 1 (max text-lines cursor-line))))

(defun render-input-area (model)
  "Render the input area with visual line wrapping."
  (let* ((input (model-input model))
         (value (tui.textinput:textinput-value input))
         (cursor-pos (tui.textinput:textinput-cursor-pos input))
         (prompt (tui.textinput:textinput-prompt input))
         (prompt-len (tui:visible-length prompt))
         (cw (input-content-width model))
         (focused (tui.textinput:textinput-focused input))
         (placeholder (tui.textinput:textinput-placeholder input)))
    (cond
      ;; Empty with placeholder
      ((and (zerop (length value)) (plusp (length placeholder)))
       (tui.textinput:textinput-view input))
      ;; Content: wrap across lines
      (t
       (let* ((nlines (input-line-count model))
              (padding (make-string prompt-len :initial-element #\Space))
              (cursor-row (floor cursor-pos cw))
              (cursor-col (mod cursor-pos cw))
              (lines '()))
         (dotimes (i nlines)
           (let* ((start (* i cw))
                  (end (min (length value) (+ start cw)))
                  (line-text (if (< start (length value))
                                 (subseq value start end)
                                 ""))
                  (prefix (if (zerop i) prompt padding))
                  (cursor-on-line (and focused (= i cursor-row))))
             (push (if cursor-on-line
                       (let* ((before (subseq line-text
                                              0 (min cursor-col (length line-text))))
                              (cursor-char (if (< cursor-col (length line-text))
                                               (string (char line-text cursor-col))
                                               " "))
                              (after (if (< cursor-col (length line-text))
                                         (subseq line-text (1+ cursor-col))
                                         ""))
                              (reversed (format nil "~C[7m~A~C[27m"
                                                #\Escape cursor-char #\Escape)))
                         (format nil "~A~A~A~A" prefix before reversed after))
                       (format nil "~A~A" prefix line-text))
                   lines)))
         (format nil "~{~A~^~%~}" (nreverse lines)))))))

;;; --- Notification toast ---

(defun render-notification-toast (model)
  "Render active notifications as a toast overlay."
  (let ((notifs (model-notifications model)))
    (when notifs
      (let* ((width (min 60 (- (model-width model) 4)))
             (lines (mapcar (lambda (n)
                              (let ((prefix (format nil "[~A] "
                                                    (string-upcase (or (notif-kind n) "info")))))
                                (tui:wrap-text (format nil "~A~A" prefix (notif-text n))
                                               (max 1 (- width 2)))))
                            (reverse notifs)))
             (body (format nil "~{~A~^~%~}" lines)))
        (style *style-notification*
               (tui:place width
                          (+ 2 (count #\Newline body))
                          tui:+center+
                          tui:+top+
                          body))))))

;;; --- Top-level view ---

(defun toast-line-count (toast-string)
  "Return the number of terminal lines occupied by TOAST-STRING, or 0."
  (if toast-string
      (1+ (count #\Newline toast-string))
      0))

(defmethod tui:view ((model tui-model))
  (let* ((toast (when (and (model-show-notifications model)
                           (model-notifications model))
                  (render-notification-toast model)))
         (toast-lines (toast-line-count toast))
         (input-lines (input-line-count model))
         (chrome-lines (+ 2 input-lines toast-lines))  ; title + status + input(N) + toast
         (vp (model-viewport model))
         (vp-height (max 1 (- (model-height model) chrome-lines))))
    ;; Temporarily adjust viewport height to fit remaining space
    (setf (tui.viewport:viewport-height vp) vp-height)
    (apply #'tui:join-vertical tui:+left+
           (append (list (render-title-bar model))
                   (when toast (list toast))
                   (list (tui.viewport:viewport-view vp)
                         (render-status-bar model)
                         (render-input-area model))))))
