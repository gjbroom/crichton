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

;;; --- Input area ---

(defun render-input-area (model)
  (tui.textinput:textinput-view (model-input model)))

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
         (chrome-lines (+ 3 toast-lines))  ; title + status + input + toast
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
