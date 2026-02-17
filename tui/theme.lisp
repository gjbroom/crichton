;;;; tui/theme.lisp
;;;;
;;;; Colour theme definitions for the Crichton TUI client.
;;;; Each UI element gets a tui:make-style object controlling
;;;; foreground, background, and text attributes.

(in-package #:crichton-tui)

;;; --- Title bar: bright-white on deep navy ---

(defparameter *style-title-bar*
  (tui:make-style :foreground tui:*fg-bright-white*
                  :background (tui:parse-hex-color "#1a1a2e" :foreground nil)
                  :bold t))

;;; --- Chat message styles ---

(defparameter *style-user-label*
  (tui:make-style :foreground tui:*fg-bright-cyan*
                  :bold t))

(defparameter *style-user-text*
  (tui:make-style :foreground tui:*fg-white*))

(defparameter *style-assistant-label*
  (tui:make-style :foreground tui:*fg-bright-green*
                  :bold t))

(defparameter *style-assistant-text*
  (tui:make-style :foreground tui:*fg-white*))

;;; --- Code blocks: bright-yellow on dark grey ---

(defparameter *style-code-block*
  (tui:make-style :foreground tui:*fg-bright-yellow*
                  :background (tui:parse-hex-color "#2d2d2d" :foreground nil)))

;;; --- Error messages ---

(defparameter *style-error*
  (tui:make-style :foreground tui:*fg-bright-red*
                  :bold t))

;;; --- Notification toast: black on bright-yellow ---

(defparameter *style-notification*
  (tui:make-style :foreground tui:*fg-black*
                  :background tui:*bg-bright-yellow*
                  :bold t))

;;; --- Status bar: bright-white on #333333 ---

(defparameter *style-status-bar*
  (tui:make-style :foreground tui:*fg-bright-white*
                  :background (tui:parse-hex-color "#333333" :foreground nil)))

;;; --- Input area ---

(defparameter *style-input-prompt*
  (tui:make-style :foreground tui:*fg-bright-cyan*
                  :bold t))

(defparameter *style-input-text*
  (tui:make-style :foreground tui:*fg-white*))

;;; --- Borders: #555555 ---

(defparameter *style-border*
  (tui:make-style :foreground (tui:parse-hex-color "#555555")))

;;; --- Timestamps: dim (bright-black) ---

(defparameter *style-timestamp*
  (tui:make-style :foreground tui:*fg-bright-black*))

;;; --- Helper ---

(defun style (style-obj text)
  "Apply a theme style to TEXT, returning an ANSI-styled string."
  (tui:render-styled style-obj text))
