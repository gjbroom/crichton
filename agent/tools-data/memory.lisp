;;;; agent/tools-data/memory.lisp
;;;;
;;;; Tool definitions for long-term memory and journal access.

(in-package #:crichton/agent)

;;; --- Memory tools ---

(define-tool memory-write
    (:description "Write a durable fact to long-term memory (MEMORY.org) or today's journal.  Use 'memory' target for important, curated facts that should persist across sessions (user preferences, project context, lessons learned).  Use 'journal' target for session-specific notes and observations that don't rise to long-term memory level.")
  ((target "string"
           "Where to write: 'memory' for MEMORY.org (curated, long-term), 'journal' for today's daily log (raw, session-specific)."
           :enum ("memory" "journal")
           :required-p t)
   (content "string"
            "The content to write. Use org-mode formatting (- for list items, * for headings)."
            :required-p t))
  (cond
    ((string-equal target "memory")
     (let ((path (crichton/state:append-to-memory content)))
       (format nil "Written to long-term memory (~A)." (file-namestring path))))
    ((string-equal target "journal")
     (let ((path (crichton/state:journal-append content)))
       (format nil "Written to journal (~A)." (file-namestring path))))
    (t
     (format nil "Unknown target: ~A" target))))

(define-tool memory-search
    (:description "Search daily journal entries by keyword.  Returns matching excerpts with dates and context.  Use this to recall session-specific notes and observations from recent days.")
  ((query "string"
          "Search term to look for in journal entries. Case-insensitive substring match."
          :required-p t)
   (days-back "integer"
              "Number of days to search back from today. Default: 7."
              :default 7))
  (crichton/state:journal-search query :days-back days-back))
