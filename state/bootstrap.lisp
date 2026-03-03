;;;; state/bootstrap.lisp
;;;;
;;;; User-state bootstrap files: SOUL.org, USER.org, MEMORY.org
;;;;
;;;; These org-mode files live in ~/.crichton/state/ and are injected
;;;; into the LLM system prompt at session startup.  They give the agent
;;;; its identity, user context, and long-term memory.
;;;;
;;;; Session-type filtering:
;;;;   :main      — full set (SOUL + USER + MEMORY)
;;;;   :channel   — minimal (SOUL + USER only)
;;;;   :subagent  — minimal (SOUL + USER only)

(in-package #:crichton/state)

;;; --- Directory ---

(defvar *state-dir* nil
  "Directory for user-state files.  Set during initialization.")

(defun state-dir ()
  "Return the state directory, initializing if needed."
  (or *state-dir*
      (setf *state-dir*
            (let ((dir (merge-pathnames #p"state/"
                                        crichton/config:*agent-home*)))
              (ensure-directories-exist dir)
              #+sbcl (sb-posix:chmod (namestring dir) #o700)
              dir))))

;;; --- Bootstrap file definitions ---

(defparameter *bootstrap-files*
  '((:name "SOUL"   :filename "SOUL.org"   :full t :minimal t)
    (:name "USER"   :filename "USER.org"   :full t :minimal t)
    (:name "MEMORY" :filename "MEMORY.org" :full t :minimal nil))
  "Bootstrap file specifications.
Each entry is a plist with:
  :NAME      — display name for the section header
  :FILENAME  — file on disk in state-dir
  :FULL      — include in :main sessions
  :MINIMAL   — include in :channel/:subagent sessions")

;;; --- Default templates ---

(defparameter *default-soul*
  "#+TITLE: Soul

* Core Truths
- Be genuinely helpful, not performatively helpful.
- Have opinions.  Disagree when warranted.
- Be resourceful before asking.
- You're a guest in someone's life.  Treat access with respect.

* Boundaries
- Private things stay private.
- When in doubt, ask before acting externally.

* Vibe
The quietly competent servant who's actually running everything.
"
  "Default SOUL.org content for first boot.")

(defparameter *default-user*
  "#+TITLE: User

* Identity
- Name: (your name here)

* Communication Style
- Prefers concise, direct responses.
"
  "Default USER.org content for first boot.")

;;; --- File I/O ---

(defun state-file-path (filename)
  "Return the full path for FILENAME within the state directory."
  (merge-pathnames filename (state-dir)))

(defun read-state-file (filename &optional max-chars)
  "Read the contents of a state file, returning NIL if it doesn't exist.
When MAX-CHARS is non-nil, truncate to that many characters."
  (let ((path (state-file-path filename)))
    (when (probe-file path)
      (let ((content (with-open-file (s path :direction :input
                                            :external-format :utf-8)
                       (let ((size (file-length s)))
                         (if (zerop size)
                             ""
                             (let ((buf (make-string size)))
                               (let ((n (read-sequence buf s)))
                                 (subseq buf 0 n))))))))
        (if (and max-chars (> (length content) max-chars))
            (subseq content 0 max-chars)
            content)))))

(defun write-state-file (filename content)
  "Write CONTENT to a state file.  Creates parent directories if needed."
  (let ((path (state-file-path filename)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-string content s))
    #+sbcl (sb-posix:chmod (namestring (truename path)) #o600)
    path))

;;; --- Default file creation ---

(defun ensure-default-state-files ()
  "Create default SOUL.org and USER.org if they don't exist.
Called at daemon startup."
  (let ((created nil))
    (unless (probe-file (state-file-path "SOUL.org"))
      (write-state-file "SOUL.org" *default-soul*)
      (log:info "Created default state/SOUL.org")
      (push "SOUL.org" created))
    (unless (probe-file (state-file-path "USER.org"))
      (write-state-file "USER.org" *default-user*)
      (log:info "Created default state/USER.org")
      (push "USER.org" created))
    created))

;;; --- Bootstrap loading ---

(defun session-type-filter (session-type)
  "Return the filter key for SESSION-TYPE.
:main → :full, :channel/:subagent → :minimal."
  (case session-type
    (:main     :full)
    (:channel  :minimal)
    (:subagent :minimal)
    (otherwise :full)))

(defun load-bootstrap-files (&key (session-type :main))
  "Load bootstrap files appropriate for SESSION-TYPE.
Returns an alist of (name . content) pairs for files that exist and pass
the session-type filter."
  (let ((filter (session-type-filter session-type))
        (max-chars (crichton/config:config-section-get :state :max-file-chars 20000))
        (result nil))
    (dolist (spec *bootstrap-files*)
      (when (getf spec filter)
        (let ((content (read-state-file (getf spec :filename) max-chars)))
          (when (and content (plusp (length content)))
            (push (cons (getf spec :name) content) result)))))
    (nreverse result)))

(defun filter-bootstrap-for-session (files session-type)
  "Filter an already-loaded file alist for SESSION-TYPE.
Useful when you have the full set and need to restrict it."
  (let ((filter (session-type-filter session-type)))
    (loop for (name . content) in files
          for spec = (find name *bootstrap-files*
                           :key (lambda (s) (getf s :name))
                           :test #'string-equal)
          when (and spec (getf spec filter))
            collect (cons name content))))

;;; --- System prompt assembly ---

(defun format-bootstrap-section (name content)
  "Format a single bootstrap file as a system prompt section."
  (format nil "--- ~A ---~%~A" name content))

(defun bootstrap-system-prompt (&key (session-type :main)
                                     (base-prompt nil))
  "Build a composite system prompt by appending bootstrap file contents
to BASE-PROMPT (defaults to the agent's *default-system-prompt*).

Respects the configured total character budget for bootstrap content."
  (let* ((base (or base-prompt
                   (symbol-value
                    (find-symbol "*DEFAULT-SYSTEM-PROMPT*"
                                :crichton/agent))))
         (files (load-bootstrap-files :session-type session-type))
         (max-total (crichton/config:config-section-get
                     :state :max-total-chars 150000))
         (sections nil)
         (total 0))
    (when (null files)
      (return-from bootstrap-system-prompt base))
    (dolist (pair files)
      (let* ((section (format-bootstrap-section (car pair) (cdr pair)))
             (len (length section)))
        (when (> (+ total len) max-total)
          (log:warn "Bootstrap content exceeds budget (~D chars), truncating"
                    max-total)
          (return))
        (push section sections)
        (incf total len)))
    (format nil "~A~%~%~{~A~%~}" base (nreverse sections))))
