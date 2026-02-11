;;;; skills/builtins/task-store.lisp
;;;;
;;;; Age-encrypted persistence for user-defined scheduled tasks.
;;;; Stores tasks to ~/.crichton/tasks/user-tasks.age so they survive
;;;; daemon restarts.

(in-package #:crichton/skills)

;;; --- Predicates ---

(defun user-task-p (task)
  "Return T if TASK is a user-defined task (name starts with 'user:')."
  (and (stringp (scheduled-task-name task))
       (>= (length (scheduled-task-name task)) 5)
       (string= "user:" (subseq (scheduled-task-name task) 0 5))))

;;; --- File path ---

(defun user-tasks-file-path ()
  "Return path to ~/.crichton/tasks/user-tasks.age"
  (merge-pathnames "tasks/user-tasks.age" crichton/config:*agent-home*))

;;; --- Serialization ---

(defun task-to-plist (task)
  "Convert a scheduled-task to a serializable plist."
  (list :name (scheduled-task-name task)
        :kind (scheduled-task-kind task)
        :action-name (scheduled-task-action-name task)
        :interval-seconds (scheduled-task-interval-seconds task)
        :daily-hour (scheduled-task-daily-hour task)
        :daily-minute (scheduled-task-daily-minute task)
        :next-ut (scheduled-task-next-ut task)))

(defun task-plists-to-json-bytes (plists)
  "Serialize a list of task plists into JSON bytes wrapped in an envelope."
  (let ((envelope (make-hash-table :test #'equal))
        (tasks-vec (make-array (length plists))))
    (setf (gethash "version" envelope) 1
          (gethash "saved_at" envelope) (format-ut (get-universal-time)))
    (loop for plist in plists
          for i from 0
          do (let ((ht (make-hash-table :test #'equal)))
               (setf (gethash "name" ht) (getf plist :name)
                     (gethash "kind" ht) (string-downcase
                                          (symbol-name (getf plist :kind)))
                     (gethash "action_name" ht) (getf plist :action-name)
                     (gethash "interval_seconds" ht) (getf plist :interval-seconds)
                     (gethash "daily_hour" ht) (getf plist :daily-hour)
                     (gethash "daily_minute" ht) (getf plist :daily-minute)
                     (gethash "next_ut" ht) (getf plist :next-ut))
               (setf (aref tasks-vec i) ht)))
    (setf (gethash "tasks" envelope) tasks-vec)
    (sb-ext:string-to-octets
     (let ((*print-pretty* nil))
       (with-output-to-string (s)
         (shasht:write-json envelope s)))
     :external-format :utf-8)))

(defun json-bytes-to-task-plists (bytes)
  "Deserialize JSON bytes back to a list of task plists."
  (let* ((json-string (sb-ext:octets-to-string bytes :external-format :utf-8))
         (envelope (shasht:read-json json-string)))
    (when (hash-table-p envelope)
      (let ((tasks (gethash "tasks" envelope)))
        (when tasks
          (map 'list
               (lambda (ht)
                 (let ((kind-str (gethash "kind" ht)))
                   (list :name (gethash "name" ht)
                         :kind (intern (string-upcase kind-str) :keyword)
                         :action-name (gethash "action_name" ht)
                         :interval-seconds (gethash "interval_seconds" ht)
                         :daily-hour (gethash "daily_hour" ht)
                         :daily-minute (gethash "daily_minute" ht)
                         :next-ut (gethash "next_ut" ht))))
               tasks))))))

;;; --- Save / Load ---

(defun save-user-tasks (tasks)
  "Persist a list of scheduled-task structs to encrypted storage.
   Only saves user tasks (user-task-p)."
  (let* ((user-tasks (remove-if-not #'user-task-p tasks))
         (plists (mapcar #'task-to-plist user-tasks))
         (json-bytes (task-plists-to-json-bytes plists))
         (path (user-tasks-file-path)))
    (crichton/crypto:encrypt-to-file json-bytes path)
    (log:info "Saved ~D user tasks to ~A" (length plists) path)
    (length plists)))

(defun load-user-tasks ()
  "Load persisted user tasks from encrypted storage.
   Returns a list of plists (NOT scheduled-task structs — caller must reconstruct).
   Returns NIL if file doesn't exist."
  (let ((path (user-tasks-file-path)))
    (unless (probe-file path)
      (return-from load-user-tasks nil))
    (let* ((plaintext-bytes (crichton/crypto:decrypt-from-file path))
           (plists (json-bytes-to-task-plists plaintext-bytes)))
      (log:info "Loaded ~D user tasks from ~A" (length plists) path)
      plists)))
