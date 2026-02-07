;;;; sessions/retention.lisp
;;;;
;;;; Session retention policy enforcement.
;;;; Purges expired sessions based on configured retention-days.
;;;; Called at daemon startup and optionally on a periodic timer.

(in-package #:crichton/sessions)

(defun retention-days ()
  "Return the configured retention period in days.
   0 means no-store mode. NIL or negative means keep forever."
  (let ((days (crichton/config:config-section-get :sessions :retention-days 30)))
    (cond
      ((null days) nil)
      ((and (numberp days) (<= days 0)) 0)
      ((numberp days) days)
      (t 30))))

(defun file-age-days (path)
  "Return the age of PATH in days based on file modification time."
  (let* ((write-date (file-write-date path))
         (now (get-universal-time))
         (age-seconds (- now write-date)))
    (/ age-seconds 86400.0)))

(defun purge-expired-sessions ()
  "Delete session files older than the retention period.
   In no-store mode (retention-days = 0), deletes ALL sessions.
   Returns the number of sessions purged."
  (let ((days (retention-days))
        (purged 0))
    (when (null days)
      (return-from purge-expired-sessions 0))
    (let ((pattern (merge-pathnames
                    (make-pathname :name :wild :type "age")
                    (sessions-dir))))
      (dolist (path (directory pattern))
        (let ((should-delete
                (if (zerop days)
                    t
                    (> (file-age-days path) days))))
          (when should-delete
            (handler-case
                (progn
                  (delete-file path)
                  (incf purged)
                  (log:info "Purged expired session: ~A" (pathname-name path)))
              (error (c)
                (log:warn "Failed to purge session ~A: ~A"
                          (pathname-name path) c)))))))
    (when (plusp purged)
      (log:info "Purged ~D expired session~:P" purged))
    purged))
