(in-package :auser)

(define-condition user-already-exists (error)
  ((id :accessor id :initarg :id))
  (:report (lambda (c s)
             (format s "USER with id ~s already exists." (id c)))))

(define-condition user-does-not-exist (error)
  ((id :accessor id :initarg :id))
  (:report (lambda (c s)
             (format s "NO USER with id ~s exists." (id c)))))

(defun add (id password db)
  (db-add-password db id (hash password)))

(defun verify (id password db)
  (check password (db-get-password db id)))

(defun update (id password db)
  (db-update-password db id (hash password)))
