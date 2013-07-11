(in-package :auser)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)))

(defmethod db-get-user (id (db memory-db))
  (let ((user (gethash id (users db))))
    (if user
        user
        (error 'user-does-not-exist :id id))))

(defmethod db-add-user ((user user) (db memory-db))
  (restart-case
      (if (gethash (id user) (users db))
          (error 'user-already-exists :id (id user))
          (setf (gethash (id user) (users db)) user))
    (overwrite-user ()
      (setf (gethash (id user) (users db)) user))))

