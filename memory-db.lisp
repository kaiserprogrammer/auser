(in-package :auser)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)))

(defmethod db-get-user ((db memory-db) id)
  (let ((user (gethash id (users db))))
    (if user
        user
        (error 'user-does-not-exist :id id))))

(defmethod db-add-user ((db memory-db) (user user))
  (restart-case
      (if (gethash (id user) (users db))
          (error 'user-already-exists :id (id user))
          (setf (gethash (id user) (users db)) user))
    (overwrite-user ()
      (setf (gethash (id user) (users db)) user))))

