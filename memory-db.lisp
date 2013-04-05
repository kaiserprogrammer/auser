(in-package :auser)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)))

(defmethod db-get-user (id (db memory-db))
  (gethash id (users db)))

(defmethod db-update-user (id password ))

(defmethod db-add-user ((user user) (db memory-db))
  (restart-case
      (if (gethash (id user) (users db))
          (error 'user-already-exists :id (id user))
          (setf (gethash (id user) (users db)) user))
    (overwrite-user ()
      (setf (gethash (id user) (users db)) user))))

