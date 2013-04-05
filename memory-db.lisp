(in-package :auser)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)))

(defmethod db-get-user (id (db memory-db))
  (gethash id (users db)))

(defmethod db-add-user ((user user) (db memory-db))
  (setf (gethash (id user) (users db)) user))
