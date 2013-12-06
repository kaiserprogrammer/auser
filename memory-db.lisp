(in-package :auser)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)))

(defmethod db-get-user ((db memory-db) id)
  (let ((user (gethash id (users db))))
    (if user
        user
        (error 'user-does-not-exist :id id))))

(defmethod db-add-user ((db memory-db) id password)
  (restart-case
      (if (gethash id (users db))
          (error 'user-already-exists :id id)
          (setf (gethash id (users db)) (list id password)))
    (overwrite-user ()
      (setf (gethash id (users db)) (list id password)))))

(defmethod db-update-password ((db memory-db) id password)
  (setf (gethash id (users db)) (list id password)))
