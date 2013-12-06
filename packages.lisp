(defpackage :auser
  (:use :cl)
  (:export
   :add-user
   :verify-user
   :update-password
   :*user-db*
   :user-does-not-exist
   :user-already-exists
   :invalid-password
   :empty-password))
