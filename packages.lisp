(defpackage :auser
  (:use :cl)
  (:export
   :add-user
   :id
   :password
   :user
   :verify-user
   :*user-db*
   :*hasher*
   :*checker*
   :memory-db
   :update-password
   :user-does-not-exist
   :user-already-exists
   :invalid-password
   :empty-password))
