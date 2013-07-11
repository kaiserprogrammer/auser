(defpackage :auser
  (:use :cl)
  (:export
   :add-user
   :verify-user
   :*user-db*
   :*hasher*
   :*checker*
   :memory-db
   :update-password
   :user-does-not-exist
   :user-already-exists))
