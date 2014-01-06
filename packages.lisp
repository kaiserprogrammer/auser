(defpackage :auser
  (:use :cl)
  (:export
   :add
   :verify
   :update
   :user-does-not-exist
   :user-already-exists
   :invalid-password
   :empty-password
   :empty-password-hash))
