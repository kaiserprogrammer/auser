(defpackage :auser
  (:use :cl :apassword)
  (:export
   :add
   :verify
   :update
   :user-does-not-exist
   :user-already-exists
   :invalid-password
   :invalid-hash
   :empty-password
   :empty-hash))
