(asdf:defsystem auser
  :version "0"
  :description "unifiy user authentification"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "MIT"
  :depends-on (apassword)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "packages")
               (:file "db")
               (:file "auser")))
