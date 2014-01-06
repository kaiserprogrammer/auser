(in-package :auser)

(defgeneric db-add-password (db id password))
(defgeneric db-get-password (db id))
(defgeneric db-update (db id password))
