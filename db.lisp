(in-package :auser)

(defgeneric db-add-user (db id password))
(defgeneric db-get-password (db id))
(defgeneric db-update-password (db id password))
