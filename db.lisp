(in-package :auser)

(defgeneric db-add-user (db id password))
(defgeneric db-get-user (db id))
(defgeneric db-update-password (db id password))
