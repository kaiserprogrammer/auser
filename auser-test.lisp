(defpackage :auser-test
  (:use :cl :auser :lisp-unit))

(in-package :auser-test)

(remove-tests :all)

(defvar *test-db-creator*)

(defmethod auser::db-get-user ((db hash-table) id)
  (let ((password (gethash id db)))
    (if password
        (list id password)
        (error 'user-does-not-exist :id id))))

(defmethod auser::db-add-user ((db hash-table) id password)
  (restart-case
      (if (gethash id db)
          (error 'user-already-exists :id id)
          (setf (gethash id db) password))
    (overwrite-user ()
      (setf (gethash id db) password))))

(defmethod auser::db-update-password ((db hash-table) id password)
  (setf (gethash id db) password))

(defmacro with-setup (&body body)
  `(let ((db (funcall *test-db-creator*)))
     (add-user "blub" "secret" db)
     ,@body))

(define-test adding-a-user
  (with-setup
    (let ((user (auser::db-get-user db "blub")))
      (assert-false (null user))
      (assert-true (string= "blub" (first user)))
      (assert-false (string= "secret" (second user))))))

(define-test verifying-password
  (with-setup
    (verify-user "blub" "secret" db)
    (assert-error 'invalid-password (verify-user "blub" "wrong" db))))

(define-test no-password
  (with-setup
    (assert-error 'empty-password (verify-user "blub" "" db))
    (assert-error 'empty-password (add-user "empty_password_user" "" db))))

(define-test no-overwriting-of-user
  (with-setup
    (assert-error 'auser::user-already-exists (add-user "blub" "secret" db))))

(define-test updating-password
  (with-setup
    (update-password "blub" "new_password" db)
    (verify-user "blub" "new_password" db)
    (assert-error 'invalid-password (verify-user "blub" "wrong" db))))

(define-test not-existing-user
  (with-setup
    (assert-error 'user-does-not-exist (verify-user "not existing" "secret" db))))

(defmacro run-tests-for-auser-with-db (create)
  `(let ((*print-failures* t)
         (*print-errors* t))
     (let ((*test-db-creator* (lambda () ,create)))
       (run-tests :all))))

(run-tests-for-auser-with-db (make-hash-table :test 'equal))
