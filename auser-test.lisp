(defpackage :auser-test
  (:use :cl :auser :lisp-unit))

(in-package :auser-test)

(remove-tests :all)

(defvar *tested-db*)

(defmacro with-setup (&body body)
  `(let ((*user-db* (make-instance *tested-db*)))
     (add-user "blub" "secret")
     ,@body))

(define-test adding-a-user
  (with-setup
    (let ((user (auser::db-get-user *user-db* "blub")))
      (assert-false (null user))
      (assert-true (string= "blub" (first user)))
      (assert-false (string= "secret" (second user))))))

(define-test verifying-password
  (with-setup
    (verify-user "blub" "secret")
    (assert-error 'invalid-password (verify-user "blub" "wrong"))))

(define-test no-password
  (with-setup
    (assert-error 'empty-password (verify-user "blub" ""))
    (assert-error 'empty-password (add-user "empty_password_user" ""))))

(define-test no-overwriting-of-user
  (with-setup
    (assert-error 'auser::user-already-exists (add-user "blub" "secret"))))

(define-test updating-password
  (with-setup
    (update-password "blub" "new_password")
    (verify-user "blub" "new_password")
    (assert-error 'invalid-password (verify-user "blub" "wrong"))))

(define-test not-existing-user
  (with-setup
    (assert-error 'user-does-not-exist (verify-user "not existing" "secret"))))


(let ((*print-failures* t)
      (*print-errors* t))
  (dolist (*tested-db* '(memory-db))
    (run-tests :all)))
