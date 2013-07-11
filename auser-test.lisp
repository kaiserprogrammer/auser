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
    (let ((user (auser::db-get-user "blub" *user-db*)))
      (assert-false (null user))
      (assert-true (string= "blub" (auser::id user)))
      (assert-false (string= "secret" (auser::password user))))))

(define-test verifying-password
  (with-setup
    (assert-true (verify-user "blub" "secret"))
    (assert-error 'invalid-password (verify-user "blub" "wrong"))))

(define-test no-password
  (with-setup
    (assert-error 'invalid-password (verify-user "blub" ""))))

(define-test no-overwriting-of-user
  (with-setup
    (assert-error 'auser::user-already-exists (add-user "blub" "secret"))))

(define-test updating-password
  (with-setup
    (update-password "blub" "new_password")
    (assert-true (verify-user "blub" "new_password"))
    (assert-error 'invalid-password (verify-user "blub" "wrong"))))

(define-test not-existing-user
  (with-setup
    (assert-error 'user-does-not-exist (verify-user "not existing" "secret"))))


(let ((*print-failures* t)
      (*print-errors* t))
  (dolist (*tested-db* '(memory-db))
    (run-tests :all)))
