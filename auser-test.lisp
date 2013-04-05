(defpackage :auser-test
  (:use :cl :auser :lisp-unit))

(in-package :auser-test)

(remove-tests :all)

(define-test adding-a-user
  (let ((*user-db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((user (auser::db-get-user "blub" *user-db*)))
      (assert-false (null user))
      (assert-true (string= "blub" (auser::id user)))
      (assert-false (string= "secret" (auser::password user))))))

(define-test verifying-password
  (let ((*user-db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (assert-true (verify-user "blub" "secret"))
    (assert-false (verify-user "blub" "wrong"))))


(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
