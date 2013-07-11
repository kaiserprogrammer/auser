(in-package :auser)

(defvar *user-db*)

(defclass user ()
  ((id :initarg :id
       :reader id)
   (password :initarg :password
             :reader password)))

(defparameter *hasher*
  (lambda (password)
    (ironclad:pbkdf2-hash-password-to-combined-string
     (ironclad:ascii-string-to-byte-array password))))

(defparameter *checker*
  (lambda (hash password)
    (ironclad:pbkdf2-check-password
     (ironclad:ascii-string-to-byte-array password)
     hash)))

(define-condition user-already-exists (error)
  ((id :accessor id :initarg :id))
  (:report (lambda (c s)
             (format s "USER with id ~s already exists." (id c)))))

(define-condition user-does-not-exist (error)
  ((id :accessor id :initarg :id))
  (:report (lambda (c s)
             (format s "NO USER with id ~s exists." (id c)))))

(define-condition invalid-password (error)
  ((id :accessor id :initarg :id)
   (invalid-password :accessor password :initarg :invalid-password))
  (:report (lambda (c s)
             (format s "USER with id ~s does not have password ~s." (id c) (password c)))))

(defmethod (setf password) (pw (user user))
  (setf (slot-value user 'password) (funcall *hasher* pw)))

(defmethod initialize-instance :after ((u user) &key)
  (when (slot-boundp u 'password)
    (setf (password u) (password u))))

(defun add-user (id password &optional (db *user-db*))
  (let ((user (make-instance 'user
                             :id id
                             :password password)))
    (db-add-user user db)))

(defun verify-user (id password &optional (db *user-db*))
  (if (funcall *checker* (password (db-get-user id db)) password)
      t
      (error 'invalid-password :invalid-password password :id id)))

(defun update-password (id password &optional (db *user-db*))
  (setf (password (db-get-user id db)) password))
