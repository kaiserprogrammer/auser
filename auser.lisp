(in-package :auser)

(defvar *user-db*)

(defun blank? (text)
  (not (and text (> (length text) 0))))

(defparameter *hasher*
  (lambda (password)
    (if (blank? password)
        (error 'empty-password)
        (ironclad:pbkdf2-hash-password-to-combined-string
         (ironclad:ascii-string-to-byte-array password)))))

(defparameter *checker*
  (lambda (hash password)
    (if (blank? password)
        (error 'empty-password)
        (ironclad:pbkdf2-check-password
         (ironclad:ascii-string-to-byte-array password)
         hash))))

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

(define-condition empty-password (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Empty Password is not allowed"))))

(defun add-user (id password &optional (db *user-db*))
  (db-add-user db id (funcall *hasher* password)))

(defun verify-user (id password &optional (db *user-db*))
  (unless (funcall *checker* (second (db-get-user db id)) password)
    (error 'invalid-password :invalid-password password :id id)))

(defun update-password (id password &optional (db *user-db*))
  (db-update-password db id (funcall *hasher* password)))
