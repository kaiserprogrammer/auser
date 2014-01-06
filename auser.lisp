(in-package :auser)

(defun blank? (text)
  (not (and text (> (length text) 0))))

(defun default-hasher (password)
  (if (blank? password)
      (error 'empty-password)
      (ironclad:pbkdf2-hash-password-to-combined-string
       (ironclad:ascii-string-to-byte-array password))))

(defun default-checker (hash password)
  (if (or (not (stringp password))
          (blank? password)
          (not (stringp hash)))
      (error 'empty-password)
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

(define-condition empty-password (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "An Empty Password is not allowed"))))

(defun add (id password db)
  (db-add db id (funcall *hasher* password)))

(defun verify (id password db &key (checker #'default-checker))
  (unless (funcall checker (db-get-password db id) password)
    (error 'invalid-password :invalid-password password :id id)))

(defun update (id password db &key (hasher #'default-hasher))
  (db-update db id (funcall hasher password)))
