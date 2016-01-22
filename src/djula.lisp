(in-package :cl-user)
(defpackage antimer.djula
  (:use :cl)
  (:documentation "Extends Djula."))
(in-package :antimer.djula)

(djula:def-tag-compiler antimer-wiki-name ()
  (lambda (stream)
    (write-string (antimer.wiki:wiki-name antimer.wiki:*wiki*) stream)))

(djula:def-tag-compiler antimer-get-user ()
  (lambda (stream)
    (declare (ignore stream))
    (setf (getf djula::*template-arguments* :user)
          (lucerne-auth:get-userid))))
