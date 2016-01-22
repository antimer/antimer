(in-package :cl-user)
(defpackage antimer.djula
  (:use :cl)
  (:documentation "Extends Djula."))
(in-package :antimer.djula)

(djula:def-tag-compiler antimer-wiki-name ()
  (lambda (stream)
    (write-string (antimer.wiki:wiki-name antimer.wiki:*wiki*) stream)))
