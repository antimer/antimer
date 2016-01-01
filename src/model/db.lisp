(in-package :cl-user)
(defpackage antimer.model.db
  (:use :cl)
  (:export :*db*)
  (:documentation "Generic database interface."))
(in-package :antimer.model.db)

(defvar *db*)
