(in-package :cl-user)
(defpackage antimer.model.db
  (:use :cl)
  (:export :database
           :create-database
           :connect
           :disconnect
           :*db*)
  (:documentation "Generic database interface."))
(in-package :antimer.model.db)

(defclass database ()
  ()
  (:documentation "The base class of Antimer databases."))

(defgeneric create-database (database)
  (:documentation "Create the database."))

(defgeneric connect (database)
  (:documentation "Connect to the database."))

(defgeneric disconnect (database)
  (:documentation "Disconnect from the database."))

(defvar *db*)
