(in-package :cl-user)
(defpackage antimer.event
  (:use :cl)
  (:export :event
           :startup
           :shutdown)
  (:documentation "Antimer's event system."))
(in-package :antimer.event)

(defclass event ()
  ()
  (:documentation "The base class of all events."))

;;; Built-in events

(defclass startup ()
  ()
  (:documentation "Emitted when the server is started."))

(defclass shutdown ()
  ()
  (:documentation "Emitted when the server is shut down."))
