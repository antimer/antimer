(in-package :cl-user)
(defpackage antimer.event
  (:use :cl)
  (:export :event
           :startup
           :shutdown
           :send)
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

(defgeneric send (event)
  (:documentation "Send an event."))
