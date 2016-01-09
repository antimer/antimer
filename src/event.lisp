(in-package :cl-user)
(defpackage antimer.event
  (:use :cl)
  (:export :event
           :send
           :startup
           :shutdown
           :send)
  (:documentation "Antimer's event system."))
(in-package :antimer.event)

(defclass event ()
  ()
  (:documentation "The base class of all events."))

(defgeneric send (event)
  (:documentation "Send an event."))

;;; Built-in events

(defclass startup (event)
  ()
  (:documentation "Emitted when the server is started."))

(defclass shutdown (event)
  ()
  (:documentation "Emitted when the server is shut down."))
