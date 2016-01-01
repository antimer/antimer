(in-package :cl-user)
(defpackage antimer
  (:use :cl)
  (:export :start)
  (:documentation "The top-level interface."))
(in-package :antimer)

(defun start (wiki &key (port 8000))
  "Start a wiki server."
  (setf antimer.wiki:*wiki* wiki)
  (format t "Starting wiki on port ~D" port)
  (lucerne:start antimer.app:app :port port)
  (format t "Server started~%")
  (when (antimer.wiki:first-time-p wiki)
    (format t "Go to http://localhost:~D/setup to set up Antimer~%" port))
  (loop (read)))
