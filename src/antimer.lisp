(in-package :cl-user)
(defpackage antimer
  (:use :cl)
  (:import-from :antimer.wiki
                :*wiki*
                :wiki
                :start)
  (:documentation "The top-level loop."))
(in-package :antimer)

(defmethod start ((wiki wiki))
  (let ((port 8000)
        (*wiki* wiki))
    (format t "Starting wiki on port ~D" port)
    ;(lucerne:start antimer.app:app :port port)
    (format t "Server started~%")
    (loop (read))))
