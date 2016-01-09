;;;; Implement some wiki operations
(in-package :antimer.wiki)

(defmethod start ((wiki wiki))
  (let ((port 8000)
        (*wiki* wiki))
    (format t "Starting wiki on port ~D" port)
    ;(lucerne:start antimer.app:app :port port)
    (format t "Server started~%")
    (loop (read))))
