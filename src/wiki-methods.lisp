;;;; Implement some wiki operations
(in-package :antimer.wiki)

(defmethod start ((wiki wiki))
  (let ((port 8000)
        (*wiki* wiki))
    (format t "Starting wiki on port ~D" port)
    ;(lucerne:start antimer.app:app :port port)
    (format t "Server started~%")
    (loop (read))))

(defmethod apply-events ((wiki wiki) (event antimer.event:event))
  (with-slots (plugins) wiki
    (loop for plugin in plugins do
      (antimer.plugin:on-event plugin event))))

(defmethod send ((event antimer.event:event))
  (apply-events *wiki* event))
