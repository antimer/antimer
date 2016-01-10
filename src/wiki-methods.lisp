;;;; Implement some wiki operations
(in-package :antimer.wiki)

(defmethod start ((wiki wiki))
  (setf *wiki* wiki)
  (send (make-instance 'antimer.event:startup)))

(defmethod stop ((wiki wiki))
  (send (make-instance 'antimer.event:shutdown)))

(defmethod apply-events ((wiki wiki) (event antimer.event:event))
  (with-slots (plugins) wiki
    (loop for plugin in plugins do
      (antimer.plugin:on-event plugin event))))

(defmethod send ((event antimer.event:event))
  (apply-events *wiki* event))

(defmethod antimer.plugin:on-event :before ((plugin antimer.plugin:plugin)
                                            (event antimer.event:startup))
  (ensure-directories-exist
   (antimer.plugin:data-directory plugin)))
