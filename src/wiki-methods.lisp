;;;; Implement some wiki operations
(in-package :antimer.wiki)

(defmethod start ((wiki wiki))
  (setf *wiki* wiki)
  (setf (slot-value wiki 'config)
        (antimer.config:parse (wiki-config-pathname wiki)))
  (send (make-instance 'antimer.event:startup)))

(defmethod stop ((wiki wiki))
  (send (make-instance 'antimer.event:shutdown)))

(defmethod apply-events ((wiki wiki) (event antimer.event:event))
  (with-slots (config) wiki
    (loop for plugin in (antimer.config:config-plugins config) do
      (antimer.plugin:on-event plugin event))))

(defmethod send ((event antimer.event:event))
  (apply-events *wiki* event))

(defmethod antimer.plugin:on-event :before ((plugin antimer.plugin:plugin)
                                            (event antimer.event:startup))
  (when (slot-boundp plugin 'antimer.plugin::directory-name)
    (ensure-directories-exist
     (antimer.plugin:data-directory plugin))))
