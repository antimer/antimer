(in-package :cl-user)
(defpackage antimer.theme.default
  (:use :cl)
  (:import-from :antimer.plugin
                :plugin
                :name
                :short-description
                :data-directory
                :on-event)
  (:import-from :antimer.event
                :startup
                :shutdown)
  (:export :default-theme)
  (:documentation "The default theme."))
(in-package :antimer.theme.default)

;;; Plugin definition

(defclass default-theme (plugin)
  ()
  (:documentation "The default theme plugin."))

(defmethod name ((plugin default-theme))
  "Default Theme")

(defmethod short-description ((plugin default-theme))
  "The default theme.")

(antimer.config:register-default-plugin (make-instance 'default-theme))

;;; Events

(defparameter +stylesheet+
  (lass:generate
   (asdf:system-relative-pathname :antimer #p"src/theme/default/style.lass")))

(defmethod on-event ((plugin default-theme) (event startup))
  "On startup, download all external assets and compile local ones unless they
already exist."
  (antimer.log:info :static "Copying stylesheet")
  (antimer.static:copy-file +stylesheet+ #p"css/style.css"))
