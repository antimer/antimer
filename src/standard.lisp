(in-package :cl-user)
(defpackage antimer.standard
  (:use :cl :antimer.wiki)
  (:documentation "The standard Antimer Wiki class."))
(in-package :antimer.standard)

;;; Class

(defclass standard-wiki (wiki)
  ()
  (:documentation "The standard wiki class."))

;;; Methods

(defmethod initialize-instance :after ((instance standard-wiki) &key)
  "After initializing the instance, install the plugins."
  (setf (wiki-plugins instance)
        (append (list (make-instance 'antimer.plugins.fonts:fonts-plugin))
                (wiki-plugins instance))))
