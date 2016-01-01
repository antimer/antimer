(in-package :cl-user)
(defpackage antimer.wiki
  (:use :cl)
  (:export :wiki
           :wiki-directory
           :database-directory
           :first-time-p
           :*wiki*)
  (:documentation "The definition of the wiki object."))
(in-package :antimer.wiki)

(defclass wiki ()
  ((directory :reader wiki-directory
              :initarg :directory
              :type pathname
              :documentation "The absolute pathname to the wiki directory."))
  (:documentation "A wiki."))

(defmethod database-directory ((wiki wiki))
  (uiop:merge-pathnames* #p"_db/"
                         (wiki-directory wiki)))

(defmethod first-time-p ((wiki wiki))
  "Is this the first time the wiki has started?"
  (not (probe-file (database-directory wiki))))

(defvar *wiki*)
