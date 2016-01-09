(in-package :cl-user)
(defpackage antimer.wiki
  (:use :cl)
  (:export :*wiki*
           :wiki
           :wiki-directory
           :start)
  (:documentation "The wiki object."))
(in-package :antimer.wiki)

(defvar *wiki*)

(defclass wiki ()
  ((directory :reader wiki-directory
              :initarg :directory
              :type pathname
              :documentation "The absolute pathname to the wiki directory.")
   (plugins :reader wiki-plugins
            :initarg :plugins
            :type list
            :documentation "A list of plugin instances."))
  (:documentation "A wiki."))

(defgeneric start (wiki)
  (:documentation "Start the wiki."))
