(in-package :cl-user)
(defpackage antimer.wiki
  (:use :cl)
  (:export :wiki
           :*wiki*)
  ;; Accessors
  (:export :wiki-name
           :wiki-directory
           :wiki-config)
  ;; Methods
  (:export :wiki-config-pathname
           :wiki-static-directory)
  (:documentation "The wiki object."))
(in-package :antimer.wiki)

;;; Classes

(defclass wiki ()
  ((name :reader wiki-name
         :initarg :name
         :initform "Antimer Wiki"
         :type string
         :documentation "The name of the wiki.")
   (directory :reader wiki-directory
              :initarg :directory
              :type pathname
              :documentation "The absolute pathname to the wiki directory.")
   (config :reader wiki-config
           :initarg :config
           :type hash-table
           :documentation "The wiki configuration."))
  (:documentation "The base wiki class."))

(defvar *wiki*)

;;; Methods

(defgeneric wiki-config-pathname (wiki)
  (:documentation "Return the absolute path to the config file.")

  (:method ((wiki wiki))
    (merge-pathnames #p"config.yaml" (wiki-directory wiki))))

(defgeneric wiki-static-directory (wiki)
  (:documentation "Return the absolute pathname to the wiki's static files
  directory.")

  (:method ((wiki wiki))
    (merge-pathnames #p"static/" (wiki-directory wiki))))
