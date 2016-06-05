(in-package :cl-user)
(defpackage antimer.wiki
  (:use :cl)
  ;; Classes
  (:export :wiki)
  ;; Accessors
  (:export :wiki-name
           :wiki-directory
           :wiki-config
           :wiki-plugins)
  ;; Methods
  (:export :wiki-config-pathname
           :wiki-articles-directory
           :wiki-static-directory
           :wiki-build-directory
           :wiki-temporary-directory)
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
           :documentation "The wiki configuration.")
   (plugins :accessor wiki-plugins
            :initarg :plugins
            :type list
            :documentation "A list of plugin instances."))
  (:documentation "The base wiki class."))

;;; Methods

(defgeneric wiki-config-pathname (wiki)
  (:documentation "Return the absolute path to the config file.")

  (:method ((wiki wiki))
    (merge-pathnames #p"config.yaml" (wiki-directory wiki))))

(defgeneric wiki-articles-directory (wiki)
  (:documentation "Return the absolute pathname to the wiki's article directory.")

  (:method ((wiki wiki))
    (merge-pathnames #p"articles/" (wiki-directory wiki))))

(defgeneric wiki-static-directory (wiki)
  (:documentation "Return the absolute pathname to the wiki's static files
  directory.")

  (:method ((wiki wiki))
    (merge-pathnames #p"static/" (wiki-directory wiki))))

(defgeneric wiki-build-directory (wiki)
  (:documentation "The absolute pathname to the wiki's build directory.")

  (:method ((wiki wiki))
    (merge-pathnames #p"build/" (wiki-directory wiki))))

(defgeneric wiki-temporary-directory (wiki)
  (:documentation "The absolute pathname to the wiki's temporary directory.")

  (:method ((wiki wiki))
    (merge-pathnames #p"_temp/" (wiki-directory wiki))))
