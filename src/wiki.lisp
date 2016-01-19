(in-package :cl-user)
(defpackage antimer.wiki
  (:use :cl)
  (:export :*wiki*
           :wiki
           :wiki-name
           :wiki-directory
           :wiki-config
           :start
           :stop
           :apply-events
           :wiki-config-pathname)
  (:documentation "The wiki object."))
(in-package :antimer.wiki)

(defvar *wiki*)

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
           :type antimer.config:config
           :documentation "The wiki configuration."))
  (:documentation "A wiki."))

(defgeneric start (wiki)
  (:documentation "Start the wiki."))

(defgeneric stop (wiki)
  (:documentation "Send the shutdown event to every plugin."))

(defgeneric apply-events (wiki event)
  (:documentation "Go through every plugin in the wiki, sending an event to
  it."))

(defgeneric wiki-config-pathname (wiki)
  (:documentation "Return the path to the config file.")

  (:method ((wiki wiki))
    (merge-pathnames #p"config.yaml" (wiki-directory wiki))))

(defgeneric wiki-static-directory (wiki)
  (:documentation "Return the absolute pathname to the wiki's static files
  directory.")

  (:method ((wiki wiki))
    (merge-pathnames #p"static/" (wiki-directory wiki))))
