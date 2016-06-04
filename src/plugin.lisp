(in-package :cl-user)
(defpackage antimer.plugin
  (:use :cl)
  (:import-from :antimer.wiki
                :wiki)
  ;; Classes
  (:export :plugin
           :static-generator)
  ;; Methods
  (:export :name
           :description
           :generate)
  (:documentation "Antimer's plugin system."))
(in-package :antimer.plugin)

;;; Classes

(defclass plugin ()
  ()
  (:documentation "The base class of Antimer plugins."))

(defclass static-generator (plugin)
  ()
  (:documentation "The base class of plugins that generate static files."))

;;; Methods for all plugins

(defgeneric name (plugin)
  (:documentation "Returns the human-readable name of a plugin, a string.")

  (:method ((plugin plugin))
    "The default method: return the class name."
    (string-capitalize (class-name (class-of plugin)))))

(defgeneric description (plugin)
  (:documentation "Return a short, one-line description of the plugin.
Return @c(nil) to indicate there is no description.")

  (:method ((plugin plugin))
    "The default method: return @c(nil)."
    nil))

;;; Static Generator Methods

(defgeneric generate (wiki plugin)
  (:documentation "Generate static files.")

  (:method ((wiki wiki) (plugin static-generator))
    "Default method: do nothing."
    (values)))
