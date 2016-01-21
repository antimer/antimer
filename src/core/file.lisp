(in-package :cl-user)
(defpackage antimer.file
  (:use :cl)
  (:import-from :antimer.wiki
                :*wiki*
                :wiki-directory)
  (:import-from :antimer.plugin
                :plugin
                :name
                :short-description
                :data-directory
                :on-event)
  (:import-from :antimer.event
                :startup
                :shutdown)
  (:export :file-store
           :file-exists-p
           :file-path)
  (:documentation "The default file store."))
(in-package :antimer.file)

;;; Plugin definition

(defclass file-store (plugin)
  ()
  (:default-initargs
   :directory-name "files")
  (:documentation "The file store plugin."))

(defmethod name ((plugin file-store))
  "File Storage")

(defmethod short-description ((plugin file-store))
  "This plugin manages file storage.")

(antimer.config:register-default-plugin (make-instance 'file-store))

;;; Methods

(defun files-directory ()
  (merge-pathnames #p"files/" (wiki-directory *wiki*)))

(defun file-exists-p (filename)
  (handler-case
      (if (probe-file (merge-pathnames (parse-namestring filename)
                                       (files-directory)))
          t)
    (t () nil)))

(defun file-path (filename)
  (handler-case
      (let ((parsed (parse-namestring filename)))
        (when parsed
          (merge-pathnames parsed (files-directory))))
    (t () nil)))

;;; Events
