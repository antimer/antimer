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
           :files-directory
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

;;; Define

(defun antimer.doc:transform-document (document)
  "Apply every transformation needed to a document.

If an error occurs, signal transformation-error."
  (let ((common-doc.file:*base-directory*
          (antimer.file:files-directory)))
    (let ((doc (common-doc.macro:expand-macros document)))
      (setf doc (common-doc.ops:fill-unique-refs doc))
      doc)))

(defun antimer.doc:render-document (document)
  "Render a CommonDoc document to HTML."
  (let ((common-html.emitter:*image-format-control* "/file/~A/data")
        (common-html.emitter:*document-section-format-control* "~A#~A")
        (common-doc.file:*base-directory*
          (antimer.file:files-directory)))
    (common-html.template:with-template ('antimer.doc:antimer-template)
      (common-doc.format:emit-to-string (make-instance 'common-html:html)
                                        document))))
