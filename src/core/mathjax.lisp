(in-package :cl-user)
(defpackage antimer.mathjax
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
  (:export :mathjax)
  (:documentation "The MathJax plugin."))
(in-package :antimer.mathjax)

;;; Plugin definition

(defclass mathjax (plugin)
  ()
  (:default-initargs
   :directory-name "mathjax")
  (:documentation "The MathJax plugin."))

(defmethod name ((plugin mathjax))
  "MathJax")

(defmethod short-description ((plugin mathjax))
  "Adds support for rendering math using MathJax.")

(antimer.config:register-default-plugin (make-instance 'mathjax))

;;; Events

(defparameter +url+
  "https://github.com/mathjax/MathJax/archive/2.6.0.zip")

(defmethod on-event ((plugin mathjax) (event startup))
  "On startup, ensure MathJax is downloaded."
  (let ((archive (merge-pathnames #p"mathjax.zip" (data-directory plugin)))
        (dir (merge-pathnames #p"MathJax-2.6.0/" (data-directory plugin)))
        (dest #p"mathjax/"))
    (unless (probe-file (antimer.static:static-pathname dest))
      (antimer.log:info :mathjax "Downloading MathJax")
      (trivial-download:download +url+ archive :quiet t)
      (antimer.log:info :mathjax "Extracting MathJax")
      (trivial-extract:extract-zip archive)
      ;; Copy the directory
      (antimer.log:info :mathjax "Copying MathJax files")
      (antimer.static:copy-directory dir #p"mathjax/")
      ;; Delete everything
      (antimer.log:info :mathjax "Deleting downloads")
      (delete-file archive)
      (uiop:delete-directory-tree dir :validate t))))
