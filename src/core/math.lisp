(in-package :cl-user)
(defpackage antimer.math
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
  (:export :math)
  (:documentation "The TeX math plugin."))
(in-package :antimer.math)

;;; Plugin definition

(defclass math (plugin)
  ()
  (:default-initargs
   :directory-name "math")
  (:documentation "The TeX math plugin."))

(defmethod name ((plugin math))
  "TeX Math")

(defmethod short-description ((plugin math))
  "Adds support for rendering math using MathJax.")

(antimer.config:register-default-plugin (make-instance 'math))

;;; Events

(defparameter +url+
  "https://github.com/mathjax/MathJax/archive/2.6.0.zip")

(defmethod on-event ((plugin math) (event startup))
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
