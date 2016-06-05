(in-package :cl-user)
(defpackage antimer.build
  (:use :cl :antimer.wiki)
  (:import-from :antimer.plugin
                :static-generator
                :generate)
  (:export :build)
  (:documentation "Building the wiki."))
(in-package :antimer.build)

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :antimer #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))

;;; Utilities

(defun render-template (wiki pathname template &rest arguments)
  (with-open-file (stream (merge-pathnames pathname (wiki-build-directory wiki))
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (apply #'djula:render-template* (append (list template stream) arguments))))

;;; Build

(defgeneric build (wiki)
  (:documentation "Generate all the files necessary for the wiki.")

  (:method ((wiki wiki))
    ;; Generate static files through the plugins
    (antimer.log:info :antimer "Generating static files")
    (dolist (plugin (wiki-plugins wiki))
     (when (typep plugin 'static-generator)
       (generate wiki plugin)))
    ;; Delete the temporary directory
    (when (probe-file (wiki-temporary-directory wiki))
      (uiop:delete-directory-tree (wiki-temporary-directory wiki) :validate t))
    (flet ((render (template pathname &rest arguments)
             (apply #'render-template (append (list wiki pathname template) arguments))))
      ;; Generate the articles
      (antimer.log:info :antimer "Compiling articles")
      (dolist (pathname (uiop:directory-files (wiki-articles-directory wiki)))
        (let ((article (antimer.article:parse-article wiki pathname)))
          (print article)))
      ;; Create index file
      (render +index+ #p"index.html"
              :antimer-wiki-name (wiki-name wiki))
      (values))))
