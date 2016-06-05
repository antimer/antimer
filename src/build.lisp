(in-package :cl-user)
(defpackage antimer.build
  (:use :cl :antimer.wiki)
  (:import-from :antimer.plugin
                :static-generator
                :generate)
  (:export :build)
  (:documentation "Building the wiki."))
(in-package :antimer.build)

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
    ;; Generate the articles
    (antimer.log:info :antimer "Compiling articles")
    (dolist (pathname (uiop:directory-files (wiki-articles-directory wiki)))
      (format t "Compiling pathname ~A" pathname))
    (values)))
