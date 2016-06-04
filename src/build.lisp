(in-package :cl-user)
(defpackage antimer.build
  (:use :cl :antimer.wiki)
  (:import-from :antimer.plugin
                :static-generator
                :generate)
  (:documentation "Building the wiki."))
(in-package :antimer.build)

(defgeneric build (wiki)
  (:documentation "Generate all the files necessary for the wiki.")

  (:method ((wiki wiki))
    (format t "Generating static files...")
    ;; Generate static files through the plugins
    (dolist (plugin (wiki-plugins wiki))
     (when (typep plugin 'static-generator)
       (generate wiki plugin)))
    ;; Generate the articles
    (format t "Generating articles")
    (dolist (pathname (uiop:directory-files (wiki-articles-directory wiki)))
      (format t "Compiling pathname ~A" pathname))))
