(in-package :cl-user)
(defpackage antimer.app
  (:use :cl :lucerne)
  (:export :app)
  (:documentation "The web application."))
(in-package :antimer.app)
(annot:enable-annot-syntax)

;;; App definition

(defapp app
  :middlewares ((clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :antimer #p"assets/")
                 :path "/static/")))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :antimer #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))
(defparameter +setup+ (djula:compile-template* "setup.html"))
