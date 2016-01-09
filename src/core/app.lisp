(in-package :cl-user)
(defpackage antimer.app
  (:use :cl :lucerne)
  (:import-from :antimer.wiki
                :*wiki*
                :first-time-p)
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

;;; Views

@route app "/"
(defview index ()
  (render-template (+index+)))

@route app (:get "/setup")
(defview setup-form ()
  (if (first-time-p *wiki*)
      ;; Proceed
      (render-template (+setup+))
      ;; Blank error page
      (respond "Already setup.")))

@route app (:post "/setup")
(defview setup-action ()
  (if (first-time-p *wiki*)
      ;; Proceed
      (respond "Success!")
      ;; Blank error page
      (respond "Already setup.")))
