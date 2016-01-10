(in-package :cl-user)
(defpackage antimer.app
  (:use :cl :lucerne)
  (:export :app)
  (:documentation "The web application."))
(in-package :antimer.app)
(annot:enable-annot-syntax)

;;; App definition

(defapp app
  :middlewares (clack.middleware.session:<clack-middleware-session>
                (clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :antimer #p"assets/")
                 :path "/static/")))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :antimer #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))
(defparameter +register+ (djula:compile-template* "auth/register.html"))
(defparameter +login+ (djula:compile-template* "auth/login.html"))

(defmacro render-view (template &rest arguments)
  `(render-template (,template)
                    :user (lucerne-auth:get-userid)
                    ,@arguments))

@route app "/"
(defview index ()
  (render-view +index+))

@route app (:get "/register")
(defview get-register ()
  (render-view +register+))

@route app (:post "/register")
(defview post-register ()
  (flet ((render-error (message)
           (render-template (+register+)
                            :error message)))
    (with-params (username email password)
      (cond
        ((null username)
         (render-error "Forgot username."))
        ((null email)
         (render-error "Forgot email."))
        ((null password)
         (render-error "Forgot password."))
        ((antimer.db:find-user username)
         (render-error "A user with that username already exists."))
        (t
         (antimer.db:create-user username
                                 :email email
                                 :plaintext-password password)
         (lucerne-auth:login username)
         (redirect "/"))))))

@route app (:get "/login")
(defview get-login ()
  (render-view +login+))

@route app (:post "/login")
(defview post-login ()
  (flet ((render-error (message)
           (render-template (+login+)
                            :error message)))
    (with-params (username password)
      (let ((user (antimer.db:find-user username)))
        (cond
          ((null user)
           (render-error "No user with that username."))
          ((antimer.db:check-password user password)
           ;; Success!
           (lucerne-auth:login username)
           (redirect "/"))
          (t
           ;; Wrong password
           (render-error "Wrong password.")))))))

@route app (:get "/logout")
(defview logout ()
  (when (lucerne-auth:logged-in-p)
    (lucerne-auth:logout))
  (redirect "/"))
