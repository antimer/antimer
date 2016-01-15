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
(defparameter +new-article+ (djula:compile-template* "article/new.html"))
(defparameter +article-list+ (djula:compile-template* "article/list.html"))
(defparameter +view-article+ (djula:compile-template* "article/view.html"))

;;; Views

(defmacro render-view (template &rest arguments)
  `(render-template (,template)
                    :user (lucerne-auth:get-userid)
                    ,@arguments))

@route app "/"
(defview index ()
  (render-view +index+))

;;; Wiki views

@route app (:get "/article/new")
(defview get-new-article ()
  (if (lucerne-auth:logged-in-p)
      (render-view +new-article+)
      (render-view +new-article+
                   :error "You must be logged in to create an article.")))

@route app (:post "/article/new")
(defview get-new-article ()
  (if (lucerne-auth:logged-in-p)
      (flet ((render-error (message)
               (render-view +new-article+
                            :error message)))
        (with-params (title slug source)
          (cond
            ((null title)
             (render-error "Forgot title"))
            ((null slug)
             (render-error "Forgot slug"))
            ((null source)
             (render-error "Can't create an empty article"))
            (t
             ;; Validate everything
             (let ((user (antimer.db:find-user (lucerne-auth:get-userid))))
               (antimer.db:create-article title
                                          slug
                                          source
                                          user))
             (redirect (format nil "/article/~A" slug))))))
      (render-view +new-article+
                   :error "You must be logged in to create an article.")))

@route app (:get "/article/all")
(defview all-articles ()
  (render-view +article-list+
               :articles
               (let ((list (list)))
                 (antimer.db:do-articles (article)
                   (push article list))
                 list)))

@route app (:get "/article/:slug")
(defview view-article (slug)
  (flet ((render-error (message)
           (render-view +view-article+
                        :error message)))
    (let ((article (antimer.db:find-article slug)))
      (if article
          (render-view +view-article+
                       :title (antimer.db:article-title article)
                       :html (antimer.doc:render-document
                              (antimer.doc:parse-document
                               (antimer.db:article-source article))))
          (render-error "No such article.")))))

;;; Authentication views

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
