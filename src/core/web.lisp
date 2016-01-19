(in-package :cl-user)
(defpackage antimer.web
  (:use :cl :lucerne)
  (:import-from :antimer.wiki
                :*wiki*
                :wiki-name
                :wiki-static-directory)
  (:import-from :antimer.plugin
                :plugin
                :name
                :short-description
                :data-directory
                :on-event)
  (:import-from :antimer.event
                :startup
                :shutdown)
  (:export :web-app)
  (:documentation "The web application."))
(in-package :antimer.web)
(annot:enable-annot-syntax)

;;; Plugin definition

(defapp app
  :middlewares (clack.middleware.session:<clack-middleware-session>))

(defclass web-app (plugin)
  ((port :reader plugin-port
         :initarg :port
         :type integer
         :documentation "The port where the server will run."))
  (:documentation "The @c(web-app) plugin implements the web interface."))

(defmethod name ((plugin web-app))
  "Web App")

(defmethod short-description ((plugin web-app))
  "The web app plugin implements Antimer's web interface.")

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :antimer #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))
(defparameter +error+ (djula:compile-template* "error.html"))
(defparameter +register+ (djula:compile-template* "auth/register.html"))
(defparameter +login+ (djula:compile-template* "auth/login.html"))
(defparameter +new-article+ (djula:compile-template* "article/new.html"))
(defparameter +article-list+ (djula:compile-template* "article/list.html"))
(defparameter +view-article+ (djula:compile-template* "article/view.html"))
(defparameter +edit-article+ (djula:compile-template* "article/edit.html"))
(defparameter +article-changes+ (djula:compile-template* "article/changes.html"))

;;; Views

(defmacro render-view (template &rest arguments)
  `(render-template (,template)
                    :wiki-name (wiki-name *wiki*)
                    :user (lucerne-auth:get-userid)
                    ,@arguments))

@route app "/"
(defview index ()
  (render-view +index+))

;;; Wiki views

@route app (:get "/article/new")
(defview get-new-article ()
  (if (lucerne-auth:logged-in-p)
      (render-view +new-article+
                   :title "New Article")
      (render-view +new-article+
                   :title "New Article"
                   :error "You must be logged in to create an article.")))

@route app (:post "/article/new")
(defview post-new-article ()
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
               (if user
                   (progn
                     (antimer.db:create-article title
                                                slug
                                                source
                                                user)
                     (redirect (format nil "/article/~A" slug)))
                   (render-error "Bad username.")))))))
      (render-view +new-article+
                   :error "You must be logged in to create an article.")))

@route app (:get "/article/all")
(defview all-articles ()
  (render-view +article-list+
               :title "All Articles"
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
          (let* ((doc (antimer.doc:parse-document
                       (antimer.db:article-source article)))
                 (word-count (antimer.doc:word-count doc)))
            (render-view +view-article+
                         :title (antimer.db:article-title article)
                         :slug slug
                         :html (antimer.doc:render-document doc)
                         :word-count word-count
                         :read-time (antimer.doc:time-to-read word-count)))
          (render-error "No such article.")))))

@route app (:get "/article/:slug/edit")
(defview get-edit-article (slug)
  (flet ((render-error (message)
           (render-view +edit-article+
                        :error message)))
    (let ((article (antimer.db:find-article slug)))
      (if article
          (if (lucerne-auth:logged-in-p)
              (render-view +edit-article+
                           :title (antimer.db:article-title article)
                           :slug slug
                           :source (antimer.db:article-source article))
              (render-view +error+
                           :message "You need to be logged in to edit an article."))
          (render-error "No such article.")))))

@route app (:post "/article/:slug/edit")
(defview post-edit-article (slug)
  (if (lucerne-auth:logged-in-p)
      (flet ((render-error (message)
               (render-view +edit-article+
                            :error message)))
        (with-params (source message)
          (cond
            ((null source)
             (render-error "Can't create an empty article"))
            ((null message)
             (render-view +edit-article+
                          :error "You must write a description of the changes."
                          :slug slug
                          :source source))
            (t
             ;; Validate everything
             (let ((user (antimer.db:find-user (lucerne-auth:get-userid)))
                   (article (antimer.db:find-article slug)))
               (if (and user article)
                   (progn
                     (antimer.db:edit-article article
                                              source
                                              message
                                              user)
                     (redirect (format nil "/article/~A" slug)))
                   (render-error "Bad username.")))))))
      (render-view +error+
                   :message "You must be logged in to create an article.")))

@route app (:get "/article/:slug/changes")
(defview changes (slug)
  (let ((article (antimer.db:find-article slug)))
    (if article
        (render-view +article-changes+
                     :title (antimer.db:article-title article)
                     :slug slug
                     :changes
                     (let ((changes (list)))
                       (antimer.db:do-changes (change article)
                         (push change changes))
                       changes))
        (render-view +error+
                     :message "No such article."))))

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

;;; Events

(defmethod on-event ((plugin web-app) (event startup))
  "On startup, start the server."
  (antimer.log:info :web "Starting web server on ~D" (plugin-port plugin))
  (lucerne.app:use app
                   (make-instance 'clack.middleware.static:<clack-middleware-static>
                                  :root (wiki-static-directory *wiki*)
                                  :path "/static/"))
  (start app :port (plugin-port plugin)))

(defmethod on-event ((plugin web-app) (event shutdown))
  "On shutdown, stop the server."
  (antimer.log:info :web "Shutting down web server")
  (stop app))

(antimer.config:register-default-plugin
 (make-instance 'web-app
                :port 8000))
