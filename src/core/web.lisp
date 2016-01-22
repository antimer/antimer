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
  (:export :app
           :web-app
           :register-tool
           :render-tool-template)
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

(defvar *tools* (make-hash-table :test #'equal))

(defun register-tool (title slug)
  (setf (gethash title *tools*) slug))

;;; Djula extensions

(djula:def-tag-compiler antimer-wiki-name ()
  (lambda (stream)
    (write-string (wiki-name *wiki*) stream)))

(djula:def-tag-compiler antimer-get-user ()
  (lambda (stream)
    (declare (ignore stream))
    (setf (getf djula::*template-arguments* :user)
          (lucerne-auth:get-userid))))

(djula:def-tag-compiler antimer-get-tools ()
  (lambda (stream)
    (declare (ignore stream))
    (setf (getf djula::*template-arguments* :tools)
          *tools*)))

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
(defparameter +new-file+ (djula:compile-template* "file/new.html"))
(defparameter +tools-index+ (djula:compile-template* "tools/index.html"))

;;; Views

@route app "/"
(defview index ()
  (render-template (+index+)))

;;; Wiki views

@route app (:get "/article/new")
(defview get-new-article ()
  (if (lucerne-auth:logged-in-p)
      (render-template (+new-article+)
                       :title "New Article")
      (render-template (+new-article+)
                       :title "New Article"
                       :error "You must be logged in to create an article.")))

@route app (:post "/article/new")
(defview post-new-article ()
  (if (lucerne-auth:logged-in-p)
      (flet ((render-error (message)
               (render-template (+new-article+)
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
      (render-template (+new-article+)
                       :error "You must be logged in to create an article.")))

@route app (:get "/article/all")
(defview all-articles ()
  (render-template (+article-list+)
                   :title "All Articles"
                   :articles
                   (let ((list (list)))
                     (antimer.db:do-articles (article)
                       (push article list))
                     list)))

@route app (:get "/article/:slug")
(defview view-article (slug)
  (flet ((render-error (message)
           (render-template (+view-article+)
                            :error message)))
    (let ((article (antimer.db:find-article slug)))
      (if article
          (let* ((doc (antimer.doc:parse-document
                       (antimer.db:article-source article)))
                 (word-count (antimer.doc:word-count doc)))
            (render-template (+view-article+)
                             :title (antimer.db:article-title article)
                             :slug slug
                             :html (antimer.doc:render-document doc)
                             :word-count word-count
                             :read-time (antimer.doc:time-to-read word-count)))
          (render-error "No such article.")))))

@route app (:get "/article/:slug/edit")
(defview get-edit-article (slug)
  (flet ((render-error (message)
           (render-template (+edit-article+)
                            :error message)))
    (let ((article (antimer.db:find-article slug)))
      (if article
          (if (lucerne-auth:logged-in-p)
              (render-template (+edit-article+)
                               :title (antimer.db:article-title article)
                               :slug slug
                               :source (antimer.db:article-source article))
              (render-template (+error+)
                               :message "You need to be logged in to edit an article."))
          (render-error "No such article.")))))

@route app (:post "/article/:slug/edit")
(defview post-edit-article (slug)
  (if (lucerne-auth:logged-in-p)
      (flet ((render-error (message)
               (render-template (+edit-article+)
                                :error message)))
        (with-params (source message)
          (cond
            ((null source)
             (render-error "Can't create an empty article"))
            ((null message)
             (render-template (+edit-article+)
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
      (render-template (+error+)
                       :message "You must be logged in to create an article.")))

@route app (:get "/article/:slug/changes")
(defview changes (slug)
  (let ((article (antimer.db:find-article slug)))
    (if article
        (render-template (+article-changes+)
                         :title (antimer.db:article-title article)
                         :slug slug
                         :changes
                         (let ((changes (list)))
                           (antimer.db:do-changes (change article)
                             (push change changes))
                           changes))
        (render-template (+error+)
                         :message "No such article."))))

;;; Files

@route app (:get "/file/new")
(defview get-new-file ()
  (if (lucerne-auth:logged-in-p)
      (render-template (+new-file+)
                       :title "New File")
      (render-template (+error+)
                       :message "You must be logged in to upload a file.")))

(defun stream->string (stream)
  (trivial-utf-8:utf-8-bytes-to-string
   (coerce (loop for c = (read-byte stream nil nil) while c collecting c)
           'vector)))

(defmacro with-file-param ((name filename stream &key (on-error '(respond "")))
                           &body body)
  `(with-params (,name ,filename)
     (if ,name
         (let ((,stream (first ,name))
               (,filename (stream->string (first ,filename))))
           ,@body)
         ,on-error)))

(defun download-uploaded-file (stream pathname)
  "Store an uploaded file in the disk."
  (with-open-file (output pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (uiop:copy-stream-to-stream stream
                                output
                                :element-type '(unsigned-byte 8))))

@route app (:post "/file/new")
(defview post-new-file ()
  (if (lucerne-auth:logged-in-p)
      (with-file-param (file filename stream)
        (let ((local-path (antimer.file:file-path filename)))
          (antimer.log:info :web "Storing file ~S in ~S" filename (namestring local-path))
          (antimer.db:create-file filename)
          (download-uploaded-file stream local-path))
        (respond "Uploaded successfully"))
      (render-template (+error+)
                       :message "You must be logged in to upload a file.")))

(defun serve-file (pathname)
  (let* ((content-type (or (trivial-mimes:mime-lookup pathname)
                           "text/plain"))
         (univ-time (or (file-write-date pathname)
                        (get-universal-time)))
         (stamp (local-time:universal-to-timestamp univ-time)))
    (with-open-file (stream pathname
                            :direction :input
                            :if-does-not-exist nil)
      `(200
        (:content-type ,content-type
         :content-length ,(file-length stream)
         :last-modified
         ,(local-time:format-rfc1123-timestring nil stamp))
        ,pathname))))

@route app (:get "/file/:filename/data")
(defview file-data (filename)
  (let ((file (antimer.db:find-file filename)))
    (if file
        (serve-file (antimer.file:file-path filename))
        (render-template (+error+)
                         :message "No such file."))))

;;; Tools

(defmacro render-tool-template ((view) &rest params)
  `(render-template (,view)
                    ,@params))

@route app "/tools"
(defview tools ()
  (render-tool-template (+tools-index+)
                        :title "Tools"))

;;; Authentication views

@route app (:get "/register")
(defview get-register ()
  (render-template (+register+)
                   :title "Register"))

@route app (:post "/register")
(defview post-register ()
  (flet ((render-error (message)
           (render-template (+register+)
                            :title "Register"
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
  (render-template (+login+)
                   :title "Sign in"))

@route app (:post "/login")
(defview post-login ()
  (flet ((render-error (message)
           (render-template (+login+)
                            :title "Sign in"
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
