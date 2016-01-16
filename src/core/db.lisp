(in-package :cl-user)
(defpackage antimer.db
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
  (:import-from :crane
                :make-session
                :deftable
                :int
                :text
                :bool
                :timestamp)
  (:export :database
           :user
           :user-username
           :user-email
           :user-password
           :user-admin-p
           :create-user
           :find-user
           :check-password
           :article
           :article-title
           :article-slug
           :article-source
           :change
           :change-article
           :change-user
           :change-message
           :change-diff
           :change-timestamp
           :create-article
           :edit-article
           :find-article
           :article-count
           :do-articles
           :random-article
           :do-changes)
  (:documentation "Antimer's relational database interface."))
(in-package :antimer.db)

;;; Plugin definition

(defclass database (plugin)
  ((url :reader plugin-url
        :initarg :url
        :type string
        :documentation "A Crane database URL.")
   (session :accessor plugin-session
            :type crane:session
            :documentation "The Crane session object."))
  (:default-initargs
   :directory-name "db")
  (:documentation "The @c(database) plugin provides an SQL database for Antimer."))

(defmethod name ((plugin database))
  "SQL Database")

(defmethod short-description ((plugin database))
  "The database plugin provides an SQL database for storing Antimer's data.")

;;; Tables

(deftable user ()
  ((username :reader user-username
             :initarg :username
             :type text
             :uniquep t
             :indexp t
             :documentation "The user's username.")
   (email :reader user-email
          :initarg :email
          :type text
          :documentation "The user's email address.")
   (password :reader user-password
             :initarg :password
             :type text
             :documentation "The user's hashed password.")
   (adminp :reader user-admin-p
           :initarg :adminp
           :type bool
           :documentation "Is the user an administrator?")
   (token :reader user-api-token
          :initarg :token
          :type text
          :documentation "The API token."))
  (:documentation "Represents a user."))

(defun create-user (username &key email plaintext-password adminp)
  "Create a user instance. Hash the password in the process."
  (crane:create 'user
                :username username
                :email email
                :password (cl-pass:hash plaintext-password
                                        :type :pbkdf2-sha256
                                        :iterations 30000)
                :adminp adminp
                :token (uuid:format-as-urn nil (uuid:make-v4-uuid))))

(defun find-user (username)
  "Find a user by their username. Return NIL if nothing is found."
  (crane:single 'user `(:where (:= :username ,username))))

(defun check-password (user password)
  "Check if a password matches."
  (cl-pass:check-password password (user-password user)))

(deftable article ()
  ((title :accessor article-title
          :initarg :title
          :type text
          :indexp t
          :documentation "The article's title.")
   (slug :accessor article-slug
         :initarg :slug
         :type text
         :uniquep t
         :indexp t
         :documentation "The article's slug.")
   (source :accessor article-source
           :initarg :source
           :type text
           :documentation "The article's current source text."))
  (:documentation "An article."))

(deftable change ()
  ((article :reader change-article
            :initarg :article
            :type int
            :foreign (article :on-delete :cascade :on-update :cascade)
            :documentation "A foreign key to the article this change belongs to.")
   (user :reader change-user
         :initarg :user
         :type int
         :nullp t
         :foreign (user :on-delete :set-null :on-update :cascade)
         :documentation "A foreign key to the user who made this change.")
   (message :reader change-message
            :initarg :message
            :type text
            :documentation "A message describing the change.")
   (diff :reader change-diff
         :initarg :diff
         :type text
         :documentation "The diff from the previous version. If this is the
         first change, then diff represents the changes from the empty string.")
   (timestamp :reader change-timestamp
              :initarg :timestamp
              :type timestamp
              :documentation "The time when the change was made."))
  (:documentation "A change in an article."))

(defun create-article (title slug source user
                       &optional (message "Created article"))
  "Create an article in the database."
  (let ((article (crane:create 'article
                               :title title
                               :slug slug
                               :source source)))
    (crane:create 'change
                  :article (crane:id article)
                  :user (crane:id user)
                  :message message
                  :diff (antimer.diff:diff "" source)
                  :timestamp (local-time:now))
    article))

(defun edit-article (article new-source message user)
  "Edit an article. Return the change object."
  (let ((diff (antimer.diff:diff (article-source article)
                                 new-source)))
    (setf (article-source article) new-source)
    (crane:save article)
    (crane:create 'change
                  :article (crane:id article)
                  :user (crane:id user)
                  :message message
                  :diff diff
                  :timestamp (local-time:now))))

(defun find-article (slug)
  (crane:single 'article `(:where (:= :slug ,slug))))

(defun article-count ()
  (crane:total 'article))

(defun call-with-articles (function results-per-page from)
  (mapc function (crane:filter 'article
                               `(:order-by (:asc :title))
                               `(:limit ,results-per-page)
                               `(:offset ,from))))

(defmacro do-articles ((article &key (results-per-page 25) (from 0))
                       &body body)
  `(call-with-articles #'(lambda (,article)
                           ,@body)
                       ,results-per-page
                       ,from))

(defun random-article ()
  "Find a random article, or NIL if there are no articles."
  (let ((count (article-count)))
    (if (> count 0)
        (crane:single 'article
                      `(:order-by :id)
                      `(:limit 1)
                      `(:offset ,(random count)))
        nil)))

(defun call-with-changes (article function results-per-page from)
  (mapc function (crane:filter 'change
                               `(:where
                                 (:= :article ,(crane:id article)))
                               `(:order-by (:desc :timestamp))
                               `(:limit ,results-per-page)
                               `(:offset ,from))))

(defmacro do-changes ((change article &key (results-per-page 25) (from 0))
                      &body body)
  `(call-with-changes ,article
                      #'(lambda (,change)
                          ,@body)
                      ,results-per-page
                      ,from))

;;; Events

(defmethod on-event ((plugin database) (event startup))
  "On startup, create the session object, hook everything up, and start the
connection."
  (let ((session (crane:make-session :migratep nil
                                     :defaultp t))
        (tag (gensym)))
    (crane.config:add-database
     tag
     (if (slot-boundp plugin 'url)
         ;; The user has provided a custom URL. In case it's an SQLite3 relative
         ;; path, set the *default-pathname-defaults* to the wiki directory
         (let ((*default-pathname-defaults*
                (antimer.wiki:wiki-directory
                 antimer.wiki:*wiki*)))
           (crane.url:parse (plugin-url plugin)))
         ;; By default, use an SQLite3 database in the data directory
         (make-instance 'crane.database.sqlite3:sqlite3
                        :name
                        (namestring
                         (merge-pathnames #p"db.sqlite3"
                                          (data-directory plugin))))))
    (crane:register-database session tag)
    (crane:register-table session 'user tag)
    (crane:register-table session 'article tag)
    (crane:register-table session 'change tag)
    (crane:start session)
    (setf (plugin-session plugin) session)))

(defmethod on-event ((plugin database) (event shutdown))
  "On shutdown, cut the connections."
  (crane:stop (plugin-session plugin)))