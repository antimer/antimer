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
                :event
                :startup
                :shutdown)
  (:import-from :crane
                :make-session
                :deftable
                :int
                :text
                :bool
                :timestamp)
  ;; Plugin
  (:export :database
           :register-table)
  ;; Events
  (:export :update-article
           :event-id
           :event-slug
           :event-document)
  ;; Models
  (:export :user
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
           :do-changes
           :file
           :filename
           :create-file
           :find-file)
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

(defvar *tables* (list))

(defun register-table (table-name)
  "Register a table that will be created in the database on startup."
  (pushnew table-name *tables* :test #'eq))

;;; Custom events

(defclass update-article (event)
  ((id :reader event-id
       :initarg :id
       :type integer
       :documentation "The document's numeric ID.")
   (slug :reader event-slug
         :initarg :slug
         :type string
         :documentation "The article slug.")
   (document :reader event-document
             :initarg :document
             :type common-doc:document
             :documentation "The CommonDoc document."))
  (:documentation "Sent whenever an article is created or updated."))

(defun send-article-update (article)
  "Parse an article, transform it, and send the transformed document down the
pipeline."
  (with-slots (crane:id slug source) article
    (let ((doc (antimer.doc:transform-document (antimer.doc:parse-document source))))
      (setf (common-doc:title doc) (article-title article))
      (antimer.event:send (make-instance 'update-article
                                         :id crane:id
                                         :slug slug
                                         :document doc)))))

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

(defun clean-source (string)
  (remove #\Return string))

(defun create-article (title slug source user
                       &optional (message "Created article"))
  "Create an article in the database."
  (let* ((source (clean-source source))
         (article (crane:create 'article
                                :title title
                                :slug slug
                                :source source)))
    (crane:create 'change
                  :article (crane:id article)
                  :user (crane:id user)
                  :message message
                  :diff (antimer.diff:diff "" source)
                  :timestamp (local-time:now))
    (send-article-update article)
    article))

(defun edit-article (article source message user)
  "Edit an article, returning the change object. If the source has not changed,
do nothing and return NIL."
  (let ((source (clean-source source)))
    (unless (string= (article-source article) source)
      (let ((diff (antimer.diff:diff (article-source article)
                                     source)))
        (setf (article-source article) source)
        (crane:save article)
        (send-article-update article)
        (crane:create 'change
                      :article (crane:id article)
                      :user (crane:id user)
                      :message message
                      :diff diff
                      :timestamp (local-time:now))))))

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

(deftable file ()
  ((name :reader filename
         :initarg :name
         :type text
         :uniquep t
         :indexp t
         :documentation "The full name of the file, including the extension."))
  (:documentation "A reference to a file."))

(defun create-file (filename)
  "Create a file in the database, returning the instance."
  (crane:create 'file
                :name filename))

(defun find-file (filename)
  (crane:single 'file `(:where (:= :name ,filename))))

;;; Events

(dolist (table '(user article change file))
  (register-table table))

(defmethod on-event ((plugin database) (event startup))
  "On startup, create the session object, hook everything up, and start the
connection."
  (let ((session (crane:make-session :migratep nil))
        (db (if (slot-boundp plugin 'url)
                ;; The user has provided a custom URL. In case it's an SQLite3
                ;; relative path, set the *default-pathname-defaults* to the
                ;; wiki directory
                (let ((*default-pathname-defaults*
                       (antimer.wiki:wiki-directory
                        antimer.wiki:*wiki*)))
                  (crane.url:parse (plugin-url plugin)))
                ;; By default, use an SQLite3 database in the data directory
                (make-instance 'crane.database.sqlite3:sqlite3
                               :name
                               (namestring
                                (merge-pathnames #p"db.sqlite3"
                                                 (data-directory plugin)))))))
    (dolist (table *tables*)
      (crane:register-table session table db))
    (antimer.log:info :db "Starting database connections")
    (crane:start session)
    (setf crane:*session* session)
    (setf (plugin-session plugin) session)))

(defmethod on-event ((plugin database) (event shutdown))
  "On shutdown, cut the connections."
  (antimer.log:info :db "Closing database connections")
  (crane:stop (plugin-session plugin))
  (makunbound 'crane:*session*))
