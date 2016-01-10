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
                :text
                :bool)
  (:export :database
           :user
           :make-user
           :user-username
           :user-email
           :user-password
           :user-admin-p)
  (:documentation "Antimer's relational database interface."))
(in-package :antimer.db)

;;; Plugin definition

(defclass database (plugin)
  ((session :accessor plugin-session
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

(defun make-user (username &key email plaintext-password adminp)
  "Create a user instance. Hash the password in the process."
  (make-instance 'user
                 :username username
                 :email email
                 :password (cl-pass:hash plaintext-password
                                         :type :pbkdf2-sha256
                                         :iterations 200000)
                 :adminp adminp
                 :token (uuid:format-as-urn nil (uuid:make-v4-uuid))))

;;; Events

(defmethod on-event ((plugin database) (event startup))
  "On startup, create the session object, hook everything up, and start the
connection."
  (let ((session (crane:make-session :migratep nil
                                     :defaultp t)))
    (crane:define-sqlite3-database antimer-db
      :name (namestring
             (merge-pathnames #p"db.sqlite3"
                              (data-directory plugin))))
    (crane:register-database session 'antimer-db)
    (crane:register-table session 'user 'antimer-db)
    (crane:start session)
    (setf (plugin-session plugin) session)))

(defmethod on-event ((plugin database) (event shutdown))
  "On shutdown, cut the connections."
  (crane:stop (plugin-session plugin)))
