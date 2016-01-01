(in-package :cl-user)
(defpackage antimer.model.lmdb
  (:use :cl)
  (:import-from :antimer.model
                ;; Classes
                :user
                :tag
                :article
                :revision
                ;; Accessors
                :model-id
                :model-creation
                :user-username
                :user-email
                :user-password
                :tag-name
                :tag-tags
                :article-title
                :article-tags
                :article-revisions
                :revision-diff)
  (:import-from :antimer.model.db
                :database
                :connect
                :disconnect)
  (:documentation "Hook up Antimer models to kv."))
(in-package :antimer.model.lmdb)

(defclass lmdb (database)
  ((directory :reader database-directory
              :initarg :directory
              :type pathname
              :documentation "The database directory.")
   (classes :accessor database-classes
            :initarg :classes
            :initform nil
            :type list
            :documentation "A list of class names.")
   (stores :accessor database-stores
           :initarg :stores
           :initform nil
           :type list
           :documentation "A list of kv:store objects.")
   (field-counts :accessor database-field-counts
                 :initarg :field-counts
                 :initform nil
                 :type list
                 :documentation "A list of field counts."))
  (:documentation "A database."))

(defun register-class (database class table-name fields)
  "Register a class on a database. The fields argument is a list of slot names."
  (flet ((subdir (pathname)
           (uiop:merge-pathnames* pathname directory)))
    (with-slots (classes stores field-counts) database
      (push class classes)
      (push (make-instance store
                           :directory (subdir
                                       (make-pathname :directory (list :relative
                                                                       table-name))))
            stores)
      (push (length fields) field-counts)))
  class)

(defmethod initialize-instance :after ((database lmdb) &key)
  (register-class database
                  'user
                  "user"
                  '(model-id model-creation


(defmethod connect ((database database))
  (loop for store in (database-stores database) do
    (kv:connect store)))

(defmethod disconnect ((database database))
