(in-package :cl-user)
(defpackage antimer.model
  (:use :cl)
  (:shadow :delete
           :find)
  ;; Interface
  (:export :save
           :delete
           :find)
  ;; Base class
  (:export :model
           :model-id
           :model-creation)
  ;; Users
  (:export :user
           :user-username
           :user-email
           :user-password
           :make-user
           :username)
  ;; Tags
  (:export :tag
           :tag-name
           :tag-tags)
  ;; Articles
  (:export :article
           :article-title
           :article-tags
           :article-revisions)
  (:export :revision
           :revision-message
           :revision-diff)
  (:documentation "Antimer models."))
(in-package :antimer.model)

;;; Interface

(defgeneric save (object)
  (:documentation "Create or update an object in the database."))

(defgeneric delete (object)
  (:documentation "Delete the object from the database."))

(defgeneric find (class id)
  (:documentation "Return an instance from its numeric ID."))

;;; Base class

(defclass model ()
  ((id :reader model-id
       :initarg :id
       :initform (error "Model without ID.")
       :type integer
       :documentation "A unique integer ID.")
   (creation :reader model-creation
             :initarg :creation
             :initform (local-time:now)
             :type local-time:timestamp
             :documentation "The creation time."))
  (:documentation "The base class of Antimer models."))

;;; Users

(defun valid-username-p (string)
  "Is @c(string) a valid username?"
  (and (every #'graphic-char-p string)
       (null (position #\Space string))))

(deftype username ()
  "The username type."
  `(and string (satisfies valid-username-p)))

(defclass user (model)
  ((username :reader user-username
             :initarg :username
             :type username
             :documentation "The user's username.")
   (email :reader user-email
          :initarg :email
          :type string
          :documentation "The user's email address.")
   (pasword :reader user-password
            :initarg :password
            :type string
            :documentation "The hashed password."))
  (:documentation "Represents a user's information."))

(defun make-user (username &key email password)
  "Create a user object."
  (make-instance 'user
                 :username username
                 :email email
                 :password password))

;;; Tags

(defclass tag (model)
  ((name :reader tag-name
         :initarg :name
         :type string
         :documentation "The name of the tag.")
   (tags :reader tag-tags
         :initarg :tags
         :type list
         :documentation "A list of IDs of tags. Tags can be tagged."))
  (:documentation "A tag."))

;;; Articles

(defclass article (model)
  ((title :reader article-title
          :initarg :title
          :type string
          :documentation "The article title.")
   (tags :reader article-tags
         :initarg :tags
         :type list
         :documentation "A list of tag IDs.")
   (revisions :reader article-revisions
              :initarg :revisions
              :type list
              :documentation "A list of revision IDs, most recent first."))
  (:documentation "An article."))

(defclass revision (model)
  ((message :reader revision-message
            :initarg :message
            :type string
            :documentation "A message describing the changes from the previous
            version.")
   (diff :reader revision-diff
         :initarg :diff
         :type diff:diff
         :documentation "A diff is a vector of patches."))
  (:documentation "A change in an article."))
