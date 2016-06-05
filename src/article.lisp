(in-package :cl-user)
(defpackage antimer.article
  (:use :cl)
  (:documentation "Antimer's article class."))
(in-package :antimer.article)

;;; Classes

(defclass article ()
  ((title :reader article-title
          :initarg :title
          :type string
          :documentation "The article title.")
   (document :reader article-document
             :initarg :document
             :type common-doc:document
             :documentation "The article's CommonDoc document.")
   (changes :reader article-changes
            :initarg :changes
            :type (vector change)
            :documentation "A vector of change objects."))
  (:documentation "An article."))

(defclass change ()
  ((summary :reader change-summary
            :initarg :summary
            :type string
            :documentation "The summary of changes.")
   (description :reader change-description
                :initarg :description
                :type string
                :documentation "An optional long description of the changes.")
   (author-name :reader change-author-name
                :initarg :author-name
                :type string
                :documentation "The author's name.")
   (author-email :reader change-author-email
                 :initarg :author-email
                 :type string
                 :documentation "The author's email address.")
   (timestamp :reader change-timestamp
              :initarg :timestamp
              :type local-time:timestamp
              :documentation "The time the changes were made."))
  (:documentation "Represents a change to an article."))

;;; Methods
