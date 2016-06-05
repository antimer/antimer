(in-package :cl-user)
(defpackage antimer.article
  (:use :cl)
  (:import-from :antimer.wiki
                :wiki)
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

(defmethod parse-article ((wiki wiki) (pathname pathname))
  "Given a wiki and an absolute path to an article, return an article object."
  (multiple-value-bind (front-matter text)
      (yaml-front-matter:parse (uiop:read-file-string pathname))
    (let ((front-matter (cl-yaml:parse front-matter))
          (document (antimer.doc:parse-document text)))
      (make-instance 'article
                     :title (gethash "title" front-matter)
                     :document document
                     :changes (parse-changes wiki pathname)))))
