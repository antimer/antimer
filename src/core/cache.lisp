(in-package :cl-user)
(defpackage antimer.cache
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
                :deftable
                :text)
  (:export :cache
           :pair
           :create-pair
           :get-value
           :pair-exists-p)
  (:documentation "The caching plugin."))
(in-package :antimer.cache)

;;; Plugin definition

(defclass cache (plugin)
  ()
  (:default-initargs
   :directory-name "cache")
  (:documentation "The cache plugin."))

(defmethod name ((plugin cache))
  "Cache")

(defmethod short-description ((plugin cache))
  "Caches data.")

(antimer.config:register-default-plugin (make-instance 'cache))

;;; Tables

(deftable pair ()
  ((key :reader pair-key
        :initarg :key
        :type text
        :uniquep t
	:indexp t
        :nullp nil
        :documentation "The key.")
   (value :accessor pair-value
          :initarg :value
          :type text
          :nullp nil
          :documentation "The value."))
  (:documentation "A key/value pair."))

(antimer.db:register-table 'pair)

(defun create-pair (key value)
  "Create a key/value pair."
  (crane:create 'pair
                :key key
                :value value))

(defun find-pair (key)
  (crane:single 'pair `(:where (:= :key ,key))))

(defun get-value (key)
  "Find the value of a key/value pair by its @cl:param(key). If it doesn't
exist, return @c(nil)."
  (let ((pair (find-pair key)))
    (if pair
        (pair-value pair))))

(defun (setf get-value) (new-value key)
  "Set the value of a key/value pair."
  (let ((pair (find-pair key)))
    (if pair
        (progn
          (setf (pair-value pair) new-value)
          (crane:save pair)
          pair)
        (create-pair key new-value))))

(defun key-exists-p (key)
  "Does @cl:param(key) exist in the database?"
  (and (get-value key) t))

;;; Events

(defmethod on-event ((plugin cache) (event antimer.db:update-article))
  "Receive a document, cache its HTML."
  (antimer.log:info :cache "Caching article")
  (with-accessors ((slug antimer.db:event-slug)
                   (doc antimer.db:event-document)) event
    (let ((text (antimer.doc:raw-text doc)))
      (setf (get-value (format nil "article.html.~A" slug))
            (antimer.doc:render-document doc)

            (get-value (format nil "article.text.~A" slug))
            text

            (get-value (format nil "article.wordcount.~A" slug))
            (write-to-string (antimer.doc:word-count text))))))
