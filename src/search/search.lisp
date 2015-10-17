(in-package :cl-user)
(defpackage antimer.search
  (:use :cl)
  (:shadowing-import-from :typed
                          :defun)
  (:export :tokenize
           :stem
           :inverted-index
           :table-index
           :make-inverted-index
           :add-term
           :term-exists-p
           :document-frequency
           :add-document)
  (:documentation "A tiny information retrieval system."))
(in-package :antimer.search)

(defun tokenize ((string string)) list
  "Turn a string into a list of tokens."
  (split-sequence:split-sequence-if #'(lambda (char)
                                        (not (alphanumericp char)))
                                    string
                                    :remove-empty-subseqs t))

(defun stem ((string string)) string
  "Stem a word."
  (antimer.search.porter2:stem string))

(defclass invertex-index ()
  ()
  (:documentation "An inverted index maps terms to document IDs."))

(defgeneric add-term (index term)
  (:documentation "Add a term to an index."))

(defgeneric term-exists-p (index term)
  (:documentation "Does the term exist in the index?"))

(defgeneric document-frequency (index term)
  (:documentation "The number of documents that contain the term."))

(defgeneric add-document (index term document)
  (:documentation "Record that @cl:param(term) appears in @cl:param(document)."))

(defclass table-index (invertex-index)
  ((table :accessor index-table
          :initarg :table
          :type hash-table
          :documentation "A hash table from terms to term-documents."))
  (:documentation "An inverted index using a hash table."))

(cl:defun make-inverted-index (&optional (class 'table-index))
  "Create an inverte index."
  (make-instance class))

(defmethod add-term ((index table-index) term)
  (setf (gethash term (index-table index))
        nil))

(defmethod term-exists-p ((index table-index) term)
  (gethash term (index-table index)))

(defmethod document-frequency ((index table-index) term)
  (length (gethash term (index-table index))))

(defmethod add-document ((index table-index) term document)
  (when (term-exists-p index term)
    (push document (gethash term (index-table index)))))
