(in-package :cl-user)
(defpackage antimer.doc
  (:use :cl)
  (:export :word-count
           :time-to-read)
  (:documentation "Some code for CommonDoc documents."))
(in-package :antimer.doc)

(defun word-count (document)
  "Return the number of words in a document."
  10)

(defun time-to-read (document)
  "Return the time it takes to read a document in seconds."
  (let ((wpm 200)) ;; sure why not
    (* (word-count document) (/ wpm 60))))
