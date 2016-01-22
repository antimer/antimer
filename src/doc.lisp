(in-package :cl-user)
(defpackage antimer.doc
  (:use :cl)
  (:export :raw-text
           :word-count
           :time-to-read
           :parse-document
           :transform-document
           :render-document
           :antimer-template)
  (:documentation "Some code for CommonDoc documents."))
(in-package :antimer.doc)

(defun raw-text (document)
  "Extract the text without markup from a document."
  (common-doc.ops:collect-all-text document))

(defun word-count (text)
  "Return the number of words in a string of text."
  (length
   (remove-if #'uiop:emptyp
              (mapcar #'(lambda (line)
                          (string-trim (list #\Space #\Newline)
                                       line))
                      (ppcre:split "[^\\w]+" text)))))

(defun time-to-read (word-count)
  "Return the time it takes to read a document in minutes."
  (let* ((wpm 200) ;; sure why not
         (result (round (/ word-count wpm))))
    (if (zerop result)
        1
        result)))

(defun parse-document (source)
  "Parse a document from its source tree. Returns the document."
  (pandocl:parse-string source :scriba))

;;; Rendering

(defclass antimer-template (common-html.template:template)
  ()
  (:documentation "A wiki article template."))

(defmethod common-html.template:render ((tmpl antimer-template)
                                        (doc common-doc:document)
                                        content-string)
  "Render a document. We just return the content-string since all the templating
  will be done somewhere else."
  (declare (ignore tmpl doc))
  content-string)
