(in-package :cl-user)
(defpackage antimer.doc
  (:use :cl)
  (:import-from :common-html.emitter
                :*image-format-control*
                :*document-section-format-control*)
  (:export :word-count
           :time-to-read
           :parse-document
           :transform-document
           :render-document)
  (:documentation "Some code for CommonDoc documents."))
(in-package :antimer.doc)

(defun word-count (document)
  "Return the number of words in a document."
  (length
   (remove-if #'uiop:emptyp
              (mapcar #'(lambda (line)
                          (string-trim (list #\Space #\Newline)
                                       line))
                      (ppcre:split "[^\\w]+"
                                   (common-doc.ops:collect-all-text
                                    document))))))

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

(defun transform-document (document)
  "Apply every transformation needed to a document.

If an error occurs, signal transformation-error."
  (let ((doc (common-doc.macro:expand-macros document)))
    (setf doc (common-doc.ops:fill-unique-refs doc))
    doc))

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

(defun render-document (document)
  "Render a CommonDoc document to HTML."
  (let ((*image-format-control* "/file/~A/data")
        (*document-section-format-control* "~A#~A"))
    (common-html.template:with-template ('antimer-template)
      (common-doc.format:emit-to-string (make-instance 'common-html:html)
                                        (transform-document document)))))
