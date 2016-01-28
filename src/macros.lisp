(in-package :cl-user)
(defpackage antimer.macros
  (:use :cl)
  (:import-from :common-doc
                :define-node
                ;; Constructors
                :make-content
                :make-meta
                :make-text
                :make-document-link
                ;; Accessors
                :children
                :reference
                :text)
  (:import-from :common-doc.macro
                :macro-node
                :expand-macro)
  (:documentation "Built-in CommonDoc macros."))
(in-package :antimer.macros)

;;; Utilities

(defun make-class (class-name)
  (make-meta (list (cons "html:class" class-name))))

;;; Math

(define-node theorem (macro-node)
  ()
  (:tag-name "theorem")
  (:documentation "A theorem container."))

(define-node statement (macro-node)
  ()
  (:tag-name "theorem:stmt")
  (:documentation "A theorem statement."))

(define-node proof (macro-node)
  ()
  (:tag-name "theorem:proof")
  (:documentation "A theorem proof."))

(defmethod expand-macro ((node theorem))
  (make-content (cons
                 (make-text "Theorem"
                            :metadata (make-class "title"))
                 (common-doc:children node))
                :reference (reference node)
                :metadata (make-class "theorem")))

(defmethod expand-macro ((node statement))
  (make-content (cons
                 (make-text "Statement"
                            :metadata (make-class "title"))
                 (common-doc:children node))
                :reference (reference node)
                :metadata (make-class "statement")))

(defmethod expand-macro ((node proof))
  (make-content (cons
                 (make-text "Proof"
                            :metadata (make-class "title"))
                 (common-doc:children node))
                :reference (reference node)
                :metadata (make-class "proof")))

;;; Wiki links

(define-node wiki-link (macro-node)
  ()
  (:tag-name "w")
  (:documentation "A wiki link."))

(defmethod expand-macro ((node wiki-link))
  (let ((text (text (first (children node)))))
    (destructuring-bind (url &optional title)
        (uiop:split-string text :separator "|")
      (destructuring-bind (document &optional section)
          (uiop:split-string url :separator "#")
        (make-document-link (if (uiop:emptyp document)
                                ;; Same document
                                nil
                                document)
                            ;; If the section is nil, it's just a link to the
                            ;; document
                            section
                            (list
                             (if (or (null title)
                                     (uiop:emptyp title))
                                 ;; The title is empty, find an article with this title
                                 (let ((article (antimer.db:find-article document)))
                                   (if article
                                       (make-text
                                        (antimer.db:article-title article))
                                       (make-text
                                        "[broken link]")))
                                 ;; The title is not empty
                                 (make-text title)))
                            :metadata (make-class "wiki-link"))))))
