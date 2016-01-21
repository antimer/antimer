(in-package :cl-user)
(defpackage antimer.macros
  (:use :cl)
  (:import-from :common-doc
                :define-node
                :make-content
                :make-meta
                :make-text
                :reference)
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
