(in-package :cl-user)
(defpackage antimer.math
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
                :text
                :int)
  (:import-from :antimer.web
                :app
                :render-tool-template)
  (:import-from :lucerne
                :route
                :defview
                :with-params
                :respond)
  (:export :math)
  (:documentation "The TeX math plugin."))
(in-package :antimer.math)
(annot:enable-annot-syntax)

;;; Plugin definition

(defclass math (plugin)
  ()
  (:default-initargs
   :directory-name "math")
  (:documentation "The TeX math plugin."))

(defmethod name ((plugin math))
  "TeX Math")

(defmethod short-description ((plugin math))
  "Adds support for rendering math using MathJax.")

(antimer.config:register-default-plugin (make-instance 'math))

;;; Classes

(deftable tex-macro ()
  ((name :reader macro-name
         :initarg :name
         :type text
         :uniquep t
         :indexp t
         :nullp nil
         :documentation "The macro's name.")
   (arity :reader macro-arity
          :initarg :arity
          :type int
          :nullp nil
          :documentation "The macro's arity.")
   (definition :accessor macro-definition
               :initarg :definition
               :type text
               :nullp nil
               :documentation "The macro's definition."))
  (:documentation "A TeX macro stored in the database."))

(antimer.db:register-table 'tex-macro)

(defun measure-macro-arity (string)
  (length
   (remove-duplicates (ppcre:all-matches-as-strings "#\\d" string)
                      :test #'string=)))

(defun create-macro (name definition)
  (crane:create 'tex-macro
                :name name
                :arity (measure-macro-arity definition)
                :definition definition))

(defun find-macro (name)
  (crane:single 'tex-macro `(:where (:= :name ,name))))

(defun update-macro-definition (macro definition)
  (setf (macro-definition macro) definition)
  (crane:save macro))

(defun delete-macro (macro)
  (crane:del macro))

(defun configure-mathjax (stream)
  (write-string "MathJax.Hub.Config({
  jax: ['input/TeX', 'output/SVG'],
  messageStyle: 'none',
  //displayAlign: 'left',
  //displayIndent: '4em',
  tex2jax: {
    inlineMath: [['$', '$']],
    displayMath: [['\\\\(','\\\\)']],
    processEscapes: true
  },
  TeX: {
    Macros: {" stream)
  (dolist (macro (crane:filter 'tex-macro))
    (with-slots (name arity definition) macro
      (format stream "~%      ~S: [~S, ~D]," name definition arity)))
  (write-string "
    },
    equationNumbers: { autoNumber: 'AMS' }
  }
});
MathJax.Hub.Register.StartupHook('TeX Jax Ready', function () {
  MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros, {
    cancel: ['Extension', 'cancel'],
  });
});" stream)
  nil)

;;; Views

(antimer.web:register-tool "Tex Macros" "tex")

(defparameter +index+ (djula:compile-template* "tools/tex/index.html"))

@route app (:get "/tools/tex")
(defview list-tex-macros ()
  (with-params (page)
    (render-tool-template (+index+)
                          :title "TeX Macros"
                          :macros
                          (crane:filter 'tex-macro))))

@route app (:post "/tools/tex")
(defview add-macro ()
  (flet ((render-error (message)
           (render-tool-template (+index+)
                                 :error message)))
    (with-params (name definition)
      (if (and name definition)
          (if (lucerne-auth:logged-in-p)
              (let ((macro (find-macro name)))
                (if macro
                    (render-error "A macro with that name already exists.")
                    (progn
                      (create-macro name definition)
                      (lucerne:redirect "/tools/tex"))))
              (render-error "You must be logged in to define a macro."))
          (render-error "You must supply both a name and a definition for the macro.")))))

@route app (:post "/tools/tex/:name")
(defview update-macro (name)
  (print name)
  (if (lucerne-auth:logged-in-p)
      (let ((macro (find-macro name)))
        (with-params (definition)
          (if (and macro definition)
              (update-macro-definition macro definition)
              (respond "Could not find macro, or did not supply definition.")))
        (respond "Successfully updated."))
      (respond "Not logged in.")))

@route app (:delete "/tools/tex/:name")
(defview update-macro (name)
  (print name)
  (if (lucerne-auth:logged-in-p)
      (let ((macro (find-macro name)))
        (if macro
            (progn
              (delete-macro macro)
              (respond "Successfully updated."))
            (respond "Could not find macro.")))
      (respond "Not logged in.")))

;;; Events

(defparameter +url+
  "https://github.com/mathjax/MathJax/archive/2.6.0.zip")

(defmethod on-event ((plugin math) (event startup))
  "On startup, ensure MathJax is downloaded."
  (let ((archive (merge-pathnames #p"mathjax.zip" (data-directory plugin)))
        (dir (merge-pathnames #p"MathJax-2.6.0/" (data-directory plugin)))
        (dest #p"mathjax/"))
    (unless (probe-file (antimer.static:static-pathname dest))
      (antimer.log:info :mathjax "Downloading MathJax")
      (trivial-download:download +url+ archive :quiet t)
      (antimer.log:info :mathjax "Extracting MathJax")
      (trivial-extract:extract-zip archive)
      ;; Copy the directory
      (antimer.log:info :mathjax "Copying MathJax files")
      (antimer.static:copy-directory dir #p"mathjax/")
      ;; Delete everything
      (antimer.log:info :mathjax "Deleting downloads")
      (delete-file archive)
      (uiop:delete-directory-tree dir :validate t)))
  (antimer.log:info :mathjax "Configure MathJax.")
  (let ((config (merge-pathnames #p"config.js" (data-directory plugin))))
    (with-open-file (stream config
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (configure-mathjax stream))
    (antimer.static:copy-file config #p"mathjax/config.js")))
