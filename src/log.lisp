(in-package :cl-user)
(defpackage antimer.log
  (:use :cl)
  (:shadow :error :warn :debug :trace)
  (:export :fatal
           :severe
           :error
           :warn
           :info
           :debug
           :trace)
  (:documentation "The logging interface."))
(in-package :antimer.log)

(defun emit-log (kind category format-string &rest format-args)
  (format *error-output*
          "[~A] ~A - ~A~%"
          kind
          (string-downcase (symbol-name category))
          (apply #'format (cons nil (cons format-string format-args)))))

(defmacro define-log-macro (kind)
  `(defmacro ,kind (category format-string &rest format-args)
     (let ((kind ',kind))
       `(emit-log ',kind ,category ,format-string ,@format-args))))

(define-log-macro fatal)
(define-log-macro severe)
(define-log-macro error)
(define-log-macro warn)
(define-log-macro info)
(define-log-macro debug)
(define-log-macro trace)
