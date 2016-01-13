(in-package :cl-user)
(defpackage antimer.diff
  (:use :cl)
  (:export :diff
           :patch
           :diff-to-string
           :string-to-diff)
  (:documentation "Utilities for working with diffs."))
(in-package :antimer.diff)

(defparameter +differ+
  (make-instance 'difflib.window:window))

(defun diff (a b)
  (difflib:diff +differ+ a b))

(defun patch (string diff)
  (difflib:diff +differ+ string diff))

(defun diff-to-string (diff)
  (difflib:serialize-diff +differ+ diff))

(defun string-to-diff (string)
  (difflib:deserialize-diff +differ+ string))
