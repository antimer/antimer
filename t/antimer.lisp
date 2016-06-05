(in-package :cl-user)
(defpackage antimer-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :antimer-test)

;;; Variables

(defparameter +wiki-dir+
  (asdf:system-relative-pathname :antimer #p"t/wiki/"))

;;; Tests

(def-suite tests
  :description "Antimer tests.")
(in-suite tests)

(test antimer
  (let ((wiki (make-instance 'antimer.standard::standard-wiki
                             :directory +wiki-dir+)))
    (finishes
      (antimer.build:build wiki))))

(defun run-tests ()
  (unwind-protect
       (progn
         (ensure-directories-exist +wiki-dir+)
         (run! 'tests))
    (uiop:delete-directory-tree +wiki-dir+ :validate t)))
