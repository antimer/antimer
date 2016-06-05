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
    (is-false (probe-file (antimer.wiki:wiki-temporary-directory wiki)))
    (finishes
      (antimer.build:build wiki))
    (is-false (probe-file (antimer.wiki:wiki-temporary-directory wiki)))))

(defun run-tests ()
  (run! 'tests))
