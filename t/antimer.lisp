(in-package :cl-user)
(defpackage antimer-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :antimer-test)

(defparameter +wiki-dir+
  (asdf:system-relative-pathname :antimer #p"t/wiki/"))

(def-suite database
  :description "Database tests.")
(in-suite database)

(test user
   (let ((user (antimer.db:create-user "test"
                                       :email "test@gmail.com"
                                       :plaintext-password "test")))
     (is
      (typep user 'antimer.db:user))
     (is
      (typep (antimer.db:find-user "test") 'antimer.db:user))
     (is
      (null (antimer.db:find-user "nonexistent")))))

(defun run-tests ()
  (ensure-directories-exist +wiki-dir+)
  (let ((wiki (make-instance 'antimer.wiki:wiki
                             :directory +wiki-dir+
                             :plugins (list
                                       (make-instance 'antimer.db:database)))))
    (antimer.wiki:start wiki)
    (run! 'database))
  (uiop:delete-directory-tree +wiki-dir+ :validate t))
