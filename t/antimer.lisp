(in-package :cl-user)
(defpackage antimer-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :antimer-test)

(defparameter +wiki-dir+
  (asdf:system-relative-pathname :antimer #p"t/wiki/"))

;;; Tests

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
     (null (antimer.db:find-user "nonexistent")))
    (is-true
     (antimer.db:check-password user "test"))
    (is-false
     (antimer.db:check-password user "test2"))))

(test article
  (let* ((user (antimer.db:find-user "test"))
         (article))
    (is
     (equal (antimer.db:article-count) 0))
    (finishes
     (setf article
           (antimer.db:create-article "My Article"
                                      "my-article"
                                      "This is the text of the article."
                                      user)))
    (is
     (equal (antimer.db:article-count) 1))
    (is
     (null (antimer.db:find-article "nonexistent-slug")))
    (let ((found (antimer.db:find-article "my-article")))
      (is
       (string= (antimer.db:article-title found)
                "My Article")))
    ;; Editing the article
    (finishes
     (antimer.db:edit-article article
                              "Updated text."
                              "Changed some things."
                              user))
    (let ((found (antimer.db:find-article "my-article")))
      (is
       (string= (antimer.db:article-title found)
                "My Article"))
      (is
       (string= (antimer.db:article-source found)
                "Updated text.")))
    ;; Edit it again
    (finishes
     (antimer.db:edit-article article
                              "New text."
                              "More changes."
                              user))
    ;; Verify changes
    (let ((change-count 0))
      (antimer.db:do-changes (change article)
        (cond
          ((= change-count 0)
           ;; Last change
           (is
            (string= (antimer.db:change-message change)
                     "More changes.")))
          ((= change-count 1)
           ;; Change before the last one
           (is
            (string= (antimer.db:change-message change)
                     "Changed some things."))))
        (incf change-count))
      (is
       (equal change-count 3)))
    ;; Create another
    (antimer.db:create-article "Another Article"
                               "another-article"
                               "Some test text."
                               user)
    ;; Verify things
    (is
     (= (antimer.db:article-count) 2))
    (let ((article-count 0))
      (antimer.db:do-articles (article)
        ;; Verify lexicographic order
        (cond
          ((= article-count 0)
           (is
            (string= (antimer.db:article-title article) "Another Article")))
          ((= article-count 1)
           (is
            (string= (antimer.db:article-title article) "My Article"))))
        (incf article-count))
      (is
       (= article-count 2)))
    (dotimes (i 100)
      (is
       (typep (antimer.db:random-article) 'antimer.db:article)))))

(defun run-tests ()
  (unwind-protect
       (progn
         (ensure-directories-exist +wiki-dir+)
         (let ((wiki (make-instance 'antimer.wiki:wiki
                                    :directory +wiki-dir+
                                    :plugins (list
                                              (make-instance 'antimer.db:database)))))
           (antimer.wiki:start wiki)
           (run! 'database)
           (antimer.wiki:stop wiki)))
    (uiop:delete-directory-tree +wiki-dir+ :validate t)))
