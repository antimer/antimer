(defsystem antimer
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:common-doc
               ;; Database
               :crane
               :cl-pass
               ;; Web interface
               :lucerne
               :clack-handler-hunchentoot)
  :components ((:module "src"
                :serial t
                :components
                ((:file "event")
                 (:file "plugin"))))
  :description "A wiki."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op antimer-test))))
