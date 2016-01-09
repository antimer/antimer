(defsystem antimer
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:common-doc
               :lucerne
               :clack-handler-hunchentoot
               :antimer-db)
  :components ((:module "src"
                :components
                ((:module "core"
                  :serial t
                  :components
                  ((:file "wiki")
                   (:file "app")
                   (:file "antimer"))))))
  :description "A wiki."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op antimer-test))))
