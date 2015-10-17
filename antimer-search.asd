(defsystem antimer-search
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:split-sequence
               :cl-ppcre
               :typed
               :alexandria)
  :components ((:module "src"
                :components
                ((:module "search"
                  :serial t
                  :components
                  ((:file "porter2")
                   (:file "search"))))))
  :description "Antimer's information retrieval system.")
