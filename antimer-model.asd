(defsystem antimer-model
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:local-time)
  :components ((:module "src"
                :components
                ((:module "model"
                  :serial t
                  :components
                  ((:file "model")
                   (:file "db"))))))
  :description "Models and database access for Antimer.")
