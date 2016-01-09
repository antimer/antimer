(defsystem antimer-db
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:integral
               :local-time)
  :components ((:module "src"
                :components
                ((:module "db"
                  :serial t
                  :components
                  ()))))
  :description "Models and database access for Antimer.")
