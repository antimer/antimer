(defsystem antimer-lmdb
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:kv
               :kv-lmdb
               :antimer-model)
  :components ((:module "src"
                :components
                ((:module "models"
                  :serial t
                  :components
                  ((:file "lmdb"))))))
  :description "LMDB implementation of Antimer's database.")
