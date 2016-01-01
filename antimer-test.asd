(defsystem antimer-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:antimer
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "antimer")))))
