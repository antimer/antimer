(defsystem antimer-cmd
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:antimer
               :command-line-arguments
               :alexandria
               :uiop)
  :build-operation program-op
  :build-pathname "antimer"
  :entry-point "antimer.cmd:main"
  :components ((:module "src"
                :components
                ((:file "command-line"))))
  :description "Command-line interface for Antimer.")
