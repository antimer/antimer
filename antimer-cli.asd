(defsystem antimer-cli
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
  :entry-point "antimer.cli:main"
  :components ((:module "src"
                :components
                ((:file "cli"))))
  :description "Command-line interface for Antimer.")
