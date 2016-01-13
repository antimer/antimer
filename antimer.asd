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
               :dbd-sqlite3
               :uuid
               ;; Web interface
               :lucerne
               :lucerne-auth
               :clack-handler-hunchentoot
               ;; Command line
               :command-line-arguments
               ;; Search
               :searchspace
               ;; Utilities
               :uiop
               :alexandria
               :difflib
               :split-sequence
               :yason)
  :build-operation program-op
  :build-pathname "antimer"
  :entry-point "antimer.cli:main"
  :components ((:module "src"
                :serial t
                :components
                ((:file "event")
                 (:file "wiki")
                 (:file "plugin")
                 (:file "diff")
                 (:file "doc")
                 (:file "db")
                 (:file "wiki-methods")
                 (:file "app")
                 (:file "cli"))))
  :description "A wiki."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op antimer-test))))
