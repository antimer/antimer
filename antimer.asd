(defsystem antimer
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.2"
  :homepage "https://github.com/antimer/antimer"
  :bug-tracker "https://github.com/antimer/antimer/issues"
  :source-control (:git "git@github.com:antimer/antimer.git")
  :depends-on (;; Documents
               :common-doc
               :pandocl
               :thorn
               :common-doc-tex
               :common-doc-gnuplot
               ;; Templates
               :djula
               ;; Command line
               :command-line-arguments
               ;; Configuration
               :cl-yaml
               ;; Assets
               :lass
               :lass-flexbox
               ;; Utilities
               :uiop
               :alexandria)
  :build-operation program-op
  :build-pathname "antimer"
  :entry-point "antimer.cli:main"
  :components ()
  :description "A wiki."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op antimer-test))))
