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
  :components ((:module "src"
                :serial t
                :components
                ((:file "event")
                 (:file "config")
                 (:file "wiki")
                 (:file "plugin")
                 (:file "log")
                 (:file "static")
                 (:file "diff")
                 (:file "doc")
                 (:module "core"
                  :serial t
                  :components
                  ((:file "db")
                   (:file "fonts")
                   (:file "file")
                   (:file "cache")
                   (:file "web")
                   (:file "search")
                   (:file "math")))
                 (:module "theme"
                  :serial t
                  :components
                  ((:module "default"
                    :serial t
                    :components
                    ((:static-file "style.lass")
                     (:file "theme")))))
                 (:file "wiki-methods")
                 (:file "macros")
                 (:file "cli"))))
  :description "A wiki."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op antimer-test))))
