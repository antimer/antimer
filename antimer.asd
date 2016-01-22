(defsystem antimer
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (;; Documents
               :common-doc
               :pandocl
               :thorn
               :common-doc-tex
               :common-doc-gnuplot
               ;; Database
               :crane
               :cl-pass
               :dbd-sqlite3
               ;; Web interface
               :lucerne
               :lucerne-auth
               :djula
               :clack-handler-hunchentoot
               ;; Command line
               :command-line-arguments
               ;; Search
               :searchspace
               ;; Configuration
               :cl-yaml
               ;; Assets
               :lass
               :lass-flexbox
               ;; Utilities
               :uiop
               :alexandria
               :difflib
               :split-sequence
               :yason
               :uuid
               :trivial-download
               :trivial-extract
               :cl-ppcre)
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
                 (:file "djula")
                 (:module "core"
                  :serial t
                  :components
                  ((:file "db")
                   (:file "file")
                   (:file "web")
                   (:file "math")
                   (:file "fonts")))
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
