(asdf:defsystem "m3utool"
  :version "0.1"
  :author "Noe"
  :mailto "xeonxu@gmail.com"
  :license "GPLv3"
  :depends-on ("clingon"
               "cl-ppcre"
               "cl-excel"
               "alexandria"
               "str"
               "dexador"
               "lparallel"
               "bordeaux-threads"
               )
  :components ((:module "src"
                :components
                ((:file "m3u-data")
                 (:file "m3u-xlsx")
                 (:file "m3u-check")
                 (:file "deploy-settings")
                 (:file "m3u-cli")
                 )))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "m3utool"
  :entry-point "m3u-cli:main"
  :description "CL implemented tool to convert xlsx files to m3u files."
  :in-order-to ((asdf:test-op (asdf:test-op "m3utool/tests"))))

(asdf:defsystem "m3utool/tests"
  :author "Noe"
  :license "GPLv3"
  :depends-on ("m3utool"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for xlstool"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
