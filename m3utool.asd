(asdf:defsystem "m3utool"
  :version #.(with-open-file (stream (merge-pathnames "version.txt" *load-pathname*)
                                     :direction :input
                                     :if-does-not-exist nil)
               (if stream
                   (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line stream))
                   "latest"))
  :author "Zhiqiang Xu"
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
               "hunchentoot")
  :components ((:module "src"
                :components
                        ((:file "version")
                         (:file "deploy-settings")
                         (:file "m3u-data")
                         (:file "m3u-xlsx")
                         (:file "m3u-check")
                         (:file "m3u-server")
                         (:file "m3u-cli"))))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-console-op"
  :build-pathname "m3utool"
  :entry-point "m3u-cli:main"
  :description "CL implemented tool to convert xlsx files to m3u files."
  :in-order-to ((asdf:test-op (asdf:test-op "m3utool/tests"))))

(asdf:defsystem "m3utool/tests"
  :author "Zhiqiang Xu"
  :license "GPLv3"
  :depends-on ("m3utool"
               "rove")
  :components ((:module "tests"
                :components
                        ((:file "main"))))
  :description "Test system for xlstool"
  :perform (asdf:test-op (op c) (uiop:symbol-call :rove :run c)))
