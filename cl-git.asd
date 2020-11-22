(asdf:defsystem "cl-git"
  :description "Study and implement different internals of git"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :licence "GPLv3"
  :version "0.0.1"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "myers"))))
  :in-order-to ((test-op (test-op "cl-git/tests"))))


(asdf:defsystem "cl-git/tests"
  :description "Tests for cl-git"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on ("rove"
               "cl-git")
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "myers-tests"))))
  :perform (test-op (o c) (symbol-call :rove '#:run :cl-git/tests)))
