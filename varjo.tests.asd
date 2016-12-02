;;;; varjo.asd

(asdf:defsystem #:varjo.tests
  :description "Tests for varjo"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:varjo #:fiveam)
  :components ((:file "tests/package")
               (:file "tests/tests")))
