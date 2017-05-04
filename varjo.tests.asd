;;;; varjo.asd

(asdf:defsystem #:varjo.tests
  :description "Tests for varjo"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:varjo #:fiveam)
  :components ((:file "tests/package")
               (:file "tests/tests")
               (:file "tests/build-tests")
               (:file "tests/array-tests")
               (:file "tests/external-functions")
               (:file "tests/first-class-functions")
               (:file "tests/flow-control")
               (:file "tests/multiple-values")
               (:file "tests/stemcell-tests")
               (:file "tests/struct-tests")
               (:file "tests/assignment-tests")
               (:file "tests/return-tests")
               (:file "tests/void-tests")
               (:file "tests/flow-ids")
               (:file "tests/inline-glsl")
               (:file "tests/symbol-macros")
               (:file "tests/compiler-macros")
               (:file "tests/regular-macros")
               (:file "tests/name-shadowing")
               (:file "tests/let-tests")
               (:file "tests/qualifier-tests")
               (:file "tests/uninitialized-value-tests")
               (:file "tests/metadata")))
