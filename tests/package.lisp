(uiop:define-package #:varjo.tests
    (:use #:cl #:varjo #:varjo-lang #:rtg-math #:rtg-math.base-maths #:fiveam)
  (:export :test-all
           :void-tests
           :build-tests
           :struct-tests
           :stemcell-tests
           :flow-control-tests
           :symbol-macro-tests
           :first-class-func-tests
           :external-functions-tests
           :multiple-value-return-tests))
