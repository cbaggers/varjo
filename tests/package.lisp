(uiop:define-package #:varjo.tests
    (:use #:cl #:varjo #:varjo-lang #:rtg-math #:rtg-math.base-maths #:fiveam)
  (:import-from :varjo :vbind)
  (:export :test-all
           :void-tests
           :array-tests
           :build-tests
           :struct-tests
           :return-tests
           :flow-id-tests
           :metadata-tests
           :stemcell-tests
           :qualifier-tests
           :assignment-tests
           :inline-glsl-tests
           :flow-control-tests
           :symbol-macro-tests
           :regular-macro-tests
           :name-shadowing-tests
           :compiler-macro-tests
           :first-class-func-tests
           :external-functions-tests
           :uninitialized-value-tests
           :multiple-value-return-tests))
