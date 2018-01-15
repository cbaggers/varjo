(uiop:define-package #:varjo.tests
    (:use #:cl #:varjo #:vari #:rtg-math #:rtg-math.base-maths #:fiveam
          :varjo.utils)
  (:import-from :varjo :vbind)
  (:import-from :varjo.internals
                :compile-form
                :flow-id-scope
                :flow-ids
                :id=
                :metadata-for-flow-id
                :v-defspecial
                :v-uniforms
                :get-base-env
                :get-symbol-binding
                :primary-type
                :v-argument-spec
                :v-discarded-p
                :v-form-bindings
                :v-function
                :v-return-spec
                :v-returned-p
                :v-voidp
                :valid-for-contextp)
  (:import-from :alexandria
                :random-elt)
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
