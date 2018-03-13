(in-package :varjo.tests)

;;------------------------------------------------------------

(5am:def-suite test-all)

(5am:def-suite let-tests :in test-all)
(5am:def-suite emit-tests :in test-all)
(5am:def-suite void-tests :in test-all)
(5am:def-suite array-tests :in test-all)
(5am:def-suite build-tests :in test-all)
(5am:def-suite struct-tests :in test-all)
(5am:def-suite return-tests :in test-all)
(5am:def-suite flow-id-tests :in test-all)
(5am:def-suite metadata-tests :in test-all)
(5am:def-suite ubo-ssbo-tests :in test-all)
(5am:def-suite stemcell-tests :in test-all)
(5am:def-suite qualifier-tests :in test-all)
(5am:def-suite assignment-tests :in test-all)
(5am:def-suite overloading-tests :in test-all)
(5am:def-suite inline-glsl-tests :in test-all)
(5am:def-suite flow-control-tests :in test-all)
(5am:def-suite symbol-macro-tests :in test-all)
(5am:def-suite regular-macro-tests :in test-all)
(5am:def-suite name-shadowing-tests :in test-all)
(5am:def-suite compiler-macro-tests :in test-all)
(5am:def-suite first-class-func-tests :in test-all)
(5am:def-suite external-functions-tests :in test-all)
(5am:def-suite uninitialized-value-tests :in test-all)
(5am:def-suite multiple-value-return-tests :in test-all)

(5am:in-suite test-all)

;;------------------------------------------------------------
