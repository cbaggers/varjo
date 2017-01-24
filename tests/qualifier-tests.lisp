(in-package :varjo.tests)
(5am:in-suite qualifier-tests)

;;------------------------------------------------------------
;; Helper data



;;------------------------------------------------------------
;; Tests


(5am:def-test qualifiers-0 (:suite qualifier-tests)
  (finishes-p
   (compile-vert () :450 nil
     (v! 0 0 0 0))))
