(in-package :varjo.tests)
(5am:in-suite name-shadowing-tests)

;;------------------------------------------------------------
;; Helper data

(varjo::v-deftype foo-g () ())

(v-def-glsl-template-fun foo-g nil nil nil foo-g)


;;------------------------------------------------------------
;; Tests


(5am:def-test shadowing-0 (:suite name-shadowing-tests)
  (finishes-p
   (compile-vert (&uniform (x foo-g)) :450 nil
     (v! 0 0 0 0))))
