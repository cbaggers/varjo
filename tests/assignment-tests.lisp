(in-package :varjo.tests)
(5am:in-suite assignment-tests)

(5am:def-test let-0 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (let (((x :bool) nil))
       x
       (v! 0 0 0 0)))))
