(in-package :varjo.tests)
(5am:in-suite return-tests)

(5am:def-test return-0 (:suite return-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (labels ((fn ((x :bool))
                (if x
                    (return (+ 1 2))
                    (return 3))))
       (v! 0 0 0 0)))))

(5am:def-test return-1 (:suite return-tests)
  (signals varjo-conditions:return-type-mismatch
   (compile-vert ((a :int)) :450 nil
     (labels ((fn ((x :bool))
                (if x
                    (return (+ 1 2))
                    (return 3.2))))
       (v! 0 0 0 0)))))

(5am:def-test return-2 (:suite return-tests)
  (signals varjo-conditions:vertex-stage-primary-type-mismatch
   (compile-vert ((a :int)) :450 nil
     (let ((x t))
       (if x
           (return (+ 1 2))
           (return 3)))
     (v! 0 0 0 0))))
