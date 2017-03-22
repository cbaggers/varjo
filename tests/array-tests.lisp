(in-package :varjo.tests)
(5am:in-suite array-tests)

(5am:def-test array-0 (:suite array-tests)
  (finishes-p
   (compile-vert ((i :int) &uniform (arr (:float 10))) :450 nil
     (v! 0 0 0 0))))

(5am:def-test array-1 (:suite array-tests)
  (finishes-p
   (compile-vert ((i :int)) :450 nil
     (let (((arr (:float 10))))
       (v! 0 0 0 0)))))
