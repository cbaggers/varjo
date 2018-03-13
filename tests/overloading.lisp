(in-package :varjo.tests)
(5am:in-suite overloading-tests)

(v-defun ol-0 ((x :float)) x)
(v-defun ol-0 ((x :double)) x)

(5am:def-test array-1 (:suite array-tests)
  (finishes-p
   (compile-vert ((i :int)) :410 nil
     (let (((arr (:float 10))))
       (v! 0 0 0 0)))))
