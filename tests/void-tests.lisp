(in-package :varjo.tests)
(5am:in-suite void-tests)

(5am:def-test void-0 (:suite void-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (labels ((fn ()
                (+ 1 2)
                (values)))
       (v! 0 0 0 0)))))
