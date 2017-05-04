(in-package :varjo.tests)
(5am:in-suite let-tests)

(5am:def-test let-0 (:suite let-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (let ((x 10))
       (setf x 20)
       (v! 0 0 0 0)))))
