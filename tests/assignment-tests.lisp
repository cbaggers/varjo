(in-package :varjo.tests)
(5am:in-suite assignment-tests)

(5am:def-test assign-0 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (let ((x 10))
       (setf x 20)
       (v! 0 0 0 0)))))

(5am:def-test assign-1 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (let ((x 10)
           (y 10))
       (setf x y
             y 10)
       (v! 0 0 0 0)))))

(5am:def-test assign-2 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (let ((x 10))
       (incf x 1)
       (v! 0 0 0 0)))))

;; (5am:def-test assign-3 (:suite assignment-tests)
;;   (finishes-p
;;    (compile-vert ((a :int)) :450 nil
;;      (let ((x 10))
;;        (decf x 1)
;;        (v! 0 0 0 0)))))
