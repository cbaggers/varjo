(in-package :varjo.tests)
(5am:in-suite multiple-value-return-tests)

(5am:def-test values-0 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (test 1))
       (v! 0 0 0 0)))))

(5am:def-test values-1 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (values (v! 1 2 3 4)
             (v! 1 2)))))

(5am:def-test values-2 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
       (test 1)))))

(5am:def-test values-3 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (int (test 1)) 0 0)))))


(5am:def-test values-4 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (values (v! 1 2 3 4)
             (v! 1 2))
     (v! 10 20 30 40))))

(5am:def-test values-5 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
       (test 1)
       (v! 10 20 30 40)))))

(5am:def-test values-6 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (int (test 1)) 0 0)
       (v! 10 20 30 40)))))

(5am:def-test values-7 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :450 nil
     (multiple-value-bind (x y) (values 1 2)
       (v! 0 0 0 0)))))


;; (varjo.tests::compile-vert () :450 nil
;;   (labels ((foo ()
;;              (return (progn (values 1 2 3)))))
;;     (multiple-value-bind (a b c) (foo)
;;       (v! a b c 0))))

;; (varjo.tests::compile-vert () :450 nil
;;   (labels ((foo ((x :bool))
;;              (if x
;;                  (values 1 2 3)
;;                  (values 1 2 3))))
;;     (multiple-value-bind (a b c) (foo t)
;;       (v! a b c 0))))
