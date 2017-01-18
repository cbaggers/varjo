(in-package :varjo.tests)
(5am:in-suite flow-control-tests)

(5am:def-test flow-control-0 (:suite flow-control-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1))
       (let ((y 2)
             (z 3))
         (v! x y z)
         (if (> x 2)
             (setq x y)
             (setq x z))
         (v! x 2 3 4))))))

(5am:def-test flow-control-1 (:suite flow-control-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 0)
           (z 1))
       (v! x z)
       (switch x
         (0 (setq z 1))
         (1 (setq z x))
         (2 z))
       (v! x z 3 4)))))


(5am:def-test flow-control-2 (:suite flow-control-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 0)
           (z 1))
       (v! x z)
       (while (< x 10)
         (setq x z)
         (setq z (+ 1 1)))
       (v! x z 3 4)))))

(5am:def-test flow-control-3 (:suite flow-control-tests)
  (glsl-doesnt-contain-p "B = false"
    (varjo.tests::compile-vert () :450 nil
      (let ((a (< 1 2))
            (b (not (< 1 2))))
        (unless (< 1 2)
          1
          4s0))
      (v! 0 0 0 0))))
