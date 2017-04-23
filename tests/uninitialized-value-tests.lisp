(in-package :varjo.tests)
(5am:in-suite uninitialized-value-tests)

(5am:def-test uninitialized-value-0 (:suite uninitialized-value-tests)
  (signals varjo-conditions:uninitialized-var
   (compile-vert () :450 nil
     (let (((i :int)))
       i
       (v! 0 0 0 0)))))

(5am:def-test uninitialized-value-1 (:suite uninitialized-value-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let (((i :int)))
       (setf i 1)
       i
       (v! 0 0 0 0)))))

(5am:def-test uninitialized-value-2 (:suite uninitialized-value-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let (((i :int)))
       (setq i 1)
       i
       (v! 0 0 0 0)))))

(5am:def-test uninitialized-value-3 (:suite uninitialized-value-tests)
  (signals varjo-conditions:uninitialized-var
    (compile-vert () :450 nil
      (let (((i (:int 4))))
        (setf (aref i 0) 10)
        (v! 0 0 0 0)))))

(5am:def-test uninitialized-value-4 (:suite uninitialized-value-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let (((i (:int 4))))
       (setf i #(1 2 3 4))
       (setf (aref i 0) 10)
       (v! 0 0 0 0)))))
