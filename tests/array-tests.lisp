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

(5am:def-test array-2 (:suite array-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let* ((i (make-array 5 :element-type :float)))
       (v! (aref i 0) 0 0 0)))))

(5am:def-test array-3 (:suite array-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let* ((i (make-array 5 :element-type :float :initial-element 5s0)))
       (v! (aref i 0) 0 0 0)))))

(5am:def-test array-4 (:suite array-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let* ((i (make-array 5 :element-type :float
                           :initial-contents '(1s0 2s0 3s0 4s0 5s0))))
       (v! (aref i 0) 0 0 0)))))

(5am:def-test array-5 (:suite array-tests)
  (signals varjo-conditions:make-array-mandatory-args
    (compile-vert () :450 nil
      (let* ((i (make-array 5)))
        (v! (aref i 0) 0 0 0)))))

(5am:def-test array-6 (:suite array-tests)
  (signals varjo-conditions:multi-dimensional-array
    (compile-vert () :450 nil
      (let* ((i (make-array (5 2) :element-type :int)))
        (v! (aref i 0) 0 0 0)))))

(5am:def-test array-7 (:suite array-tests)
  (signals varjo-conditions:should-be-quoted
    (compile-vert () :450 nil
      (let* ((i (make-array 2 :element-type int)))
        (v! (aref i 0) 0 0 0)))))

(5am:def-test array-8 (:suite array-tests)
  (signals varjo-conditions:should-be-quoted
    (compile-vert () :450 nil
      (let* ((i (make-array 2 :element-type :int :initial-contents (1 2 3))))
        (v! (aref i 0) 0 0 0)))))

(5am:def-test array-9 (:suite array-tests)
  (signals varjo-conditions:make-array-cant-cast-args
    (compile-vert () :450 nil
      (let* ((i (make-array 3 :element-type :int :initial-contents '(1s0 2 3))))
        (v! (aref i 0) 0 0 0)))))

(5am:def-test array-10 (:suite array-tests)
  (signals varjo-conditions:should-be-constant
    (compile-vert () :450 nil
      (let* ((i (make-array 2 :element-type :int :initial-element a)))
        (v! (aref i 0) 0 0 0)))))
