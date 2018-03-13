(in-package :varjo.tests)
(5am:in-suite ubo-ssbo-tests)

;;------------------------------------------------------------
;; Helper data

(v-defstruct some-data ()
  (ints (:int 1000)))

;;------------------------------------------------------------
;; Tests

(5am:def-test ubo-0 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo)) :410 nil
     (v! 1 2 3 4))))

(5am:def-test ubo-1 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo)) :410 nil
     (with-slots (ints) the-data
       (v! (aref ints 1) 2 3 4)))))

(5am:def-test ubo-2 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo :std-140))
       :410 nil
     (with-slots (ints) the-data
       (v! (aref ints 1) 2 3 4)))))

(5am:def-test ubo-3 (:suite ubo-ssbo-tests)
  (signals error
    (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo :std-430))
        :410 nil
      (with-slots (ints) the-data
        (v! (aref ints 1) 2 3 4)))))

(5am:def-test ubo-4 (:suite ubo-ssbo-tests)
  (signals varjo-conditions::assigning-to-readonly
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo :std-140))
       :410 nil
     (with-slots (ints) the-data
       (setf (aref ints 1) 10)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-0 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (with-slots (ints) the-data
       (v! (aref ints 1) 2 3 4)))))

(5am:def-test ssbo-1 (:suite ubo-ssbo-tests)
  (finishes-p
    (compile-vert ((vert pos-col) &uniform (the-data some-data :ssbo :std-430))
        :450 nil
      (with-slots (ints) the-data
        (v! (aref ints 1) 2 3 4)))))

(5am:def-test ssbo-2 (:suite ubo-ssbo-tests)
  (finishes-p
    (compile-vert ((vert pos-col) &uniform (the-data some-data :ssbo :std-140))
        :450 nil
      (with-slots (ints) the-data
        (setf (aref ints 1) 10)
        (v! 1 2 3 4)))))

;;------------------------------------------------------------
