(in-package :varjo.tests)
(5am:in-suite ubo-ssbo-tests)

;;------------------------------------------------------------
;; Helper data

(v-defstruct some-data ()
  (ints (:int 1000)))

(v-defstruct unsized-tail ()
  (pos :vec3)
  (all-the-ints (:int *)))

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

(5am:def-test ssbo-3 (:suite ubo-ssbo-tests)
  (glsl-doesnt-contain-p "SOME_DATA"
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10))))
       (v! 1 2 3 4)))))

(5am:def-test ssbo-4 (:suite ubo-ssbo-tests)
  (glsl-doesnt-contain-p "SOME_DATA"
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10))))
       (blah the-data)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-5 (:suite ubo-ssbo-tests)
  (glsl-doesnt-contain-p "SOME_DATA"
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10)))
              (ham ((x some-data))
                (blah x)))
       ;; note that we dont even have to call ham for this to happen!
       (blah the-data)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-6 (:suite ubo-ssbo-tests)
  (glsl-doesnt-contain-p "SOME_DATA"
    (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10)))
              (ham ((x some-data))
                (blah x)))
       (ham the-data)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-7 (:suite ubo-ssbo-tests)
  (glsl-doesnt-contain-p "SOME_DATA"
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10)))
              (ham ((x some-data))
                (blah x)))
       (ham the-data)
       (blah the-data)
       (v! 1 2 3 4)))))



(5am:def-test ssbo-8 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col)
                  &uniform (the-data unsized-tail :ssbo :std-140))
       :450 nil
     (with-slots (all-the-ints) the-data
       (setf (aref all-the-ints 1) 10)
       (v! 1 2 3 4)))))
