(in-package :varjo.tests)
(5am:in-suite ubo-ssbo-tests)

;;------------------------------------------------------------
;; Helper data

(v-defstruct some-data ()
  (ints (:int 1000)))

(v-defstruct outer-one ()
  (data (some-data 10)))

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
   (compile-frag (&uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (with-slots (ints) the-data
       (setf (aref ints 0) 1)
       (setf (aref ints 1) 1))
     (v! 1 2 3 4))))

(5am:def-test ssbo-9 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-frag (&uniform (the-data some-data :ssbo :std-430))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints &inline-form t) x
                  (setf (aref ints 1) 10))))
       (blah the-data)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-10 (:suite ubo-ssbo-tests)
  (signals varjo-conditions:assigning-to-readonly
   (compile-vert-frag ((the-data outer-one)) :430 nil
     (()
      (values (v! 1 2 3 4) (v! 1 2)))
     (((hmm :vec2))
      (labels ((blah ((x outer-one) (i :int))
                 (with-slots (ints &inline-form t)
                     (aref (outer-one-data the-data) i)
                   (setf (aref ints 1) 10))))
        (v! 1 2 3 (blah the-data 5)))))))

(5am:def-test ssbo-11 (:suite ubo-ssbo-tests)
  (signals varjo-conditions:assigning-to-readonly
   (compile-frag (&uniform (the-data outer-one))
       :450 nil
     (labels ((blah ((x outer-one) (i :int))
                (setf (aref (slot-value
                             (aref (outer-one-data the-data) 0)
                             ints)
                            0)
                      (aref (slot-value
                             (aref (outer-one-data the-data) 0)
                             ints)
                            0))))
       (v! 1 2 3 (blah the-data 5))))))

(5am:def-test ssbo-12 (:suite ubo-ssbo-tests)
  (signals varjo-conditions:assigning-to-readonly
   (compile-frag (&uniform (the-data outer-one))
       :450 nil
     (labels ((blah ((x outer-one) (i :int))
                (setf (slot-value
                       (aref (outer-one-data the-data) 0)
                       ints)
                      (slot-value
                       (aref (outer-one-data the-data) 0)
                       ints))))
       (blah the-data 5)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-13 (:suite ubo-ssbo-tests)
  (signals varjo-conditions:assigning-to-readonly
   (compile-frag (&uniform (the-data outer-one))
       :450 nil
     (labels ((blah ((x outer-one) (i :int))
                (setf (aref (outer-one-data the-data) 0)
                      (aref (outer-one-data the-data) 0))))
       (blah the-data 5)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-14 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-frag (&uniform (the-data some-data :ssbo :std-430))
       :450 nil
     (setf (aref (some-data-ints the-data) 0) 1)
     (setf (aref (some-data-ints the-data) 1) 1)
     (v! 1 2 3 4))))

(5am:def-test ssbo-15 (:suite ubo-ssbo-tests)
  (signals varjo-conditions:assigning-to-readonly
   (compile-frag (&uniform (the-data some-data))
       :450 nil
     (setf (aref (some-data-ints the-data) 0) 1)
     (setf (aref (some-data-ints the-data) 1) 1)
     (v! 1 2 3 4))))

(5am:def-test ssbo-16 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert-frag ((the-data outer-one)) :430 nil
     (()
      (values (v! 1 2 3 4) (v! 1 2)))
     (((hmm :vec2))
      (labels ((blah ((x outer-one) (i :int))
                 (with-slots (ints &inline-form t)
                     (aref (outer-one-data x) i)
                   (setf (aref ints 1) 10))))
        (v! 1 2 3 (blah the-data 5)))))))

(5am:def-test ssbo-17 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-frag (&uniform (the-data outer-one))
       :450 nil
     (labels ((blah ((x outer-one) (i :int))
                (setf (aref (slot-value
                             (aref (outer-one-data x) 0)
                             ints)
                            0)
                      (aref (slot-value
                             (aref (outer-one-data x) 0)
                             ints)
                            0))))
       (v! 1 2 3 (blah the-data 5))))))

(5am:def-test ssbo-18 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-frag (&uniform (the-data outer-one))
       :450 nil
     (labels ((blah ((x outer-one) (i :int))
                (setf (slot-value
                       (aref (outer-one-data x) 0)
                       ints)
                      (slot-value
                       (aref (outer-one-data x) 0)
                       ints))))
       (blah the-data 5)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-19 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-frag (&uniform (the-data outer-one))
       :450 nil
     (labels ((blah ((x outer-one) (i :int))
                (setf (aref (outer-one-data x) 0)
                      (aref (outer-one-data x) 0))))
       (blah the-data 5)
       (v! 1 2 3 4)))))


(defun try-it-out2 ()
  (varjo:glsl-code
   (varjo:translate
    (varjo:create-stage
     :fragment :450
     :uniform-variables '((the-data outer-one :ssbo :std-140))
     :code '((labels ((blah ()
                        (with-slots (ints &inline-form t)
                            (aref (outer-one-data the-data) 0)
                          (aref ints 1))))
               (blah)
               (v! 1 2 3 4)))))))
