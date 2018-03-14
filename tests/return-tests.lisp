(in-package :varjo.tests)
(5am:in-suite return-tests)

(5am:def-test return-0 (:suite return-tests)
  (finishes-p
   (compile-vert ((a :int)) :410 nil
     (labels ((fn ((x :bool))
                (if x
                    (return (+ 1 2))
                    (return 3))))
       (v! 0 0 0 0)))))

(5am:def-test return-1 (:suite return-tests)
  (signals varjo-conditions:return-type-mismatch
   (compile-vert ((a :int)) :410 nil
     (labels ((fn ((x :bool))
                (if x
                    (return (+ 1 2))
                    (return 3.2))))
       (v! 0 0 0 0)))))

(5am:def-test return-2 (:suite return-tests)
  (signals varjo-conditions:stage-primary-type-mismatch
   (compile-vert ((a :int)) :410 nil
     (let ((x t))
       (if x
           (return (+ 1 2))
           (return 3)))
     (v! 0 0 0 0))))

(5am:def-test return-3 (:suite return-tests)
  (glsl-contains-n-p 1
      "return 1;"
    (varjo.tests::compile-vert () :410 t
      (labels ((gen-line ((index :int))
                 1))
        (gen-line 2)
        (values)))))

(5am:def-test return-4 (:suite return-tests)
  (glsl-doesnt-contain-p "out"
    (varjo.tests::compile-vert () :410 t
      (values))))

(5am:def-test return-5 (:suite return-tests)
  (glsl-contains-n-p 1 "out vec4"
    (varjo.tests::compile-frag () :410 t
      (return (v! 1 2 3 4)))))

(5am:def-test return-6 (:suite return-tests)
  (glsl-contains-n-p 1 "out vec4"
    (varjo.tests::compile-frag () :410 t
      (v! 1 2 3 4))))

(5am:def-test return-7 (:suite return-tests)
  (glsl-contains-n-p 1 "out vec4"
    (varjo.tests::compile-frag () :410 t
      (return (v! 1 2 3 4)))))

(5am:def-test return-8 (:suite return-tests)
  (glsl-contains-n-p 1 "return 1;"
    (varjo.tests::compile-frag () :410 t
      (labels ((gen-line ()
                 1))
        (gen-line)
        (values)))))

(5am:def-test return-9 (:suite return-tests)
  (glsl-contains-n-p 1 "return 1;"
    (varjo.tests::compile-frag () :410 t
      (labels ((gen-line ()
                 (return 1)))
        (gen-line)
        (values)))))

(5am:def-test return-10 (:suite return-tests)
  (glsl-contains-1-of-all-p ("\\(out int return_1, out int return_2\\);"
                             "return_1 = "
                             "return_2 = "
                             "return g_")
    (varjo.tests::compile-frag () :410 t
      (labels ((gen-line ()
                 (values 1 2 3)))
        (gen-line)
        (values)))))

(5am:def-test return-11 (:suite return-tests)
  (glsl-contains-1-of-all-p  ("\\(out int return_1, out int return_2\\);"
                              "return_1 = "
                              "return_2 = "
                              "return g_")
    (varjo.tests::compile-frag () :410 t
      (labels ((gen-line ()
                 (return (values 1 2 3))))
        (gen-line)))))

(5am:def-test return-12 (:suite return-tests)
  (signals varjo-conditions:return-type-mismatch
    (compile-frag ((a :int :flat)) :450 nil
      (if (= a 1)
          (return)
          (v! 1 2)))))

(5am:def-test return-13 (:suite return-tests)
  (signals varjo-conditions:let-returned
    (compile-frag ((a :int :flat)) :450 nil
      (let ((a (if (= a 1)
                   (return 1)
                   (return (values 2)))))
        (return (values 1 2))))))

(5am:def-test return-14 (:suite return-tests)
  (finishes-p
   (compile-frag ((a :int :flat)) :450 nil
     (if (= a 1)
         (return (v! 1 1 1 1))
         (discard)))))

(5am:def-test return-15 (:suite return-tests)
  (finishes-p
   (compile-frag ((a :int :flat)) :450 nil
     (if (= a 1)
         (discard)
         (discard)))))

(5am:def-test return-16 (:suite return-tests)
  (signals varjo-conditions:return-type-mismatch
    (compile-frag ((a :int :flat)) :450 nil
      (let ((a (if (= a 1)
                   (return)
                   (v! 1 2))))
        a))))

(5am:def-test return-17 (:suite return-tests)
  (finishes-p
   (compile-frag ((a :int :flat)) :450 nil
     (labels ((foo ()
                (if (= a 1)
                    (return)
                    (return))))
       (v! 1 2 3 4)))))
