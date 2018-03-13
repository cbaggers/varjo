(in-package :varjo.tests)
(5am:in-suite multiple-value-return-tests)

(5am:def-test values-0 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (test 1))
       (v! 0 0 0 0)))))

(5am:def-test values-1 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (values (v! 1 2 3 4)
             (v! 1 2)))))

(5am:def-test values-2 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
       (test 1)))))

(5am:def-test values-3 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (int (test 1)) 0 0)))))


(5am:def-test values-4 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (values (v! 1 2 3 4)
             (v! 1 2))
     (v! 10 20 30 40))))

(5am:def-test values-5 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
       (test 1)
       (v! 10 20 30 40)))))

(5am:def-test values-6 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (int (test 1)) 0 0)
       (v! 10 20 30 40)))))

(5am:def-test values-7 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert () :410 nil
     (multiple-value-bind (x y) (values 1 2)
       (v! 0 0 0 0)))))

(5am:def-test values-8 (:suite multiple-value-return-tests)
  (glsl-contains-p "gl_Position = TEST\\(1,v_out._VERTEX_STAGE_OUT_1\\)"
    (varjo.tests::compile-vert () :410 nil
      (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
        (test 1)))))

(5am:def-test values-9 (:suite multiple-value-return-tests)
  (glsl-contains-p "_FRAGMENT_STAGE_OUT_0 = TEST\\(1,_FRAGMENT_STAGE_OUT_1\\)"
    (varjo.tests::compile-frag () :410 nil
      (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
        (test 1)))))

(5am:def-test values-10 (:suite multiple-value-return-tests)
  (glsl-contains-p "int NC_1;"
    (compile-vert () :410 nil
      (labels ((test ((x :int)) (values x 2)))
        (v! 0 (test 1))
        (v! 0 0 0 0)))))

(5am:def-test values-11 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-vert () :410 nil
     (labels ((foo ()
                (return (progn (values 1 2 3)))))
       (multiple-value-bind (a b c) (foo)
         (v! a b c 0))))))

(5am:def-test values-12 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-vert () :410 nil
     (labels ((foo ((x :bool))
                (if x
                    (values 1 2 3)
                    (values 1 2 3))))
       (multiple-value-bind (a b c) (foo t)
         (v! a b c 0))))))

(5am:def-test values-13 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-frag () :410 nil
     (labels ((foo ((x :float))
                (values x x)))
       (multiple-value-call #'v! (foo 10) (foo 20))))))

(5am:def-test values-14 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-vert () :410 nil
            (labels ((foo ((x :float))
                       (let ((sq (* x x)))
                         (values (v2! sq)
                                 (v2! (* sq x))))))
              (multiple-value-call #'+ (foo 10))
              (v! 0 0 0 0)))))

(5am:def-test values-15 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-vert () :410 nil
            (labels ((foo ((x :float))
                       (values x x)))
              (multiple-value-call #'v! (foo 10) )
              (v! 0 0 0 0)))))

(5am:def-test values-16 (:suite multiple-value-return-tests)
  (glsl-contains-all-p ("gl_Position = TEST\\(1,v_out._VERTEX_STAGE_OUT_1\\)"
                        "TEST\\(int X, out vec2 return_1\\)"
                        "return_1 = MVB_1")
    (varjo.tests::compile-vert () :410 nil
      (labels ((test ((x :int))
                 (multiple-value-prog1 (values (v! 0 0 0 0) (v! 1 1))
                   10001)))
        (test 1)))))

(5am:def-test values-17 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-vert () :410 nil
     (multiple-value-bind (x y) (values 1 2)
       (values))
     (vec4 1 2 3 4))))

(5am:def-test values-18 (:suite multiple-value-return-tests)
  ;; tests emit in tail position of multiple-value-bind
  (finishes-p
   (varjo.tests::compile-geom () :410 nil
     (declare (output-primitive :kind :triangle-strip
                                :max-vertices 3))
     (multiple-value-bind (x y) (values 1 2)
       (emit ()
             (vec4 0.0 0.0 0.0 1.0)
             (vec3 0.0 0.0 0.0))
       (emit ()
             (vec4 0.0 0.0 0.0 1.0)
             (vec3 0.0 y 0.0))
       (emit ()
             (vec4 0.0 0.0 0.0 1.0)
             (vec3 0.0 x 0.0)))
     (end-primitive)
     (values))))

(5am:def-test values-19 (:suite multiple-value-return-tests)
  (glsl-doesnt-contain-p ".+=.+return"
    (varjo.tests::compile-vert () :410 nil
      (flet ((foo ((test :bool) (a :int) (b :int) (c :int) (d :int))
               (if test
                   (values a b c)
                   (values b c d))))
        (vec4 (foo t 1 2 3 4) 2 3 4)))))

(5am:def-test values-20 (:suite multiple-value-return-tests)
  (glsl-doesnt-contain-p ".+=.+return"
    (varjo.tests::compile-vert () :410 nil
     (flet ((foo ((test :bool) (a :int) (b :int) (c :int) (d :int))
              (if test
                  (return (values a b c))
                  (return (values b c d)))))
       (vec4 (foo t 1 2 3 4) 2 3 4)))))

(5am:def-test values-21 (:suite multiple-value-return-tests)
  (finishes-p
   (varjo.tests::compile-vert () :410 t
     (labels ((gen-line ((x :int))
                (values x 1 2)))
       (gen-line 2)
       (multiple-value-bind (a b c) (gen-line 2)
         (values (v! a b c 4) (v! 2 2)))))))

(5am:def-test values-22 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert ((x :int)) :410 t
     (flet ((foo ()
              (if (= x 1)
                  (return (v! 1 1 1 1))
                  (v! 2 2 2 2))))
       (foo)))))

(5am:def-test values-23 (:suite multiple-value-return-tests)
  (finishes-p
   (compile-vert ((x :int)) :410 t
     (flet ((foo ()
              (if (= x 1)
                  (return (values (v! 1 1 1 1)
                                  (v! 2 2)))
                  (values (v! 3 3 3 3)
                          (v! 4 4)))))
       (foo)))))


;; These are not valid until #'vector is a function

;; (glsl-code
;;           (varjo.tests::compile-vert () :410 nil
;;             (funcall #'vector 10 20)
;;             (v! 0 0 0 0)))
;; (glsl-code
;;           (varjo.tests::compile-vert () :410 nil
;;             (labels ((foo ((x :float))
;;                        (values x x)))
;;               (multiple-value-call #'vector (foo 10))
;;               (v! 0 0 0 0))))
;; (glsl-code
;;           (varjo.tests::compile-vert () :410 nil
;;             (labels ((foo ((x :float))
;;                        (values x x)))
;;               (multiple-value-call #'vector (foo 10) (foo 10))
;;               (v! 0 0 0 0))))
;; (glsl-code
;;           (varjo.tests::compile-vert () :410 nil
;;             (labels ((foo ((x :float))
;;                        (values x x)))
;;               (multiple-value-call #'vector (foo 10) )
;;               (v! 0 0 0 0))))
