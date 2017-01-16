(in-package :varjo.tests)
(5am:in-suite first-class-func-tests)

(5am:def-test f-c-func-0 (:suite first-class-func-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((fn (labels ((test ((x :int)) x))
                 #'test)))
       (v! 0 0 0 0)))))

(5am:def-test f-c-func-1 (:suite first-class-func-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((fn (labels ((test ((x :int)) x))
                 #'test)))
       (funcall fn 10)
       (v! 0 0 0 0)))))

(5am:def-test f-c-func-2 (:suite first-class-func-tests)
  (glsl-doesnt-contain-p "FN;"
    (compile-vert () :450 nil
      (let ((fn (labels ((test ((x :int)) x))
                  #'test)))
        fn
        (v! 0 0 0 0)))))

(5am:def-test f-c-func-3 (:suite first-class-func-tests)
  (glsl-doesnt-contain-p "FN;"
    (compile-vert () :450 nil
      (let ((fn (labels ((test ((x :int)) x))
                  #'test)))
        (labels ((foo ((ffn (function (:int) :int)))
                   (funcall ffn 10)))
          (foo fn))
        (v! 0 0 0 0)))))

(5am:def-test f-c-func-4 (:suite first-class-func-tests)
  (signals varjo-conditions:closures-not-supported
    (compile-vert () :450 nil
      (let* ((y 10)
             (fn (labels ((test ((x :int)) (* y x)))
                   #'test)))
        (v! 0 0 0 0)))))

(5am:def-test f-c-func-5 (:suite first-class-func-tests)
  (signals varjo-conditions:cross-scope-mutate
    (compile-vert () :450 nil
      (let* ((y 10)
             (fn (labels ((test ((x :int))
                            (setf y 2)
                            x))
                   #'test)))
        (funcall fn 10)
        (v! 0 0 0 0)))))

(5am:def-test f-c-func-6 (:suite first-class-func-tests)
  (signals varjo-conditions:cross-scope-mutate
    (compile-vert () :450 nil
      (let* ((y 10)
             (fn (labels ((test ((x :int)) x))
                   #'test)))
        (labels ((foo ((ffn (function (:int) :int)))
                   (setf y 2)
                   (funcall ffn 10)))
          (foo fn))
        (v! 0 0 0 0)))))

(5am:def-test f-c-func-7 (:suite first-class-func-tests)
  (signals varjo-conditions:symbol-unidentified
    (compile-vert () :450 nil
      (labels ((foo ((ffn (function (:int) :int)))
                 (funcall ffn y)))
        (let ((y 10))
          (foo (lambda ((a :int)) a))))
      (v! 0 0 0 0))))

(5am:def-test f-c-func-8 (:suite first-class-func-tests)
  (is (ast-stabalizes-p
       (compile-vert () :450 nil
         (let ((fn (labels ((test ((x :int)) x))
                     #'test)))
           (labels ((foo ((ffn (function (:int) :int)))
                      (funcall ffn 10)))
             (foo fn))
           (v! 0 0 0 0))))))

(5am:def-test f-c-func-9 (:suite first-class-func-tests)
  (glsl-doesnt-contain-p "<invalid>"
    (compile-vert ((a :int)) :450 nil
      (let ((x 1)
            (fn (lambda () (lambda ((x :float)) (* x 2)))))
        (v! 0 0 0 (funcall (funcall fn) x))))))

(5am:def-test f-c-func-10 (:suite first-class-func-tests)
  (glsl-doesnt-contain-p "<invalid>"
    (compile-vert ((a :int)) :450 nil
      (let ((x 1))
        (labels ((fn ((thr (function (:float) :float)) (x :int))
                   (+ 1 x 3)
                   thr))
          (v! 0 0 0 (funcall (fn (lambda ((x :float)) (* x 2)) 10)
                             x)))))))
