(in-package :varjo.tests)
(5am:in-suite compiler-macro-tests)

;;------------------------------------------------------------
;; Helper data

(add-external-function 'test-cm '((x :int) (y :float)) nil
                       '((* x y)))

(v-define-compiler-macro test-cm (&whole w (x :int) (y :float))
  (if (and (numberp x) (numberp y))
      (* x y)
      w))

(v-define-compiler-macro test-cm (&whole w (x :int) (y :int))
  (if (and (numberp x) (numberp y))
      (* x y)
      w))

(v-define-compiler-macro test-cm (&whole w (x v-type) (y v-type))
  (if (and (numberp x) (numberp y))
      (* x y)
      w))

;;------------------------------------------------------------
;; Tests

(5am:def-test compiler-macros-0 (:suite compiler-macro-tests)
  (finishes-p
   (compile-vert () :450 nil
     (test-cm 10 2s0)
     (v! 0 0 0 0))))

(5am:def-test compiler-macros-1 (:suite compiler-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (let ((x 1))
            (test-cm x 2s0))
          (v! 0 0 0 0)))
       '((progn
           (let ((x 1))
             (varjo.tests::test-cm x 2.0))
           (v! 0 0 0 0))))))

(5am:def-test compiler-macros-2 (:suite compiler-macro-tests)
  (is (equal
       (ast->code
        (varjo.tests::compile-vert () :450 nil
          (varjo.tests::test-cm 1 2s0)
          (v! 0 0 0 0)))
       '((progn 2.0 (v! 0 0 0 0))))))
