(in-package :varjo.tests)
(5am:in-suite symbol-macro-tests)

;;------------------------------------------------------------
;; Tests

(5am:def-test symbol-macros-0 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (symbol-macrolet ((x 10))
            (v! x x 0 0))))
       '((v! 10 10 0 0)))))

(5am:def-test symbol-macros-1 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (symbol-macrolet ((x 10))
            (let ((x 20))
              (v! x x))
            (v! x x 0 0))))
       '((progn
           (let ((x 20))
             (v! x x))
           (v! 10 10 0 0))))))

(5am:def-test symbol-macros-2 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (symbol-macrolet ((x 10))
            (let ((x 20))
              (v! x (symbol-macrolet ((x 2s0))
                      (v! x x))))
            (v! x x 0 0))))
       '((progn
           (let ((x 20))
             (v! x (v! 2.0 2.0)))
           (v! 10 10 0 0))))))

(5am:def-test symbol-macros-3 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (let ((v (v! 0 0 0 0)))
            (symbol-macrolet ((x (x v)))
              (v! x x x x)))))
       '((let ((v (v! 0 0 0 0)))
           (v! (x v) (x v) (x v) (x v)))))))

(5am:def-test symbol-macros-4 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (varjo.tests::compile-vert () :450 nil
          (let ((v (v! 0 0 0 0)))
            (symbol-macrolet ((x (x v)))
              (let ((y 0s0))
                (setf x 10s0)
                (setf y 10s0))
              (v! x x x x)))))
       '((let ((v (v! 0 0 0 0)))
           (let ((y 0.0))
             (setf (x v) 10.0)
             (setf y 10.0))
           (v! (x v) (x v) (x v) (x v)))))))
