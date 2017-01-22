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
       '(v! 10 10 0 0))))

(5am:def-test symbol-macros-1 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (symbol-macrolet ((x 10))
            (let ((x 20))
              (v! x x))
            (v! x x 0 0))))
       '(progn
         (let ((x 20))
           (v! x x))
         (v! 10 10 0 0)))))

(5am:def-test symbol-macros-2 (:suite symbol-macro-tests)
  (is (equal
       (ast->code
        (compile-vert () :450 nil
          (symbol-macrolet ((x 10))
            (let ((x 20))
              (v! x (symbol-macrolet ((x 2s0))
                      (v! x x))))
            (v! x x 0 0))))
       '(progn
         (let ((x 20))
           (v! x (v! 2.0 2.0)))
         (v! 10 10 0 0)))))
