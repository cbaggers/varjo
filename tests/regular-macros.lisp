(in-package :varjo.tests)
(5am:in-suite regular-macro-tests)

;;------------------------------------------------------------
;; Helper data

(v-defmacro test-rm-0 (x y)
  `(v! ,x ,y))

(v-defmacro test-rm-1 (&environment env x y)
  `(v! ,x ,y))

;;------------------------------------------------------------
;; Tests

(5am:def-test regular-macros-0 (:suite regular-macro-tests)
  (finishes-p
   (compile-vert () :450 nil
     (test-rm-0 10 2s0)
     (v! 0 0 0 0))))

(5am:def-test regular-macros-1 (:suite regular-macro-tests)
  (finishes-p
   (compile-vert () :450 nil
     (test-rm-1 10 2s0)
     (v! 0 0 0 0))))
