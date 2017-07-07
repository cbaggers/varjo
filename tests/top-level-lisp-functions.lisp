(in-package :varjo.tests)
(5am:in-suite top-level-lisp-function-decls-tests)

;;------------------------------------------------------------
;; Helper data

(varjo:add-top-level-lisp-function-decl
 'test-ext '((ham :float)) nil
 `((progn (v! ham ham ham ham))))

(varjo:add-top-level-lisp-function-decl
 'test-ext2 '((ham :float)) nil
 `((progn (values (v! ham ham ham ham) 2 3))))

(varjo:add-top-level-lisp-function-decl
 'test-ext3 '((ham :float)) nil
 `((let ((jam (test-ext ham)))
     jam)))

;;------------------------------------------------------------
;; Tests

(5am:def-test ext-func-0 (:suite top-level-lisp-function-decls-tests)
  (glsl-contains-n-p 1 "vec4 TEST_EXT.*\\(float HAM\\);"
    (compile-vert () :450 nil
      (test-ext 10s0)
      (test-ext 10s0)
      (v! 0 0 0 0))))

(5am:def-test ext-func-1 (:suite top-level-lisp-function-decls-tests)
  (glsl-contains-n-p 1
      "vec4 TEST_EXT2.*\\(float HAM, out int return_1, out int return_2\\);"
    (compile-vert () :450 nil
      (test-ext2 10s0)
      (test-ext2 10s0)
      (v! 0 0 0 0))))

(5am:def-test ext-func-2 (:suite top-level-lisp-function-decls-tests)
  (glsl-contains-n-p 1
      "vec4 TEST_EXT3.*\\(float HAM.*\\);"
    (compile-vert () :450 nil
      (test-ext3 10s0)
      (test-ext3 10s0)
      (v! 0 0 0 0))))
