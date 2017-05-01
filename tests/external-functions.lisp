(in-package :varjo.tests)
(5am:in-suite external-functions-tests)

;;------------------------------------------------------------
;; Helper data

(varjo:add-external-function
 'test-ext '((ham :float)) nil
 `((progn (v! ham ham ham ham))))

(varjo:add-external-function
 'test-ext2 '((ham :float)) nil
 `((progn (values (v! ham ham ham ham) 2 3))))

(varjo:add-external-function
 'test-ext3 '((ham :float)) nil
 `((let ((jam (test-ext ham)))
     jam)))

;;------------------------------------------------------------
;; Tests

(5am:def-test ext-func-0 (:suite external-functions-tests)
  (glsl-contains-n-p 1 "vec4 TEST_EXT.*\\(float HAM\\);"
    (compile-vert () :450 nil
      (test-ext 10s0)
      (test-ext 10s0)
      (v! 0 0 0 0))))

(5am:def-test ext-func-1 (:suite external-functions-tests)
  (glsl-contains-n-p 1
      "vec4 TEST_EXT2.*\\(float HAM, out int return_1, out int return_2\\);"
    (compile-vert () :450 nil
      (test-ext2 10s0)
      (test-ext2 10s0)
      (v! 0 0 0 0))))

(5am:def-test ext-func-2 (:suite external-functions-tests)
  (glsl-contains-n-p 1
      "vec4 TEST_EXT3.*\\(float HAM.*\\);"
    (compile-vert () :450 nil
      (test-ext3 10s0)
      (test-ext3 10s0)
      (v! 0 0 0 0))))
