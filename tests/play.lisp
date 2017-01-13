(in-package :varjo.tests)

(varjo:add-external-function
 'some-vec4er '((jam :float)) nil
 `((progn (v! jam jam jam))))

(glsl-code
 (varjo.tests::compile-vert () :450
   (some-vec4er 10s0)
   (v! 0 0 0 0)))

;; currently broken
(glsl-code
 (compile-vert () :450
   (let ((fn #'test-ext3))
     (funcall fn 10s0))
   (v! 0 0 0 0)))
