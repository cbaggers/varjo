(in-package :varjo.tests)
(5am:in-suite inline-glsl-tests)

(5am:def-test inline-glsl-0 (:suite inline-glsl-tests)
  (finishes-p
   (compile-vert ((a :int)) :450 nil
     (glsl-expr "vec4(1,2,3,4)" :vec4))))

(5am:def-test inline-glsl-1 (:suite inline-glsl-tests)
  (finishes
    (varjo::glsl-to-compile-result
     :fragment
     '(("color_in" :vec4))
     nil
     '(("color_out" :vec4))
     '(:330)
     "void main() { color_out = v_in.color_in; }")))
