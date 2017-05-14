(in-package :varjo.cl)

(v-def-glsl-template-fun not (a) "!~a" (v-bool) v-bool)
(v-def-glsl-template-fun not (a) "false" (v-type) v-bool)
(v-def-glsl-template-fun %equal (a b) "(~a == ~a)" (v-number v-number) v-bool)
(v-def-glsl-template-fun %eql (a b) "(~a == ~a)" (v-number v-number) v-bool)
(v-def-glsl-template-fun %/= (a b) "(~a != ~a)" (v-number v-number) v-bool)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-vec4 v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-vec3 v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-vec2 v-vec2) v-vec2 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-float v-float) v-float :pure t)
