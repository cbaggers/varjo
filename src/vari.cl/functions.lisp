(in-package :vari.cl)

(v-def-glsl-template-fun not (a) "!~a" (v-bool) v-bool)
(v-def-glsl-template-fun not (a) "false" (v-type) v-bool)

(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-vec4 v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-vec3 v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-vec2 v-vec2) v-vec2 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (v-float v-float) v-float :pure t)

(v-def-glsl-template-fun eql (a b) "(~a == ~a)" (v-number v-number) v-bool)
(v-def-glsl-template-fun equal (a b) "(~a == ~a)" (v-number v-number) v-bool)

(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-number v-number) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-vec2 v-vec2) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-vec3 v-vec3) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-vec4 v-vec4) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-ivec2 v-ivec2) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-ivec3 v-ivec3) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-ivec4 v-ivec4) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-uvec2 v-uvec2) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-uvec3 v-uvec3) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-uvec4 v-uvec4) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-dvec2 v-dvec2) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-dvec3 v-dvec3) v-bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-dvec4 v-dvec4) v-bool :pure t)

(v-def-glsl-template-fun zerop (a) "(~a == 0)" (v-number) v-bool :pure t)
