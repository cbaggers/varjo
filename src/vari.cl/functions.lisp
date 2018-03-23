(in-package :vari.cl)

(v-def-glsl-template-fun not (a) "!~a" (:bool) :bool)
(v-def-glsl-template-fun not (a) "false" (t) :bool)

(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:vec4 :vec4) :vec4 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:vec3 :vec3) :vec3 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:vec2 :vec2) :vec2 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:float :float) :float :pure t)

(v-def-glsl-template-fun eql (a b) "(~a == ~a)" (v-real v-real) :bool)
(v-def-glsl-template-fun equal (a b) "(~a == ~a)" (v-real v-real) :bool)

(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-real v-real) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:vec2 :vec2) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:vec3 :vec3) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:vec4 :vec4) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:ivec2 :ivec2) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:ivec3 :ivec3) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:ivec4 :ivec4) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:uvec2 :uvec2) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:uvec3 :uvec3) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:uvec4 :uvec4) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:dvec2 :dvec2) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:dvec3 :dvec3) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:dvec4 :dvec4) :bool :pure t)

(v-def-glsl-template-fun zerop (a) "(~a == 0)" (v-number) :bool :pure t)
