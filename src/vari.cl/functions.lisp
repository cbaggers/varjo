(in-package :vari.cl)

;;------------------------------------------------------------

(v-def-glsl-template-fun identity (x) "(~a)" (v-type) 0 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun not (a) "!~a" (:bool) :bool)
(v-def-glsl-template-fun not (a) "false" (t) :bool)

;;------------------------------------------------------------

(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:vec4 :vec4) :vec4 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:vec3 :vec3) :vec3 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:vec2 :vec2) :vec2 :pure t)
(v-def-glsl-template-fun expt (x y) "pow(~a, ~a)" (:float :float) :float :pure t)
(v-def-glsl-template-fun eql (a b) "(~a == ~a)" (:short-float :short-float) :bool :pure t)

(v-def-glsl-template-fun equal (a b) "(~a == ~a)" (:short-float :short-float) :bool :pure t)
(v-def-glsl-template-fun equal (a b) "(~a == ~a)" (v-array v-array) :bool :pure t)

(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (:short-float :short-float) :bool :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (v-array v-array) :bool :pure t)

(v-def-glsl-template-fun zerop (a) "(~a == 0)" (v-number) :bool :pure t)

;;------------------------------------------------------------
