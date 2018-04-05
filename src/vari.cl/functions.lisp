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
;; GLSL only has 2 arg min & max

(v-def-glsl-template-fun max (a b c &rest c) "max(~a, max(~a, max(~a~{ ,~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro max ((a t) (b t) (c t) &rest (d t))
  `(max ,a (max ,b (max ,c ,@d))))

(v-def-glsl-template-fun min (a b c &rest c) "min(~a, min(~a, min(~a~{ ,~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro min ((a t) (b t) (c t) &rest (d t))
  `(min ,a (min ,b (min ,c ,@d))))

(v-def-glsl-template-fun max (a) "~a" (:float) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec2) :vec2 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec3) :vec3 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec4) :vec4 :pure t)

(v-def-glsl-template-fun max (a) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec2) :ivec2 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec3) :ivec3 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec4) :ivec4 :pure t)

(v-def-glsl-template-fun max (a) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec2) :uvec2 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec3) :uvec3 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec4) :uvec4 :pure t)

(v-def-glsl-template-fun min (a) "~a" (:float) :float :pure t)
(v-def-glsl-template-fun min (a) "~a" (:vec2) :vec2 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:vec3) :vec3 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:vec4) :vec4 :pure t)

(v-def-glsl-template-fun min (a) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun min (a) "~a" (:ivec2) :ivec2 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:ivec3) :ivec3 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:ivec4) :ivec4 :pure t)

(v-def-glsl-template-fun min (a) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun min (a) "~a" (:uvec2) :uvec2 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:uvec3) :uvec3 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:uvec4) :uvec4 :pure t)

;;------------------------------------------------------------
