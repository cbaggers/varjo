(in-package :vari.glsl)

;;------------------------------------------------------------
;; Not standard additional constructors.. I think I'll remove these

(v-def-glsl-template-fun vec3 (x y) "vec3(~a, ~a, 0.0f)" (v-float v-float) v-vec3 :pure t)

(v-def-glsl-template-fun vec4 (x y) "vec4(~a, ~a, 0.0f, 0.0f)" (v-float v-float) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a, ~a, ~a, 0.0f)" (v-float v-float v-float) v-vec4 :pure t)

(v-def-glsl-template-fun dvec3 (x y) "dvec3(~a, ~a, 0.0lf)" (v-double v-double) v-dvec3 :pure t)
(v-def-glsl-template-fun dvec4 (x y) "dvec4(~a, ~a, 0.0lf, 0.0lf)" (v-double v-double) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y z) "dvec4(~a, ~a, ~a, 0.0lf)" (v-double v-double v-double) v-dvec4 :pure t)

;;------------------------------------------------------------
