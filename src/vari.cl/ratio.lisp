(in-package :vari.cl)

;;----------------------------------------------------------------------

(v-def-glsl-template-fun %ratio (a b) "ivec2(~a, ~a)" (v-float v-float)
                         v-ratio :pure t)

(v-def-glsl-template-fun numerator (a) "~a.x" (v-ratio)
                         v-int :pure t)

(v-def-glsl-template-fun denominator (a) "~a.y" (v-ratio)
                         v-int :pure t)
