(in-package :vari.cl)

;;----------------------------------------------------------------------

(def-v-type-class v-ratio (v-type)
  ((core :initform nil :reader core-typep)
   (glsl-string :initform "ivec2" :reader v-glsl-string)))

;;----------------------------------------------------------------------

(v-def-glsl-template-fun %ratio (a b) "vec2(~a, ~a)" (v-float v-float)
                         v-single-complex :pure t)

(v-def-glsl-template-fun numerator (a) "~a.x" (ratio)
                         v-int :pure t)

(v-def-glsl-template-fun denominator (a) "~a.y" (ratio)
                         v-int :pure t)
