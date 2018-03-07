(in-package :vari.cl)

;;----------------------------------------------------------------------

(def-v-type-class v-complex (v-type) ())

(def-v-type-class v-single-complex (v-complex)
  ((core :initform nil :reader core-typep)
   (glsl-string :initform "vec2" :reader v-glsl-string)))

(def-v-type-class v-double-complex (v-complex)
  ((core :initform nil :reader core-typep)
   (glsl-string :initform "dvec2" :reader v-glsl-string)))

;;----------------------------------------------------------------------

(v-def-glsl-template-fun complex (a b) "vec2(~a, ~a)" (v-float v-float)
                         v-single-complex :pure t)

(v-def-glsl-template-fun complex (a b) "dvec2(~a, ~a)" (v-double v-double)
                         v-double-complex :pure t)

(v-def-glsl-template-fun complex (a b) "vec2(~a, 0.0f)" (v-float)
                         v-single-complex :pure t)

(v-def-glsl-template-fun complex (a b) "dvec2(~a, 0.0f)" (v-double)
                         v-double-complex :pure t)



(v-def-glsl-template-fun realpart (a) "~a.x" (v-single-complex)
                         v-float :pure t)

(v-def-glsl-template-fun imagpart (a) "~a.y" (v-single-complex)
                         v-float :pure t)

(v-def-glsl-template-fun realpart (a) "~a.x" (v-double-complex)
                         v-double :pure t)

(v-def-glsl-template-fun imagpart (a) "~a.y" (v-double-complex)
                         v-double :pure t)
