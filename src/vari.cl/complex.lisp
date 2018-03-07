(in-package :vari.cl)

;;----------------------------------------------------------------------

(v-def-glsl-template-fun complex (a b) "vec2(~a, ~a)" (v-float v-float)
                         v-complex :pure t)

(v-def-glsl-template-fun complex (a) "vec2(~a, 0.0f)" (v-float)
                         v-complex :pure t)



(v-def-glsl-template-fun realpart (a) "~a.x" (v-complex)
                         v-float :pure t)

(v-def-glsl-template-fun imagpart (a) "~a.y" (v-complex)
                         v-float :pure t)

;;----------------------------------------------------------------------

;; currently complex isnt a v-number

(v-def-glsl-template-fun conjugate (x) "~a" (v-number) 0 :pure t)

(v-defun conjugate ((x v-complex))
  (complex (realpart x) (- (imagpart x))))

;; cis
(v-defun cis ((x v-float))
  (complex (cos x) (sin x)))
