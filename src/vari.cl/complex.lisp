(in-package :vari.cl)

;;----------------------------------------------------------------------

(v-def-glsl-template-fun complex (a b) "vec2(~a, ~a)" (v-float v-float)
                         complex :pure t)

(v-def-glsl-template-fun complex (a) "vec2(~a, 0.0f)" (v-float)
                         complex :pure t)



(v-def-glsl-template-fun realpart (a) "~a.x" (complex)
                         v-float :pure t)

(v-def-glsl-template-fun imagpart (a) "~a.y" (complex)
                         v-float :pure t)

;;----------------------------------------------------------------------

(v-def-glsl-template-fun conjugate (x) "~a" (v-double) v-double :pure t)
(v-def-glsl-template-fun conjugate (x) "~a" (v-short-float) v-short-float :pure t)
(v-def-glsl-template-fun conjugate (x) "~a" (v-float) v-float :pure t)
(v-def-glsl-template-fun conjugate (x) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun conjugate (x) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun conjugate (x) "~a" (v-rational) v-rational :pure t)

(v-defun conjugate ((x complex))
  (complex (realpart x) (- (imagpart x))))

;; cis
(v-defun cis ((x v-float))
  (complex (cos x) (sin x)))

(v-def-glsl-template-fun = (a b) "(~a == ~a)" (complex complex) v-bool :pure t)
(v-def-glsl-template-fun eql (a b) "(~a == ~a)" (complex complex) v-bool)
(v-def-glsl-template-fun equal (a b) "(~a == ~a)" (complex complex) v-bool)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (complex complex) complex :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (complex) complex :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (complex complex) complex :pure t)
;; (v-def-glsl-template-fun * (a b) "(~a * ~a)" (complex complex) complex :pure t)
;; (v-def-glsl-template-fun / (a b) "(~a / ~a)" (complex complex) complex :pure t)
(v-def-glsl-template-fun /= (a b) "(~a != ~a)" (complex complex) v-bool :pure t)

(v-defun phase ((n v-float))
  (atan 0 (realpart x)))

(v-defun phase ((n complex))
  (atan (imagpart x) (realpart x)))
