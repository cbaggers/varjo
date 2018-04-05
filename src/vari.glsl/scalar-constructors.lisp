(in-package :vari.glsl)

;;------------------------------------------------------------
;; Scalar Constructors

(v-def-glsl-template-fun int (x) "~a" (v-int) v-int :pure t)
(v-def-glsl-template-fun uint (x) "~a" (v-uint) v-uint :pure t)
(v-def-glsl-template-fun bool (x) "~a" (v-bool) v-bool :pure t)
(v-def-glsl-template-fun float (x) "~a" (v-float) v-float :pure t)
(v-def-glsl-template-fun double (x) "~a" (v-double)  v-double :pure t)

(v-def-glsl-template-fun int (x) "int(~a)" (v-uint) v-int :pure t)
(v-def-glsl-template-fun int (x) "int(~a)" (v-bool) v-int :pure t)
(v-def-glsl-template-fun int (x) "int(~a)" (v-float) v-int :pure t)
(v-def-glsl-template-fun int (x) "int(~a)" (v-double)  v-int :pure t)
;; uint handled by special form
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-int) v-bool :pure t)
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-uint) v-bool :pure t)
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-float)  v-bool :pure t)
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-double)  v-bool :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-int) v-float :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-uint)  v-float :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-bool)  v-float :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-double) v-float :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-int)  v-double :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-uint)  v-double :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-bool)  v-double :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-float) v-double :pure t)

(v-def-glsl-template-fun atomic-counter (c &context (:330 :440))
  "atomicCounter(~a)"
  (v-atomic-uint) v-uint)
;;------------------------------------------------------------
