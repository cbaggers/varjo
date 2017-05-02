(in-package :varjo)

;; Currently glsl-spec doesnt contain info on the built-in types, therefore we
;; add them manually here.

(v-defstruct v-depth-range-parameters ()
  (near v-float :accessor near)
  (far v-float :accessor far)
  (diff v-float :accessor diff))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-v-type-class v-per-vertex (v-ephemeral-type) ()))

(v-def-glsl-template-fun gl-position (vert-data) "~a.gl_Position" (v-per-vertex) :vec4)
(v-def-glsl-template-fun gl-point-size (vert-data) "~a.gl_PointSize" (v-per-vertex) :float)
(v-def-glsl-template-fun gl-clip-distance (vert-data) "~a.gl_ClipDistance" (v-per-vertex) (:float *))
