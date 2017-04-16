(in-package :varjo)

;; Currently glsl-spec doesnt contain info on the built-in types, therefore we
;; add them manually here.

(v-defstruct v-depth-range-parameters ()
  (near v-float :accessor near)
  (far v-float :accessor far)
  (diff v-float :accessor diff))

(def-v-type-class v-per-vertex (v-ephemeral-type) ())

(v-defun gl-position (vert-data) "~a.gl_Position" (v-per-vertex) :vec4)
(v-defun gl-point-size (vert-data) "~a.gl_PointSize" (v-per-vertex) :float)
(v-defun gl-clip-distance (vert-data) "~a.gl_ClipDistance" (v-per-vertex) (:float *))
