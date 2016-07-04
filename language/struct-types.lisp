(in-package :varjo)

(v-defstruct v-depth-range-parameters ()
  (near v-float :accessor near)
  (far v-float :accessor far)
  (diff v-float :accessor diff))
