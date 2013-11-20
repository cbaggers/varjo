;;;; package.lisp

(defpackage #:varjo
  (:use #:cl :split-sequence)
  (:export :v-glsl-size
           :v-casts-to-p
           :v-casts-to
           :find-mutual-cast-type))

