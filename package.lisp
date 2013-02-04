;;;; package.lisp

(defpackage #:varjo
  (:use #:cl)
  (:export :defshader
	   :flesh-out-type
           :glsl-defun
	   :mat-typep
	   :mat/vec-length
	   :rolling-translate
	   :split-shader-args
	   :struct-definition
           :translate
	   :type-aggregate-p
	   :type-arrayp
	   :type-component-count
	   :type-component-type
	   :type-gl-name
	   :type-glsl-size
	   :type-place
           :type-array-length 
           :type-built-inp
           :type-mat-col-to-vec
           :type-placep
           :type-principle
           :type-vec-core-type
           :vdefmacro
	   :vdefspecial
	   :vdefstruct
	   :vec-typep))

