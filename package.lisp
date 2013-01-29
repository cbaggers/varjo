;;;; package.lisp

(defpackage #:varjo
  (:use #:cl)
  (:export :defshader
	   :flesh-out-type
           :glsl-defun
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
           :type-placep
           :type-principle
           :vdefmacro
	   :vdefspecial
	   :vdefstruct))

