(in-package :varjo)

(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (out-vars :initarg :out-vars :accessor out-vars)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)
   (used-macros :initarg :used-macros :reader used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
			 :reader used-compiler-macros)
   (ast :initarg :ast :reader ast)
   (used-symbol-macros :initarg :used-symbol-macros
		       :reader used-symbol-macros)
   (third-party-metadata :initarg :third-party-metadata
			 :initform (make-hash-table)
			 :reader third-party-metadata)))
;; {NOTE} third-party-metadata only applies to rolling-translate
