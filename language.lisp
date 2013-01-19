;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(defun vlambda (&key in-args output-type transform
		  context-restriction)
  (list (mapcar #'flesh-out-type
		(mapcar #'second in-args))
	(flesh-out-type output-type)
	transform
	(mapcar #'(lambda (x) (find :compatible x)) in-args)
	(mapcar #'(lambda (x) (find :match x)) in-args)
	context-restriction))

(defun glsl-defun (&key name in-args output-type
		     transform context-restriction)
  (let* ((func-spec (vlambda :in-args in-args 
			     :output-type output-type
			     :transform transform
			     :context-restriction 
			     context-restriction)))
    (setf *glsl-functions*
	  (acons name (cons func-spec
			    (assocr name *glsl-functions*))
		 *glsl-functions*))))

(defun func-specs (name)
  (let ((all-matching (assocr name *glsl-functions*)))
    (remove-if 
     #'null (loop for spec in all-matching
		  :collect (if (func-restriction spec)
			       (when (find *shader-type* 
					   (func-restriction spec))
				 spec)
			       spec)))))

(defun vfunctionp (name)
  (not (null (func-specs name))))

(defun special-functionp (symbol)
  (not (null (gethash symbol *glsl-special-functions*))))

(defun funcall-special (symbol arg-objs)
  (funcall (gethash symbol *glsl-special-functions*)
	   arg-objs))

(defun register-special-function (symbol function)
  (setf (gethash symbol *glsl-special-functions*) 
	function))

(defmacro vdefspecial (name (code-var) &body body)
  `(register-special-function
    ',name
    (lambda (,code-var)
      ,@body)))

(defun register-substitution (symbol function)
  (setf *glsl-substitutions*
	(acons symbol function *glsl-substitutions*)))

(defun substitutionp (symbol)
  (not (null (assoc symbol *glsl-substitutions*))))

(defun substitution (symbol)
  (assocr symbol *glsl-substitutions*))

(defmacro vdefmacro (name lambda-list &body body)
  `(register-substitution
    ',name
    (lambda ,lambda-list
      ,@body)))

(defun varjo-type->glsl-type (type)
  (let ((principle (first type))
	(len (second type)))
    (if len
	(format nil "~a[~a]" principle (if (numberp len) len ""))
	(format nil "~a" principle))))

(defun instance-var (symbol)
  (let ((var-spec (assoc symbol *glsl-variables*)))
    (make-instance 'code
		   :type (set-place-t
			  (flesh-out-type (var-type var-spec)))
		   :current-line (format nil "~a" 
					 (var-gl-name var-spec))
		   :read-only (var-read-only var-spec))))

;;------------------------------------------------------------
;; Core Language Definitions
;;---------------------------

(glsl-defun :name 'wah
            :in-args '((x ((:double :float :int :uint :bool
			    :bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "bool(~a)"
	    :context-restriction '(:vertex))

(glsl-defun :name 'bool
            :in-args '((x ((:double :float :int :uint :bool
			    :bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "bool(~a)")

(glsl-defun :name 'double
            :in-args '((x ((:bool :float :int :uint :double))))
            :output-type :double
            :transform "double(~a)")

(glsl-defun :name 'float
            :in-args '((x ((:bool :double :int :uint :float
			    :vec2 :vec3 :vec4))))
            :output-type :float
            :transform "float(~a)")

(glsl-defun :name 'int
            :in-args '((x ((:bool :double :float :uint :int
			    :ivec2 :ivec3 :ivec4))))
            :output-type :int
            :transform "int(~a)")

(glsl-defun :name 'uint
            :in-args '((x ((:bool :double :float :int :uint
			    :uvec2 :uvec3 :uvec4))))
            :output-type :uint
            :transform "uint(~a)")

(glsl-defun :name 'uint
            :in-args '((x ((:bool :double :float :int :uint
			    :uvec2 :uvec3 :uvec4))))
            :output-type :uint
            :transform "uint(~a)")

(glsl-defun :name 'degrees
            :in-args '((radians ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "degrees(~a)")

(glsl-defun :name 'radians
            :in-args '((degrees ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "radians(~a)")

(glsl-defun :name 'sin
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "sin(~a)")

(glsl-defun :name 'cos
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "cos(~a)")

(glsl-defun :name 'tan
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "tan(~a)")

(glsl-defun :name 'asin
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "asin(~a)")

(glsl-defun :name 'acos
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "acos(~a)")

(glsl-defun :name 'atan
            :in-args '((y ((:float :vec2 :vec3 :vec4)) :compatible)
		       (x ((:float :vec2 :vec3 :vec4)) :compatible))
            :output-type '(0 nil)
            :transform "atan(~a, ~a)")

(glsl-defun :name 'atan
            :in-args '((y-over-x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "atan(~a)")

(glsl-defun :name 'sinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "sinh(~a)")

(glsl-defun :name 'cosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "cosh(~a)")

(glsl-defun :name 'tanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "tanh(~a)")

(glsl-defun :name 'asinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "asinh(~a)")

(glsl-defun :name 'acosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "acosh(~a)")

(glsl-defun :name 'atanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "atanh(~a)")

(glsl-defun :name 'pow
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "pow(~a, ~a)")

(glsl-defun :name 'exp
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "exp(~a)")

(glsl-defun :name 'log
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "log(~a)")

(glsl-defun :name 'exp2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "exp2(~a)")

(glsl-defun :name 'log2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "log2(~a)")

(glsl-defun :name 'sqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "exp(~a)")

(glsl-defun :name 'inversesqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "inversesqrt(~a)")

(glsl-defun :name 'abs
            :in-args '((x ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4))))
            :output-type '(0 nil)
            :transform "abs(~a)")

(glsl-defun :name 'sign
            :in-args '((x ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4))))
            :output-type '(:float nil)
            :transform "sign(~a)")

(glsl-defun :name 'floor
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "floor(~a)")

(glsl-defun :name 'trunc
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "trunc(~a)")

(glsl-defun :name 'round
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "round(~a)")

(glsl-defun :name 'round-even
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "roundEven(~a)")

(glsl-defun :name 'ceil
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "ceil(~a)")

(glsl-defun :name 'fract
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "fract(~a)")

(glsl-defun :name 'mod
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "mod(~a, ~a)")

(glsl-defun :name 'min
            :in-args '((x ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4
			    :uint :uvec2 :uvec3 :uvec4)) :match)
		       (y ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4
			    :uint :uvec2 :uvec3 :uvec4)) :match))
            :output-type '(0 nil)
            :transform "min(~a, ~a)")

(glsl-defun :name 'min
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y :float))
            :output-type '(0 nil)
            :transform "min(~a, ~a)")

(glsl-defun :name 'min
            :in-args '((x ((:int :ivec2 :ivec3 :ivec4)))
		       (y :int))
            :output-type '(0 nil)
            :transform "min(~a, ~a)")

(glsl-defun :name 'min
            :in-args '((x ((:uint :uvec2 :uvec3 :uvec4)))
		       (y :uint))
            :output-type '(0 nil)
            :transform "min(~a, ~a)")

(glsl-defun :name 'max
            :in-args '((x ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4
			    :uint :uvec2 :uvec3 :uvec4)) :match)
		       (y ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4
			    :uint :uvec2 :uvec3 :uvec4)) :match))
            :output-type '(0 nil)
            :transform "max(~a, ~a)")

(glsl-defun :name 'max
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y :float))
            :output-type '(0 nil)
            :transform "max(~a, ~a)")

(glsl-defun :name 'max
            :in-args '((x ((:int :ivec2 :ivec3 :ivec4)))
		       (y :int))
            :output-type '(0 nil)
            :transform "max(~a, ~a)")

(glsl-defun :name 'max
            :in-args '((x ((:uint :uvec2 :uvec3 :uvec4)))
		       (y :uint))
            :output-type '(0 nil)
            :transform "max(~a, ~a)")

(glsl-defun :name 'clamp
            :in-args '((x ((:float :vec2 :vec3 :vec4
			    :int :ivec2 :ivec3 :ivec4
			    :uint :uvec2 :uvec3 :uvec4)) :match)
		       (min-val ((:float :vec2 :vec3 :vec4
				  :int :ivec2 :ivec3 :ivec4
				  :uint :uvec2 :uvec3 :uvec4)) 
			:match)
		       (max-val ((:float :vec2 :vec3 :vec4
				  :int :ivec2 :ivec3 :ivec4
				  :uint :uvec2 :uvec3 :uvec4))
			:match))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)")

(glsl-defun :name 'clamp
            :in-args '((x ((:float :vec2 :vec3 :vec4)) )
		       (min-val :float )
		       (max-val :float ))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)")

(glsl-defun :name 'clamp
            :in-args '((x ((:int :ivec2 :ivec3 :ivec4)) )
		       (min-val :int )
		       (max-val :int ))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)")

(glsl-defun :name 'clamp
            :in-args '((x ((:uint :uvec2 :uvec3 :uvec4)) 
			)
		       (min-val :uint )
		       (max-val :uint ))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)")

(glsl-defun :name 'mix
            :in-args '((x ((:float :vec2 :vec3 :vec4)) :match)
		       (y ((:float :vec2 :vec3 :vec4)) :match)
		       (a ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(0 nil)
            :transform "mix(~a, ~a, ~a)")

(glsl-defun :name 'mix
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y ((:float :vec2 :vec3 :vec4)))
		       (a ((:float :bvec2 :bvec3 :bvec4 :bool))))
            :output-type '(0 nil)
            :transform "mix(~a, ~a, ~a)")

(glsl-defun :name 'smooth-step
            :in-args '((edge0 ((:float :vec2 :vec3 :vec4)) :match)
		       (edge1 ((:float :vec2 :vec3 :vec4)) :match)
		       (x ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(2 nil)
            :transform "smoothstep(~a, ~a, ~a)")

(glsl-defun :name 'smooth-step
            :in-args '((edge0 :float)
		       (edge1 :float)
		       (x ((:float :vec2 :vec3 :vec4))))
            :output-type '(2 nil)
            :transform "smoothstep(~a, ~a, ~a)")

(glsl-defun :name 'is-nan
            :in-args '((x :float))
            :output-type '(:bool nil)
            :transform "isnan(~a, ~a, ~a)")

(glsl-defun :name 'is-nan
            :in-args '((x :vec2))
            :output-type '(:bvec2 nil)
            :transform "isnan(~a, ~a, ~a)")

(glsl-defun :name 'is-nan
            :in-args '((x :vec3))
            :output-type '(:bvec3 nil)
            :transform "isnan(~a, ~a, ~a)")

(glsl-defun :name 'is-nan
            :in-args '((x :vec4))
            :output-type '(:bvec4 nil)
            :transform "isnan(~a, ~a, ~a)")

(glsl-defun :name 'length
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type :float
            :transform "length(~a)")

(glsl-defun :name 'distance
            :in-args '((p0 ((:float :vec2 :vec3 :vec4)) :match)
		       (p1 ((:float :vec2 :vec3 :vec4)) :match))
            :output-type :float
            :transform "distance(~a, ~a)")

(glsl-defun :name 'dot
            :in-args '((x ((:float :vec2 :vec3 :vec4)) :match)
		       (y ((:float :vec2 :vec3 :vec4)) :match))
            :output-type :float
            :transform "dot(~a, ~a)")

(glsl-defun :name 'cross
            :in-args '((x :vec3)
		       (y :vec3))
            :output-type :vec3
            :transform "cross(~a, ~a)")

(glsl-defun :name 'normalize
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "cross(~a, ~a)")

(glsl-defun :name 'aref
            :in-args '((array (t t))
		       (index ((:uint :int))))
            :output-type '(0 nil t)
            :transform "~a[~a]")

(glsl-defun :name 'aref
            :in-args '((vector ((:vec2 :vec3 :vec4)))
		       (index ((:uint :int))))
            :output-type '(:float 0 t)
            :transform "~a[~a]")

(glsl-defun :name 'aref
            :in-args '((vector ((:ivec2 :ivec3 :ivec4)))
		       (index ((:uint :int))))
            :output-type '(:int 0 t)
            :transform "~a[~a]")

(glsl-defun :name 'aref
            :in-args '((vector ((:uvec2 :uvec3 :uvec4)))
		       (index ((:uint :int))))
            :output-type '(:uint 0 t)
            :transform "~a[~a]")

(glsl-defun :name 'setf
            :in-args '((x (t nil t) :match)
		       (y (t nil nil) :match))
            :output-type '(0 0)
            :transform "~a = ~a")

(glsl-defun :name 'setf
            :in-args '((x (t t t) :match)
		       (y (t t nil) :match))
            :output-type '(0 0)
            :transform "~a = ~a")

(glsl-defun :name 'f-transform
            :in-args '()
            :output-type :vec4
            :transform "ftransform()")

(glsl-defun :name 'face-forward
            :in-args '((n ((:float :vec2 :vec3 :vec4)) :match)
		       (i ((:float :vec2 :vec3 :vec4)) :match)
		       (nref ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(0 0)
            :transform "faceforward(~a, ~a, ~a)")

(glsl-defun :name 'discard
            :in-args '()
            :output-type :none
            :transform "discard()"
	    :context-restriction '(:fragment))

(glsl-defun :name '*
            :in-args '((x ((:int :float)))
		       (y ((:int :float))))
            :output-type '(0 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:int :float)))
		       (y ((:vec2 :vec3 :vec4
			    :ivec2 :ivec3 :ivec4
			    :mat2 :mat3 :mat4 
			    :mat2x2 :mat2x3 :mat2x4
			    :mat3x2 :mat3x3 :mat3x4
			    :mat4x2 :mat4x3 :mat4x4))))
            :output-type '(0 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:vec2 :vec3 :vec4
			    :ivec2 :ivec3 :ivec4
			    :mat2 :mat3 :mat4 
			    :mat2x2 :mat2x3 :mat2x4
			    :mat3x2 :mat3x3 :mat3x4
			    :mat4x2 :mat4x3 :mat4x4)))
		       (y ((:int :float))))
            :output-type '(0 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:vec2 :vec3 :vec4
			    :ivec2 :ivec3 :ivec4)) :compatible)
		       (y ((:vec2 :vec3 :vec4
			    :ivec2 :ivec3 :ivec4)) :compatible))
            :output-type '(0 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:mat2x2 :mat2x3 :mat2x4)))
		       (y ((:vec2 :ivec2))))
            :output-type '(0 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:mat3x2 :mat3x3 :mat3x4)))
		       (y ((:vec3 :ivec3))))
            :output-type '(0 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:mat4x2 :mat4x3 :mat4x4)))
		       (y ((:vec4 :ivec4))))
            :output-type '(0 nil)
            :transform "(~a * ~a)")


(glsl-defun :name '%
            :in-args '((x ((:int :uint :ivec2 :uvec2 
			    :ivec3 :uvec3 :ivec4 :uvec4)))
		       (y ((:int :uint))))
            :output-type '(0 nil)
            :transform "(~a % ~a)")

(glsl-defun :name '<
            :in-args '((x ((:float :int)))
		       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a < ~a)")

(glsl-defun :name '>
            :in-args '((x ((:float :int)))
		       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a > ~a)")

(glsl-defun :name '<=
            :in-args '((x ((:float :int)))
		       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a <= ~a)")

(glsl-defun :name '>=
            :in-args '((x ((:float :int)))
		       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a >= ~a)")

(glsl-defun :name '==
	    :in-args '((a (t t) :compatible)
		       (b (t t) :compatible))
	    :output-type '(:bool nil)
	    :transform "(~a == ~a)")

(glsl-defun :name '!=
	    :in-args '((a (t t) :compatible)
		       (b (t t) :compatible))
	    :output-type '(:bool nil)
	    :transform "(~a != ~a)")

(glsl-defun :name '==
	    :in-args '((a (t nil) :compatible)
		       (b (t nil) :compatible))
	    :output-type '(:bool nil)
	    :transform "(~a == ~a)")

(glsl-defun :name '!=
	    :in-args '((a (t nil) :compatible)
		       (b (t nil) :compatible))
	    :output-type '(:bool nil)
	    :transform "(~a != ~a)")

(glsl-defun :name '!
	    :in-args '((a (:bool nil)))
	    :output-type '(:bool nil)
	    :transform "(! ~a)")

(glsl-defun :name '~
	    :in-args '((a ((:int :uint :ivec2 :ivec3 :ivec4) 
			   nil)))
	    :output-type '(0 nil)
	    :transform "(~ ~a)")

(glsl-defun :name '<<
	    :in-args '((a ((:int :uint :float) nil))
		       (b ((:int :uint :float) nil)))
	    :output-type '(0 nil)
	    :transform "(~a << ~a)")

(glsl-defun :name '<<
	    :in-args '((a ((:ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil))
		       (b ((:int :uint :float) nil)))
	    :output-type '(0 nil)
	    :transform "(~a << ~a)")

(glsl-defun :name '<<
	    :in-args '((a ((:ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :compatible)
		       (b ((:ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :compatible))
	    :output-type '(0 nil)
	    :transform "(~a << ~a)")

(glsl-defun :name '>>
	    :in-args '((a ((:int :uint :float) nil))
		       (b ((:int :uint :float) nil)))
	    :output-type '(0 nil)
	    :transform "(~a >> ~a)")

(glsl-defun :name '>>
	    :in-args '((a ((:ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil))
		       (b ((:int :uint :float) nil)))
	    :output-type '(0 nil)
	    :transform "(~a >> ~a)")

(glsl-defun :name '>>
	    :in-args '((a ((:ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :compatible)
		       (b ((:ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :compatible))
	    :output-type '(0 nil)
	    :transform "(~a >> ~a)")

(glsl-defun :name '&
	    :in-args '((a ((:int :uint
			    :ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :match)
		       (b ((:int :uint
			    :ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :match))
	    :output-type '(0 nil)
	    :transform "(~a & ~a)")

(glsl-defun :name '^
	    :in-args '((a ((:int :uint
			    :ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :match)
		       (b ((:int :uint
			    :ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :match))
	    :output-type '(0 nil)
	    :transform "(~a ^ ~a)")

(glsl-defun :name 'pipe
	    :in-args '((a ((:int :uint
			    :ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :match)
		       (b ((:int :uint
			    :ivec2 :ivec3 :ivec4
			    :uvec2 :uvec3 :uvec4) nil) :match))
	    :output-type '(0 nil)
	    :transform "(~a | ~a)")

(glsl-defun :name '&&
	    :in-args '((a (:bool nil))
		       (b (:bool nil)))
	    :output-type '(0 nil)
	    :transform "(~a && ~a)")

(glsl-defun :name '^^
	    :in-args '((a (:bool nil))
		       (b (:bool nil)))
	    :output-type '(0 nil)
	    :transform "(~a && ~a)")

(glsl-defun :name '||
	    :in-args '((a (:bool nil))
		       (b (:bool nil)))
	    :output-type '(0 nil)
	    :transform "(~a && ~a)")



;;------------------------------------------------------------
;; Special Function
;;------------------

(vdefspecial progn (varjo-code)    
  (let ((arg-objs (mapcar #'varjo->glsl varjo-code)))
    (if (eq 1 (length arg-objs))
	(car arg-objs)
	(let ((last-arg (car (last arg-objs)))
	      (args (subseq arg-objs 0 (- (length arg-objs) 1))))
	  (merge-obs arg-objs
	   :type (code-type last-arg)
	   :current-line (current-line last-arg)
	   :to-block (remove-if 
		      #'null
		      (append
		       (mapcan #'(lambda (x) 
				   (list (to-block x) 
					 (format nil "~a;"
					  (current-line x))))
			       args)
		       (list (to-block last-arg)))))))))

(vdefspecial %typify (varjo-code)    
  (let ((arg-objs (mapcar #'varjo->glsl varjo-code)))
    (if (> (length arg-objs) 1)
      (error "Typify cannot take more than one form")	
	(let* ((arg (car arg-objs))
	       (type (code-type arg)))
	  (merge-obs arg :current-line 
		     (format nil "~a ~a" 
			     (varjo-type->glsl-type type)
			     (current-line arg)))))))


(vdefspecial %make-var (varjo-code)  
  (let ((name (first varjo-code))
	(type (second varjo-code)))
    (make-instance 'code 
		   :type (set-place-t type)
		   :current-line (string name))))

(vdefspecial %make-array (varjo-code)  
  (let* ((type (first varjo-code))
	 (literal-length (typep (second varjo-code) 'code))
	 (length (if literal-length
		     (second varjo-code)
		     (varjo->glsl (second varjo-code))))
	 (contents (mapcar #'varjo->glsl (third varjo-code))))
    (merge-obs 
     (cons length contents)
     :type (flesh-out-type 
	    `(,type ,(if literal-length
			 (parse-integer (current-line length))
			 t)))
     :current-line (format nil "~a[~a]{~{~a~^,~}}" 
			   type
			   (current-line length) 
			   (mapcar #'current-line contents)))))

(vdefspecial %init-vec-or-mat (varjo-code)
  (let* ((target-type (flesh-out-type (first varjo-code)))
	 (target-length (type-component-count target-type))
	 (arg-objs (mapcar #'varjo->glsl (rest varjo-code)))
	 (types (mapcar #'code-type arg-objs))
	 (lengths (mapcar #'type-component-count types)))
    (if (eq target-length (apply #'+ lengths))
	(merge-obs arg-objs
		   :type target-type
		   :current-line 
		   (format nil "~a(~{~a~^,~^ ~})"
			   (varjo-type->glsl-type target-type)
			   (mapcar #'current-line arg-objs)))
	(error "The lengths of the types provided~%(~{~a~^,~^ ~})~%do not add up to the length of ~a" types target-type))))

;; check for name clashes between forms
;; create init forms, for each one 

(vdefspecial let (varjo-code)
  (labels ((var-name (form) 
	     (if (listp (first form)) (first (first form))
		 (first form)))
	   (var-type (form) 
	     (when (listp (first form))
	       (flesh-out-type (second (first form)))))
	   (val (form) 
	     (second form))
	   (compile-form (name type value)
	     (varjo->glsl `(%typify (setf (%make-var ,name ,type)
					  ,value)))))
    (let* ((form-code (first varjo-code))
	   (body-code (rest varjo-code))	 
	   (val-objs (loop :for form in form-code
			   :collect (varjo->glsl (val form))))
	   (var-names (mapcar #'var-name form-code))
	   (var-gl-names (mapcar #'glsl-gensym var-names))
	   (var-types (loop :for form :in form-code
			    :for obj :in val-objs
			    :collect (or (var-type form)
					 (code-type obj))))
	   (form-objs (mapcar #'compile-form 
			      var-gl-names var-types val-objs))
	   (*glsl-variables*
	     (append (mapcar #'list 
			     var-names var-types var-gl-names)
		     *glsl-variables*)))
      (let* ((prog-ob (funcall-special 'progn body-code)))
	(merge-obs (cons prog-ob form-objs)
		   :type (code-type prog-ob)
		   :current-line (current-line prog-ob)
		   :to-block (append 
			      (mapcan #'to-block form-objs)
			      (mapcar (lambda (x)
					(format nil "~a;"
						(current-line x))) 
				      form-objs)
				  (to-block prog-ob))
		   :to-top (append 
			    (mapcan #'to-top form-objs)
			    (to-top prog-ob)))))))

(vdefspecial out (varjo-code)
  (let* ((arg-obj (varjo->glsl (second varjo-code)))
	 (out-var-name (first varjo-code))
	 (qualifiers (subseq varjo-code 2)))
    (if (assoc out-var-name *glsl-variables*)
	(error "The variable name '~a' is already taken and so cannot be used~%for an out variable" out-var-name)
	(make-instance 'code
		       :type :void
		       :current-line (format nil "~a = ~a" 
					     out-var-name
					     (current-line arg-obj))
		       :to-block (to-block arg-obj)
		       :to-top (cons (format nil "~{~a ~}out ~a ~a;"
					     qualifiers
					     (varjo-type->glsl-type
					      (code-type arg-obj))
					     out-var-name)
				     (to-top arg-obj))
		       :out-vars (list varjo-code)))))

(vdefspecial + (varjo-code)    
  (let* ((arg-objs (mapcar #'varjo->glsl varjo-code))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(merge-obs arg-objs
		   :type (apply #'superior-type types)
		   :current-line (format nil "(~{~a~^ ~^+~^ ~})"
					 (mapcar #'current-line 
						 arg-objs)))
	(error "The types of object passed to + are not compatible~%~{~s~^ ~}" types))))

(vdefspecial %- (varjo-code)    
  (let* ((arg-objs (mapcar #'varjo->glsl varjo-code))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(merge-obs arg-objs
		   :type (apply #'superior-type types)
		   :current-line (format nil "(~{~a~^ ~^-~^ ~})"
					 (mapcar #'current-line 
						 arg-objs)))
	(error "The types of object passed to - are not compatible~%~{~s~^ ~}" types))))

(vdefspecial / (varjo-code)    
  (let* ((arg-objs (mapcar #'varjo->glsl varjo-code))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(merge-obs arg-objs
		   :type (apply #'superior-type types)
		   :current-line (format nil "(~{~a~^ ~^/~^ ~})"
					 (mapcar #'current-line 
						 arg-objs)))
	(error "The types of object passed to - are not compatible~%~{~s~^ ~}" types))))

(vdefspecial %negate (varjo-code)  
  (if (> (length varjo-code) 1)
      (error "Negate cannot take more than one form")
      (let* ((arg-obj (varjo->glsl (first varjo-code))))
	(merge-obs arg-obj
		   :current-line (format nil "-~a"
					 (current-line arg-obj))))))

(vdefspecial if (varjo-code)  
  (let* ((test (varjo->glsl (first varjo-code)))
	 (t-obj (varjo->glsl (second varjo-code)))
	 (nil-obj (varjo->glsl (third varjo-code)))
	 (arg-objs (remove-if #'null (list test t-obj nil-obj))))
    (if (glsl-typep test '(:bool nil))
	(merge-obs 
	 arg-objs
	 :type :none
	 :current-line ""
	 :to-block 
	 (list (if nil-obj
		   (format nil "~a~%if (~a) {~{~%~a~}~%~a;~%} else {~{~%~a~}~%~a;~%}"
			   (or (to-block test) "") 
			   (current-line test)
			   (or (to-block t-obj) nil) 
			   (current-line t-obj)
			   (or (to-block nil-obj) nil) 
			   (current-line nil-obj))
		   (format nil "~a~%if (~a) {~{~%~a~}~%~a;~%}"
			   (or (to-block test) "") 
			   (current-line test)
			   (or (to-block t-obj) nil)
			   (current-line t-obj)))))
	(error "The result of the test must be a bool.~%~s"
	       (code-type test)))))

(vdefspecial ? (varjo-code)  
  (let* ((test (varjo->glsl (first varjo-code)))
	 (t-obj (varjo->glsl (second varjo-code)))
	 (nil-obj (varjo->glsl (third varjo-code)))
	 (arg-objs (remove-if #'null (list test t-obj nil-obj))))
    (if (glsl-typep test '(:bool nil))
	(if (equal (code-type nil-obj) (code-type t-obj))
	    (merge-obs 
	     arg-objs
	     :type (code-type nil-obj)
	     :current-line (format nil "(~a ? ~a : ~a)"
				   (current-line test)
				   (current-line t-obj)
				   (current-line nil-obj)))
	    (error "Verjo: Both potential outputs must be of the same type"))
	(error "The result of the test must be a bool.~%~a"
	       (code-type test)))))

(vdefspecial switch (varjo-code)  
  (let* ((test (varjo->glsl (first varjo-code)))
	 (clauses (rest varjo-code))
	 (keys (mapcar #'first clauses))
	 (arg-objs (mapcar #'(lambda (x) (varjo->glsl (second x)))
			   clauses))
	 (format-clauses 
	   (loop :for key :in keys
		 :for obj :in arg-objs
		 :append
		 (cond ((eq key 'otherwise) 
			(list "default" nil "jam"))
		       ((glsl-typep key '(:int nil))
			(list (current-line key)
				  (or (to-block obj) nil) 
				  (current-line obj)))))))
    (print format-clauses)
    (if (glsl-typep test '(:int nil))
	(merge-obs 
	 arg-objs
	 :type :none
	 :current-line ""
	 :to-block 
	 (list 
	  (format nil "~a~%switch (~a) {~{~%case ~a:~%~{~a~^~%~}~a;~%break;~}}"
		  (or (to-block test) "") 
		  (current-line test)
		  format-clauses)))
	(error "The result of the test must be an int.~%~s"
	       (code-type test)))))

;;------------------------------------------------------------
;; Lisp Function Substitutions
;;-----------------------------

(vdefmacro - (&rest args)
  (if (eq 1 (length args))
      `(%negate ,@args)
      `(%- ,@args)))

(vdefmacro * (&rest args)
  (oper-segment-list args '*))

(vdefmacro / (&rest args)
  (oper-segment-list args '/))

(vdefmacro v! (&rest args)
  `(%init-vec-or-mat ,(kwd (symb :vec (length args))) ,@args))

(vdefmacro vec2 (&rest args)
  `(%init-vec-or-mat :vec2 ,@args))

(vdefmacro vec3 (&rest args)
  `(%init-vec-or-mat :vec3 ,@args))

(vdefmacro vec4 (&rest args)
  `(%init-vec-or-mat :vec4 ,@args))

(vdefmacro ivec2 (&rest args)
  `(%init-vec-or-mat :ivec2 ,@args))

(vdefmacro ivec3 (&rest args)
  `(%init-vec-or-mat :ivec3 ,@args))

(vdefmacro ivec4 (&rest args)
  `(%init-vec-or-mat :ivec4 ,@args))

(vdefmacro uvec2 (&rest args)
  `(%init-vec-or-mat :uvec2 ,@args))

(vdefmacro uvec3 (&rest args)
  `(%init-vec-or-mat :uvec3 ,@args))

(vdefmacro uvec4 (&rest args)
  `(%init-vec-or-mat :uvec4 ,@args))

(vdefmacro mat2 (&rest args)
  `(%init-vec-or-mat :mat2 ,@args))

(vdefmacro mat3 (&rest args)
  `(%init-vec-or-mat :mat3 ,@args))

(vdefmacro mat4 (&rest args)
  `(%init-vec-or-mat :mat4 ,@args))

(vdefmacro mat2x2 (&rest args)
  `(%init-vec-or-mat :mat2x2 ,@args))

(vdefmacro mat2x3 (&rest args)
  `(%init-vec-or-mat :mat2x3 ,@args))

(vdefmacro mat2x4 (&rest args)
  `(%init-vec-or-mat :mat2x4 ,@args))

(vdefmacro mat3x2 (&rest args)
  `(%init-vec-or-mat :mat3x2 ,@args))

(vdefmacro mat3x3 (&rest args)
  `(%init-vec-or-mat :mat3x3 ,@args))

(vdefmacro mat3x4 (&rest args)
  `(%init-vec-or-mat :mat3x4 ,@args))

(vdefmacro mat4x2 (&rest args)
  `(%init-vec-or-mat :mat4x2 ,@args))

(vdefmacro mat4x3 (&rest args)
  `(%init-vec-or-mat :mat4x3 ,@args))

(vdefmacro mat4x4 (&rest args)
  `(%init-vec-or-mat :mat4x4 ,@args))

