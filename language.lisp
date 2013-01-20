;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

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
  (not (null (find symbol *glsl-special-functions*))))

(defun apply-special (symbol arg-objs)
  (if (special-functionp symbol)
      (apply (symbol-function (symb symbol '%special)) arg-objs)
      (error "Varjo: '~a' is not a special function" symbol)))

(defun register-special-function (symbol)
  (setf *glsl-special-functions* 
	(cons symbol *glsl-special-functions*)))

(defmacro vdefspecial (name args &body body)
  (let ((new-name (symb name '%special)))
    `(progn 
       (defun ,new-name ,args ,@body)
       (register-special-function ',name))))

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

(defun varjo-type->glsl-type (type-spec)
  (let* ((type (flesh-out-type type-spec))
	 (principle (first type))
	 (len (second type)))
    (if len
	(format nil "~a[~a]" principle (if (numberp len) len ""))
	(format nil "~a" principle))))

(defun instance-var (symbol)
  
  (let ((var-spec (assoc symbol *glsl-variables*)))<
    (make-instance 'code
		   :type (set-place-t
			  (flesh-out-type (var-type var-spec)))
		   :current-line (format nil "~a" 
					 (or (var-gl-name var-spec)
					     (var-name var-spec)))
		   :read-only (var-read-only var-spec))))

;;------------------------------------------------------------
;; Built-in Structs
;;------------------

(vdefstruct gl-per-vertex-v (:slot-prefix per-vertex
			     :context-restriction (:vertex))
  (position :vec4 "gl_Position")
  (point-size :float "gl_PointSize")
  (clip-distance (:float t) "gl_ClipDistance")
  (clip-vertex :vec4 "gl_ClipVertex"))

(vdefstruct gl-per-vertex-g (:slot-prefix per-vertex
			     :context-restriction (:fragment))
  (position :vec4 "gl_Position")
  (point-size :float "gl_PointSize")
  (clip-distance (:float t) "gl_ClipDistance"))

;;------------------------------------------------------------
;; Core Language Definitions
;;---------------------------

(glsl-defun :name '%void
            :in-args '()
            :output-type :void
            :transform "")

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

(glsl-defun :name 'reflect
            :in-args '((i ((:float :vec2 :vec3 :vec4)) :match)
		       (n ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(0 0)
            :transform "reflect(~a, ~a)")

(glsl-defun :name 'refract
            :in-args '((i ((:float :vec2 :vec3 :vec4)) :match)
		       (n ((:float :vec2 :vec3 :vec4)) :match)
		       (eta :float))
            :output-type '(0 0)
            :transform "reflect(~a, ~a, ~a)")

(glsl-defun :name 'matrix-comp-mult
            :in-args '((i ((:mat2 :mat3 :mat4 
			    :mat2x2 :mat2x3 :mat2x4 
			    :mat3x2 :mat3x3 :mat3x4 
			    :mat4x2 :mat4x3 :mat4x4)) :compatible)
		       (n ((:mat2 :mat3 :mat4 
			    :mat2x2 :mat2x3 :mat2x4 
			    :mat3x2 :mat3x3 :mat3x4 
			    :mat4x2 :mat4x3 :mat4x4)) :compatible)
		       (eta :float))
            :output-type '(0 0)
            :transform "matrixCompMult(~a, ~a)")

(glsl-defun :name 'outer-product
            :in-args '((i :vec2)
		       (n :vec2))
            :output-type :mat2
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((i :vec3)
		       (n :vec3))
            :output-type :mat3
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((i :vec4)
		       (n :vec4))
            :output-type :mat4
            :transform "outerProduct(~a, ~a)")

(glsl-defun :name 'outer-product
            :in-args '((m :vec2)
		       (n :vec3))
            :output-type :mat3x2
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((m :vec2)
		       (n :vec4))
            :output-type :mat4x2
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((m :vec3)
		       (n :vec2))
            :output-type :mat2x3
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((m :vec3)
		       (n :vec4))
            :output-type :mat4x3
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((m :vec4)
		       (n :vec2))
            :output-type :mat2x4
            :transform "outerProduct(~a, ~a)")
(glsl-defun :name 'outer-product
            :in-args '((m :vec4)
		       (n :vec3))
            :output-type :mat4x3
            :transform "outerProduct(~a, ~a)")

(glsl-defun :name 'transpose
            :in-args '((m ((:mat2 :mat3 :mat4
			    :mat2x2 :mat3x3 :mat4x4))))
            :output-type 0
            :transform "transpose(~a)")

(glsl-defun :name 'transpose
            :in-args '((m ((:mat2x3))))
            :output-type :mat3x2
            :transform "transpose(~a)")
(glsl-defun :name 'transpose
            :in-args '((m ((:mat2x4))))
            :output-type :mat4x2
            :transform "transpose(~a)")

(glsl-defun :name 'transpose
            :in-args '((m ((:mat3x2))))
            :output-type :mat2x3 
            :transform "transpose(~a)")
(glsl-defun :name 'transpose
            :in-args '((m ((:mat3x4))))
            :output-type :mat4x3
            :transform "transpose(~a)")

(glsl-defun :name 'transpose
            :in-args '((m ((:mat4x3))))
            :output-type :mat3x4 
            :transform "transpose(~a)")
(glsl-defun :name 'transpose
            :in-args '((m ((:mat4x2))))
            :output-type :mat2x4 
            :transform "transpose(~a)")

(glsl-defun :name 'determinant
            :in-args '((m ((:mat2 :mat3 :mat4 
			    :mat2x2 :mat2x3 :mat2x4 
			    :mat3x2 :mat3x3 :mat3x4 
			    :mat4x2 :mat4x3 :mat4x4))))
            :output-type :float
            :transform "determinant(~a)")

(glsl-defun :name 'inverse
            :in-args '((m ((:mat2 :mat3 :mat4))))
            :output-type 0
            :transform "inverse(~a)")

(glsl-defun :name 'less-than
	    :in-args '((x ((:vec2 :ivec2 :uvec2)) :compatible)
		       (y ((:vec2 :ivec2 :uvec2)) :compatible))
	    :output-type :bvec2
	    :transform "lessThan(~a, ~a)")
(glsl-defun :name 'less-than
	    :in-args '((x ((:vec3 :ivec3 :uvec3)) :compatible)
		       (y ((:vec3 :ivec3 :uvec3)) :compatible))
	    :output-type :bvec3
	    :transform "lessThan(~a, ~a)")
(glsl-defun :name 'less-than
	    :in-args '((x ((:vec4 :ivec4 :uvec4)) :compatible)
		       (y ((:vec4 :ivec4 :uvec4)) :compatible))
	    :output-type :bvec4
	    :transform "lessThan(~a, ~a)")

(glsl-defun :name 'less-than-equal
	    :in-args '((x ((:vec2 :ivec2 :uvec2)) :compatible)
		       (y ((:vec2 :ivec2 :uvec2)) :compatible))
	    :output-type :bvec2
	    :transform "lessThanEqual(~a, ~a)")
(glsl-defun :name 'less-than-equal
	    :in-args '((x ((:vec3 :ivec3 :uvec3)) :compatible)
		       (y ((:vec3 :ivec3 :uvec3)) :compatible))
	    :output-type :bvec3
	    :transform "lessThanEqual(~a, ~a)")
(glsl-defun :name 'less-than-equal
	    :in-args '((x ((:vec4 :ivec4 :uvec4)) :compatible)
		       (y ((:vec4 :ivec4 :uvec4)) :compatible))
	    :output-type :bvec4
	    :transform "lessThanEqual(~a, ~a)")

(glsl-defun :name 'greater-than
	    :in-args '((x ((:vec2 :ivec2 :uvec2)) :compatible)
		       (y ((:vec2 :ivec2 :uvec2)) :compatible))
	    :output-type :bvec2
	    :transform "greaterThan(~a, ~a)")
(glsl-defun :name 'greater-than
	    :in-args '((x ((:vec3 :ivec3 :uvec3)) :compatible)
		       (y ((:vec3 :ivec3 :uvec3)) :compatible))
	    :output-type :bvec3
	    :transform "greaterThan(~a, ~a)")
(glsl-defun :name 'greater-than
	    :in-args '((x ((:vec4 :ivec4 :uvec4)) :compatible)
		       (y ((:vec4 :ivec4 :uvec4)) :compatible))
	    :output-type :bvec4
	    :transform "greaterThan(~a, ~a)")

(glsl-defun :name 'greater-than-equal
	    :in-args '((x ((:vec2 :ivec2 :uvec2)) :compatible)
		       (y ((:vec2 :ivec2 :uvec2)) :compatible))
	    :output-type :bvec2
	    :transform "greaterThanEqual(~a, ~a)")
(glsl-defun :name 'greater-than-equal
	    :in-args '((x ((:vec3 :ivec3 :uvec3)) :compatible)
		       (y ((:vec3 :ivec3 :uvec3)) :compatible))
	    :output-type :bvec3
	    :transform "greaterThanEqual(~a, ~a)")
(glsl-defun :name 'greater-than-equal
	    :in-args '((x ((:vec4 :ivec4 :uvec4)) :compatible)
		       (y ((:vec4 :ivec4 :uvec4)) :compatible))
	    :output-type :bvec4
	    :transform "greaterThanEqual(~a, ~a)")

(glsl-defun :name 'equal
	    :in-args '((x ((:vec2 :ivec2 :uvec2)) :compatible)
		       (y ((:vec2 :ivec2 :uvec2)) :compatible))
	    :output-type :bvec2
	    :transform "equal(~a, ~a)")
(glsl-defun :name 'equal
	    :in-args '((x ((:vec3 :ivec3 :uvec3)) :compatible)
		       (y ((:vec3 :ivec3 :uvec3)) :compatible))
	    :output-type :bvec3
	    :transform "equal(~a, ~a)")
(glsl-defun :name 'equal
	    :in-args '((x ((:vec4 :ivec4 :uvec4)) :compatible)
		       (y ((:vec4 :ivec4 :uvec4)) :compatible))
	    :output-type :bvec4
	    :transform "equal(~a, ~a)")
(glsl-defun :name 'equal
	    :in-args '((x ((:bvec2 :bvec3 :bvec4)) :match)
		       (y ((:bvec2 :bvec3 :bvec4)) :match))
	    :output-type 0
	    :transform "equal(~a, ~a)")

(glsl-defun :name 'b-any
	    :in-args '((x ((:bvec2 :bvec3 :bvec4))))
	    :output-type :bool
	    :transform "any(~a)")

(glsl-defun :name 'b-all
	    :in-args '((x ((:bvec2 :bvec3 :bvec4))))
	    :output-type :bool
	    :transform "all(~a)")

(glsl-defun :name 'b-not
	    :in-args '((x ((:bvec2 :bvec3 :bvec4))))
	    :output-type 0
	    :transform "not(~a)")

(glsl-defun :name 'dfdx
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type 0
            :transform "dFdx(~a)"
	    :context-restriction '(:fragment))

(glsl-defun :name 'dfdy
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type 0
            :transform "dFdy(~a)"
	    :context-restriction '(:fragment))

(glsl-defun :name 'f-width
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type 0
            :transform "fwidth(~a)"
	    :context-restriction '(:fragment))

(glsl-defun :name 'noise-1
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :float
            :transform "noise1(~a)")

(glsl-defun :name 'noise-2
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :vec2
            :transform "noise2(~a)")

(glsl-defun :name 'noise-3
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :vec2
            :transform "noise3(~a)")

(glsl-defun :name 'noise-4
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :vec2
            :transform "noise4(~a)")

(glsl-defun :name 'emit-vertex
            :in-args nil
            :output-type :void
            :transform "EmitVertex()"
	    :context-restriction '(:geometry))

(glsl-defun :name 'end-primitive
            :in-args nil
            :output-type :void
            :transform "EndPrimitive()"
	    :context-restriction '(:geometry))

(glsl-defun :name 'discard
            :in-args '()
            :output-type :none
            :transform "discard()"
	    :context-restriction '(:fragment))

(glsl-defun :name 'break
            :in-args '()
            :output-type :none
            :transform "break")

(glsl-defun :name 'continue
            :in-args '()
            :output-type :none
            :transform "continue")

(glsl-defun :name 'incf
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(~a++)")

(glsl-defun :name 'decf
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(~a--)")

(glsl-defun :name '++
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(++~a)")

(glsl-defun :name '--
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(--~a)")

(glsl-defun :name '*
            :in-args '((x ((:int :float)) :compatible)
		       (y ((:int :float)) :compatible))
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
            :in-args '((x ((:mat2 :mat2x2 :mat2x3 :mat2x4)))
		       (y ((:vec2 :ivec2))))
            :output-type '(1 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:mat3 :mat3x2 :mat3x3 :mat3x4)))
		       (y ((:vec3 :ivec3))))
            :output-type '(1 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:mat4 :mat4x2 :mat4x3 :mat4x4)))
		       (y ((:vec4 :ivec4))))
            :output-type '(1 nil)
            :transform "(~a * ~a)")

(glsl-defun :name '*
            :in-args '((x ((:mat2 :mat3 :mat4)) :compatible)
		       (y ((:mat2 :mat3 :mat4)) :compatible))
            :output-type '(1 nil)
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

(defgeneric indent (input))

(defmethod indent ((input string))
  (mapcar #'(lambda (x) (format nil "    ~a" x))
	  (split-sequence:split-sequence #\newline input)))

(defmethod indent ((input list))
  (mapcan #'indent input))

;; (vdefspecial %indent (form)
;;   )

(vdefspecial progn (&rest body)
  (let ((arg-objs (mapcar #'varjo->glsl body)))
    (if (eq 1 (length arg-objs))
	(let ((ob (first arg-objs)))
	  (merge-obs ob :current-line 
		     (first (indent (current-line ob)))))
	(let ((last-arg (car (last arg-objs)))
	      (args (subseq arg-objs 0 (- (length arg-objs) 1))))
	  (merge-obs arg-objs
	   :type (code-type last-arg)
	   :current-line (current-line last-arg)
	   :to-block 
	   (indent (list
		    (mapcar #'to-block args)
		    (mapcar #'(lambda (x) (format nil "~@[~a;~]"
						  (current-line x)))
			    args)
		    (to-block last-arg))))))))

(vdefspecial %typify (form)
  (let* ((arg (varjo->glsl form))
	 (type (code-type arg)))
    (merge-obs arg :current-line 
	       (format nil "~a ~a" (varjo-type->glsl-type type)
		       (current-line arg)))))

(vdefspecial %in-typify (form)
  (let* ((arg (varjo->glsl form))
	 (type (code-type arg)))
    (merge-obs arg :current-line 
	       (format nil "~a ~a~@[[~a]~]" 
		       (varjo-type->glsl-type (first type))
		       (current-line arg)
		       (when (second type)
			 (if (numberp (second type))
			     (second type)
			     ""))))))

(vdefspecial %%typify (form)
  (let* ((arg (varjo->glsl form))
	 (type (code-type arg)))
    (merge-obs arg :current-line 
	       (format nil "<~a ~a>" type (current-line arg)))))

(vdefspecial %make-var (name type)
  (make-instance 'code :type (set-place-t type)
		       :current-line (string name)))

(vdefspecial %make-array (type length &optional contents)
  (let* ((literal-length (typep length 'code))
	 (length (varjo->glsl length))
	 (contents (mapcar #'varjo->glsl contents)))
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

(vdefspecial %init-vec-or-mat (type &rest args)
  (let* ((target-type (flesh-out-type type))
	 (target-length (type-component-count target-type))
	 (arg-objs (mapcar #'varjo->glsl args))
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

(vdefspecial let (form-code &rest body-code)
  (destructuring-bind (form-objs new-vars)
      (compile-let-forms form-code)
    (let* ((*glsl-variables* (append new-vars *glsl-variables*))
	   (prog-ob (apply-special 'progn body-code)))
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
			  (to-top prog-ob))))))

(vdefspecial return (&optional (form '(%void)))
  (let ((ob (varjo->glsl form)))
    (merge-obs ob :current-line (format nil "return ~a" 
					(current-line ob))
		  :returns (list (code-type ob)))))

(vdefspecial out (out-var-name form &rest qualifiers)
  (let ((arg-obj (varjo->glsl form)))
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
		       :out-vars `(out-var-name form ,@qualifiers)))))

(vdefspecial + (&rest args)    
  (let* ((arg-objs (mapcar #'varjo->glsl args))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(merge-obs arg-objs
		   :type (apply #'superior-type types)
		   :current-line (format nil "(~{~a~^ ~^+~^ ~})"
					 (mapcar #'current-line 
						 arg-objs)))
	(error "The types of object passed to + are not compatible~%~{~s~^ ~}" types))))

(vdefspecial %- (&rest args)    
  (let* ((arg-objs (mapcar #'varjo->glsl args))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(merge-obs arg-objs
		   :type (apply #'superior-type types)
		   :current-line (format nil "(~{~a~^ ~^-~^ ~})"
					 (mapcar #'current-line 
						 arg-objs)))
	(error "The types of object passed to - are not compatible~%~{~s~^ ~}" types))))

(vdefspecial / (&rest args)    
  (let* ((arg-objs (mapcar #'varjo->glsl args))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(merge-obs arg-objs
		   :type (apply #'superior-type types)
		   :current-line (format nil "(~{~a~^ ~^/~^ ~})"
					 (mapcar #'current-line 
						 arg-objs)))
	(error "The types of object passed to - are not compatible~%~{~s~^ ~}" types))))

(vdefspecial for (var-form condition update &rest body)
  (if 
   (consp (first var-form))
   (error "for can only iterate over one variable")
   (destructuring-bind (form-objs new-vars)
       (compile-let-forms (list var-form) nil)
     (let* ((form-obj (first form-objs))
	    (*glsl-variables* (append new-vars *glsl-variables*))
	    (con-ob (varjo->glsl condition))
	    (up-ob (varjo->glsl update))
	    (prog-ob (apply-special 'progn body)))
       (if (and (null (to-block con-ob)) (null (to-block up-ob)))
	   (merge-obs (list prog-ob form-obj)
		      :type :none
		      :current-line nil
		      :to-block 
		      (list
		       (format nil "~{~a~%~}for (~a;~a;~a) {~%~{~a~%~}~a;~%}"
			       (to-block form-obj)
			       (current-line form-obj)
			       (current-line con-ob)
			       (current-line up-ob)
			       (to-block prog-ob)
			       (current-line prog-ob))))
	   (error "Varjo: Only simple expressions are allowed in the condition and update slots of a for loop"))))))

(vdefspecial labels (func-specs &rest body)
  (let* ((*glsl-variables* (append (assocr :core *built-in-vars*)
				   (assocr *shader-type*
					   *built-in-vars*)))
	 (func-objs (mapcar 
		     #'(lambda (f) (varjo->glsl 
				    (cons '%make-function f)))
		     func-specs))
	 (*glsl-functions* 
	   (append 
	    (loop for spec in func-specs
		  for obj in func-objs
		  :collect 
		  (list
		   (first spec)
		   (vlambda :in-args (second spec)
			    :output-type (code-type obj)
			    :transform 
			    (format nil"~a(~{~a~^,~^ ~})"
				    (first spec)
				    (loop for i below (length (second spec))
					  :collect "~a")))))
	    *glsl-functions*)))
    (let ((prog-obj (apply-special 'progn body)))
      (merge-obs (append func-objs (list prog-obj))
		 :type (code-type prog-obj)
		 :current-line (current-line prog-obj)))))

(vdefspecial %make-function (name args 
			     &rest body)
  (destructuring-bind (form-objs new-vars)
      (compile-let-forms (mapcar #'list args) nil)
    (declare (ignore form-objs))
    (let* ((*glsl-variables* (append new-vars *glsl-variables*)) 
	   (body-obj (apply-special 'progn body))
	   (name (if (eq name :main) :main (glsl-gensym name)))
	   (returns (returns body-obj))
	   (type (if (eq name :main) '(:void nil nil) 
		     (code-type body-obj))))
      (if (or (not returns) (every (equalp! type) returns)) 
	  (make-instance 
	   'code :type type
		 :current-line nil
		 :to-top (list (format nil "~a ~a(~{~{~a ~a~}~^,~^ ~}) {~%~{~a~%~}~@[~a;~%~]}~%"		       
				       (varjo-type->glsl-type type)
				       name 
				       (mapcar #'reverse args)
				       (to-block body-obj) 
				       (current-line body-obj))))
	  (error "Some of the return statements in function '~a' returns different types~%~a~%~a" name type returns)))))

(vdefspecial while (test &rest body)
  (let* ((test-ob (varjo->glsl test))
	 (prog-ob (apply-special 'progn body)))
    (merge-obs (list prog-ob test-ob)
	       :type :none
	       :current-line nil
	       :to-block 
	       (list
		(format nil "~{~a~%~}while (~a) {~%~{~a~%~}~a;~%}"
			(to-block test-ob)
			(current-line test-ob)
			(to-block prog-ob)
			(current-line prog-ob))))))

(vdefspecial %negate (form)  
  (let* ((arg-obj (varjo->glsl form)))
    (merge-obs arg-obj
	       :current-line (format nil "-~a"
				     (current-line arg-obj)))))

(vdefspecial if (test-form then-form &optional else-form)  
  (let* ((test (varjo->glsl test-form))
	 (t-obj (varjo->glsl then-form))
	 (nil-obj (varjo->glsl else-form))
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

(vdefspecial ? (test-form then-form &optional else-form)
  (let* ((test (varjo->glsl test-form))
	 (t-obj (varjo->glsl then-form))
	 (nil-obj (varjo->glsl else-form))
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

(vdefspecial switch (test-form &rest clauses)    
  (let* ((test (varjo->glsl test-form))
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

(vdefmacro while (test &rest body)
  `(while ,test (progn ,@body)))
