;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(glsl-defun :name '%void
            :in-args '()
            :output-type :void
            :transform ""
            :context-restriction nil)

(glsl-defun :name 'x
            :in-args '((vec ((:bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "~a.x"
            :context-restriction '((:330)))

(glsl-defun :name 'x
            :in-args '((vec ((:ivec2 :ivec3 :ivec4))))
            :output-type :int
            :transform "~a.x"
            :context-restriction '((:330)))

(glsl-defun :name 'x
            :in-args '((vec ((:uvec2 :uvec3 :uvec4))))
            :output-type :uint
            :transform "~a.x"
            :context-restriction '((:330)))

(glsl-defun :name 'x
            :in-args '((vec ((:vec2 :vec3 :vec4))))
            :output-type :float
            :transform "~a.x"
            :context-restriction '((:330)))

(glsl-defun :name 'y
            :in-args '((vec ((:bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "~a.y"
            :context-restriction '((:330)))

(glsl-defun :name 'y
            :in-args '((vec ((:ivec2 :ivec3 :ivec4))))
            :output-type :int
            :transform "~a.y"
            :context-restriction '((:330)))

(glsl-defun :name 'y
            :in-args '((vec ((:uvec2 :uvec3 :uvec4))))
            :output-type :uint
            :transform "~a.y"
            :context-restriction '((:330)))

(glsl-defun :name 'y
            :in-args '((vec ((:vec2 :vec3 :vec4))))
            :output-type :float
            :transform "~a.y"
            :context-restriction '((:330)))

(glsl-defun :name 'z
            :in-args '((vec ((:bvec3 :bvec4))))
            :output-type :bool
            :transform "~a.z"
            :context-restriction '((:330)))

(glsl-defun :name 'z
            :in-args '((vec ((:ivec3 :ivec4))))
            :output-type :int
            :transform "~a.z"
            :context-restriction '((:330)))

(glsl-defun :name 'z
            :in-args '((vec ((:uvec3 :uvec4))))
            :output-type :uint
            :transform "~a.z"
            :context-restriction '((:330)))

(glsl-defun :name 'z
            :in-args '((vec ((:vec3 :vec4))))
            :output-type :float
            :transform "~a.z"
            :context-restriction '((:330)))

(glsl-defun :name 'w
            :in-args '((vec :bvec4))
            :output-type :bool
            :transform "~a.w"
            :context-restriction '((:330)))

(glsl-defun :name 'w
            :in-args '((vec :ivec4))
            :output-type :int
            :transform "~a.w"
            :context-restriction '((:330)))

(glsl-defun :name 'w
            :in-args '((vec :uvec4))
            :output-type :uint
            :transform "~a.w"
            :context-restriction '((:330)))

(glsl-defun :name 'w
            :in-args '((vec :vec4))
            :output-type :float
            :transform "~a.w"
            :context-restriction '((:330)))

(glsl-defun :name 'bool
            :in-args '((x ((:double :float :int :uint :bool
                                    :bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "bool(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'double
            :in-args '((x ((:bool :float :int :uint :double))))
            :output-type :double
            :transform "double(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'float
            :in-args '((x ((:bool :double :int :uint :float
                                  :vec2 :vec3 :vec4))))
            :output-type :float
            :transform "float(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'int
            :in-args '((x ((:bool :double :float :uint :int
                                  :ivec2 :ivec3 :ivec4))))
            :output-type :int
            :transform "int(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'uint
            :in-args '((x ((:bool :double :float :int :uint
                                  :uvec2 :uvec3 :uvec4))))
            :output-type :uint
            :transform "uint(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'uint
            :in-args '((x ((:bool :double :float :int :uint
                                  :uvec2 :uvec3 :uvec4))))
            :output-type :uint
            :transform "uint(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'degrees
            :in-args '((radians ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "degrees(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'radians
            :in-args '((degrees ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "radians(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'sin
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "sin(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'cos
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "cos(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'tan
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "tan(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'asin
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "asin(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'acos
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "acos(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'atan
            :in-args '((y ((:float :vec2 :vec3 :vec4)) :compatible)
                       (x ((:float :vec2 :vec3 :vec4)) :compatible))
            :output-type '(0 nil)
            :transform "atan(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'atan
            :in-args '((y-over-x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "atan(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'sinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "sinh(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'cosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "cosh(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'tanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "tanh(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'asinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "asinh(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'acosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "acosh(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'atanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "atanh(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'pow
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
                       (y ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "pow(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'exp
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "exp(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'log
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "log(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'exp2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "exp2(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'log2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "log2(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'sqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "exp(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'inversesqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "inversesqrt(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'abs
            :in-args '((x ((:float :vec2 :vec3 :vec4
                                   :int :ivec2 :ivec3 :ivec4))))
            :output-type '(0 nil)
            :transform "abs(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'sign
            :in-args '((x ((:float :vec2 :vec3 :vec4
                                   :int :ivec2 :ivec3 :ivec4))))
            :output-type '(:float nil)
            :transform "sign(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'floor
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "floor(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'trunc
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "trunc(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'round
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "round(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'round-even
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "roundEven(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'ceil
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(:int nil)
            :transform "ceil(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'fract
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "fract(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'mod
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
                       (y ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "mod(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'min
            :in-args '((x ((:float :vec2 :vec3 :vec4
                                   :int :ivec2 :ivec3 :ivec4
                                   :uint :uvec2 :uvec3 :uvec4)) :match)
                       (y ((:float :vec2 :vec3 :vec4
                                   :int :ivec2 :ivec3 :ivec4
                                   :uint :uvec2 :uvec3 :uvec4)) :match))
            :output-type '(0 nil)
            :transform "min(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'min
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
                       (y :float))
            :output-type '(0 nil)
            :transform "min(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'min
            :in-args '((x ((:int :ivec2 :ivec3 :ivec4)))
                       (y :int))
            :output-type '(0 nil)
            :transform "min(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'min
            :in-args '((x ((:uint :uvec2 :uvec3 :uvec4)))
                       (y :uint))
            :output-type '(0 nil)
            :transform "min(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'max
            :in-args '((x ((:float :vec2 :vec3 :vec4
                                   :int :ivec2 :ivec3 :ivec4
                                   :uint :uvec2 :uvec3 :uvec4)) :match)
                       (y ((:float :vec2 :vec3 :vec4
                                   :int :ivec2 :ivec3 :ivec4
                                   :uint :uvec2 :uvec3 :uvec4)) :match))
            :output-type '(0 nil)
            :transform "max(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'max
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
                       (y :float))
            :output-type '(0 nil)
            :transform "max(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'max
            :in-args '((x ((:int :ivec2 :ivec3 :ivec4)))
                       (y :int))
            :output-type '(0 nil)
            :transform "max(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'max
            :in-args '((x ((:uint :uvec2 :uvec3 :uvec4)))
                       (y :uint))
            :output-type '(0 nil)
            :transform "max(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "clamp(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'clamp
            :in-args '((x ((:float :vec2 :vec3 :vec4)) )
                       (min-val :float )
                       (max-val :float ))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'clamp
            :in-args '((x ((:int :ivec2 :ivec3 :ivec4)) )
                       (min-val :int )
                       (max-val :int ))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'clamp
            :in-args '((x ((:uint :uvec2 :uvec3 :uvec4)))
                       (min-val :uint )
                       (max-val :uint ))
            :output-type '(0 nil)
            :transform "clamp(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'mix
            :in-args '((x ((:float :vec2 :vec3 :vec4)) :match)
                       (y ((:float :vec2 :vec3 :vec4)) :match)
                       (a ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(0 nil)
            :transform "mix(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'mix
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
                       (y ((:float :vec2 :vec3 :vec4)))
                       (a ((:float :bvec2 :bvec3 :bvec4 :bool))))
            :output-type '(0 nil)
            :transform "mix(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'smooth-step
            :in-args '((edge0 ((:float :vec2 :vec3 :vec4)) :match)
                       (edge1 ((:float :vec2 :vec3 :vec4)) :match)
                       (x ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(2 nil)
            :transform "smoothstep(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'smooth-step
            :in-args '((edge0 :float)
                       (edge1 :float)
                       (x ((:float :vec2 :vec3 :vec4))))
            :output-type '(2 nil)
            :transform "smoothstep(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'is-nan
            :in-args '((x :float))
            :output-type '(:bool nil)
            :transform "isnan(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'is-nan
            :in-args '((x :vec2))
            :output-type '(:bvec2 nil)
            :transform "isnan(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'is-nan
            :in-args '((x :vec3))
            :output-type '(:bvec3 nil)
            :transform "isnan(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'is-nan
            :in-args '((x :vec4))
            :output-type '(:bvec4 nil)
            :transform "isnan(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'length
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type :float
            :transform "length(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'distance
            :in-args '((p0 ((:float :vec2 :vec3 :vec4)) :match)
                       (p1 ((:float :vec2 :vec3 :vec4)) :match))
            :output-type :float
            :transform "distance(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'dot
            :in-args '((x ((:float :vec2 :vec3 :vec4)) :match)
                       (y ((:float :vec2 :vec3 :vec4)) :match))
            :output-type :float
            :transform "dot(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'cross
            :in-args '((x :vec3)
                       (y :vec3))
            :output-type :vec3
            :transform "cross(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'normalize
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil)
            :transform "normalize(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'aref
            :in-args '((array (t t))
                       (index ((:uint :int))))
            :output-type '(0 nil t)
            :transform "~a[~a]"
            :context-restriction '((:330)))

(glsl-defun :name 'aref
            :in-args '((vector ((:vec2 :vec3 :vec4)))
                       (index ((:uint :int))))
            :output-type '(:float 0 t)
            :transform "~a[~a]"
            :context-restriction '((:330)))

(glsl-defun :name 'aref
            :in-args '((vector ((:ivec2 :ivec3 :ivec4)))
                       (index ((:uint :int))))
            :output-type '(:int 0 t)
            :transform "~a[~a]"
            :context-restriction '((:330)))

(glsl-defun :name 'aref
            :in-args '((vector ((:uvec2 :uvec3 :uvec4)))
                       (index ((:uint :int))))
            :output-type '(:uint 0 t)
            :transform "~a[~a]"
            :context-restriction '((:330)))

(glsl-defun :name 'setf
            :in-args '((x (t nil t) :match)
                       (y (t nil nil) :match))
            :output-type '(0 0)
            :transform "~a = ~a"
            :context-restriction '((:330)))

(glsl-defun :name 'setf
            :in-args '((x (t t t) :match)
                       (y (t t nil) :match))
            :output-type '(0 0)
            :transform "~a = ~a"
            :context-restriction '((:330)))

(glsl-defun :name 'f-transform
            :in-args '()
            :output-type :vec4
            :transform "ftransform()"
            :context-restriction '((:330)))

(glsl-defun :name 'face-forward
            :in-args '((n ((:float :vec2 :vec3 :vec4)) :match)
                       (i ((:float :vec2 :vec3 :vec4)) :match)
                       (nref ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(0 0)
            :transform "faceforward(~a, ~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'reflect
            :in-args '((i ((:float :vec2 :vec3 :vec4)) :match)
                       (n ((:float :vec2 :vec3 :vec4)) :match))
            :output-type '(0 0)
            :transform "reflect(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'refract
            :in-args '((i ((:float :vec2 :vec3 :vec4)) :match)
                       (n ((:float :vec2 :vec3 :vec4)) :match)
                       (eta :float))
            :output-type '(0 0)
            :transform "reflect(~a, ~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "matrixCompMult(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "outerProduct(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "outerProduct(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'transpose
            :in-args '((m ((:mat2 :mat3 :mat4
                                  :mat2x2 :mat3x3 :mat4x4))))
            :output-type 0
            :transform "transpose(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'transpose
            :in-args '((m ((:mat2x3))))
            :output-type :mat3x2
            :transform "transpose(~a)")
(glsl-defun :name 'transpose
            :in-args '((m ((:mat2x4))))
            :output-type :mat4x2
            :transform "transpose(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'transpose
            :in-args '((m ((:mat3x2))))
            :output-type :mat2x3 
            :transform "transpose(~a)")
(glsl-defun :name 'transpose
            :in-args '((m ((:mat3x4))))
            :output-type :mat4x3
            :transform "transpose(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'transpose
            :in-args '((m ((:mat4x3))))
            :output-type :mat3x4 
            :transform "transpose(~a)")
(glsl-defun :name 'transpose
            :in-args '((m ((:mat4x2))))
            :output-type :mat2x4 
            :transform "transpose(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'determinant
            :in-args '((m ((:mat2 :mat3 :mat4 
                                  :mat2x2 :mat2x3 :mat2x4 
                                  :mat3x2 :mat3x3 :mat3x4 
                                  :mat4x2 :mat4x3 :mat4x4))))
            :output-type :float
            :transform "determinant(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'inverse
            :in-args '((m ((:mat2 :mat3 :mat4))))
            :output-type 0
            :transform "inverse(~a)"
            :context-restriction '((:330)))

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
            :transform "lessThan(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "lessThanEqual(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "greaterThan(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "greaterThanEqual(~a, ~a)"
            :context-restriction '((:330)))

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
            :transform "equal(~a, ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'b-any
            :in-args '((x ((:bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "any(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'b-all
            :in-args '((x ((:bvec2 :bvec3 :bvec4))))
            :output-type :bool
            :transform "all(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'b-not
            :in-args '((x ((:bvec2 :bvec3 :bvec4))))
            :output-type 0
            :transform "not(~a)"
            :context-restriction '((:330)))

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
            :transform "noise1(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'noise-2
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :vec2
            :transform "noise2(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'noise-3
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :vec2
            :transform "noise3(~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'noise-4
            :in-args '((i ((:float :vec2 :vec3 :vec4))))
            :output-type :vec2
            :transform "noise4(~a)"
            :context-restriction '((:330)))

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

;; [TODO] why does this need semicolon?
(glsl-defun :name 'break
            :in-args '()
            :output-type :none
            :transform "break;"
            :context-restriction '((:330)))

(glsl-defun :name 'continue
            :in-args '()
            :output-type :none
            :transform "continue"
            :context-restriction '((:330)))

(glsl-defun :name 'incf
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(~a++)"
            :context-restriction '((:330)))

(glsl-defun :name 'decf
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(~a--)"
            :context-restriction '((:330)))

(glsl-defun :name '++
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(++~a)"
            :context-restriction '((:330)))

(glsl-defun :name '--
            :in-args '((x ((:int :uint :float) nil nil)))
            :output-type 0
            :transform "(--~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:int :float)) :compatible)
                       (y ((:int :float)) :compatible))
            :output-type '(0 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:int :float)))
                       (y ((:vec2 :vec3 :vec4
                                  :ivec2 :ivec3 :ivec4
                                  :mat2 :mat3 :mat4 
                                  :mat2x2 :mat2x3 :mat2x4
                                  :mat3x2 :mat3x3 :mat3x4
                                  :mat4x2 :mat4x3 :mat4x4))))
            :output-type '(1 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:vec2 :vec3 :vec4
                                  :ivec2 :ivec3 :ivec4
                                  :mat2 :mat3 :mat4 
                                  :mat2x2 :mat2x3 :mat2x4
                                  :mat3x2 :mat3x3 :mat3x4
                                  :mat4x2 :mat4x3 :mat4x4)))
                       (y ((:int :float))))
            :output-type '(0 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:vec2 :vec3 :vec4
                                  :ivec2 :ivec3 :ivec4)) :compatible)
                       (y ((:vec2 :vec3 :vec4
                                  :ivec2 :ivec3 :ivec4)) :compatible))
            :output-type '(0 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:mat2 :mat2x2 :mat2x3 :mat2x4)))
                       (y ((:vec2 :ivec2))))
            :output-type '(1 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:mat3 :mat3x2 :mat3x3 :mat3x4)))
                       (y ((:vec3 :ivec3))))
            :output-type '(1 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:mat4 :mat4x2 :mat4x3 :mat4x4)))
                       (y ((:vec4 :ivec4))))
            :output-type '(1 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '*
            :in-args '((x ((:mat2 :mat3 :mat4)) :compatible)
                       (y ((:mat2 :mat3 :mat4)) :compatible))
            :output-type '(1 nil)
            :transform "(~a * ~a)"
            :context-restriction '((:330)))


(glsl-defun :name '%
            :in-args '((x ((:int :uint :ivec2 :uvec2 
                                 :ivec3 :uvec3 :ivec4 :uvec4)))
                       (y ((:int :uint))))
            :output-type '(0 nil)
            :transform "(~a % ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '<
            :in-args '((x ((:float :int)))
                       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a < ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '>
            :in-args '((x ((:float :int)))
                       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a > ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '<=
            :in-args '((x ((:float :int)))
                       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a <= ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '>=
            :in-args '((x ((:float :int)))
                       (y ((:float :int))))
            :output-type '(:bool nil)
            :transform "(~a >= ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '==
            :in-args '((a (t t) :compatible)
                       (b (t t) :compatible))
            :output-type '(:bool nil)
            :transform "(~a == ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '!=
            :in-args '((a (t t) :compatible)
                       (b (t t) :compatible))
            :output-type '(:bool nil)
            :transform "(~a != ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '==
            :in-args '((a (t nil) :compatible)
                       (b (t nil) :compatible))
            :output-type '(:bool nil)
            :transform "(~a == ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '!=
            :in-args '((a (t nil) :compatible)
                       (b (t nil) :compatible))
            :output-type '(:bool nil)
            :transform "(~a != ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '!
            :in-args '((a (:bool nil)))
            :output-type '(:bool nil)
            :transform "(! ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '~
            :in-args '((a ((:int :uint :ivec2 :ivec3 :ivec4) 
                           nil)))
            :output-type '(0 nil)
            :transform "(~ ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '<<
            :in-args '((a ((:int :uint :float) nil))
                       (b ((:int :uint :float) nil)))
            :output-type '(0 nil)
            :transform "(~a << ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '<<
            :in-args '((a ((:ivec2 :ivec3 :ivec4
                                   :uvec2 :uvec3 :uvec4) nil))
                       (b ((:int :uint :float) nil)))
            :output-type '(0 nil)
            :transform "(~a << ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '<<
            :in-args '((a ((:ivec2 :ivec3 :ivec4
                                   :uvec2 :uvec3 :uvec4) nil) :compatible)
                       (b ((:ivec2 :ivec3 :ivec4
                                   :uvec2 :uvec3 :uvec4) nil) :compatible))
            :output-type '(0 nil)
            :transform "(~a << ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '>>
            :in-args '((a ((:int :uint :float) nil))
                       (b ((:int :uint :float) nil)))
            :output-type '(0 nil)
            :transform "(~a >> ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '>>
            :in-args '((a ((:ivec2 :ivec3 :ivec4
                                   :uvec2 :uvec3 :uvec4) nil))
                       (b ((:int :uint :float) nil)))
            :output-type '(0 nil)
            :transform "(~a >> ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '>>
            :in-args '((a ((:ivec2 :ivec3 :ivec4
                                   :uvec2 :uvec3 :uvec4) nil) :compatible)
                       (b ((:ivec2 :ivec3 :ivec4
                                   :uvec2 :uvec3 :uvec4) nil) :compatible))
            :output-type '(0 nil)
            :transform "(~a >> ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '&
            :in-args '((a ((:int :uint
                                 :ivec2 :ivec3 :ivec4
                                 :uvec2 :uvec3 :uvec4) nil) :match)
                       (b ((:int :uint
                                 :ivec2 :ivec3 :ivec4
                                 :uvec2 :uvec3 :uvec4) nil) :match))
            :output-type '(0 nil)
            :transform "(~a & ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '^
            :in-args '((a ((:int :uint
                                 :ivec2 :ivec3 :ivec4
                                 :uvec2 :uvec3 :uvec4) nil) :match)
                       (b ((:int :uint
                                 :ivec2 :ivec3 :ivec4
                                 :uvec2 :uvec3 :uvec4) nil) :match))
            :output-type '(0 nil)
            :transform "(~a ^ ~a)"
            :context-restriction '((:330)))

(glsl-defun :name 'pipe
            :in-args '((a ((:int :uint
                                 :ivec2 :ivec3 :ivec4
                                 :uvec2 :uvec3 :uvec4) nil) :match)
                       (b ((:int :uint
                                 :ivec2 :ivec3 :ivec4
                                 :uvec2 :uvec3 :uvec4) nil) :match))
            :output-type '(0 nil)
            :transform "(~a | ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '&&
            :in-args '((a (:bool nil))
                       (b (:bool nil)))
            :output-type '(0 nil)
            :transform "(~a && ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '^^
            :in-args '((a (:bool nil))
                       (b (:bool nil)))
            :output-type '(0 nil)
            :transform "(~a && ~a)"
            :context-restriction '((:330)))

(glsl-defun :name '||
            :in-args '((a (:bool nil))
                       (b (:bool nil)))
            :output-type '(0 nil)
            :transform "(~a && ~a)"
            :context-restriction '((:330)))

