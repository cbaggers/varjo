(in-package :varjo.internals)

;;----------------------------------------------------------------------

(define-v-type-class v-bool (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bool" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-number (v-type) ())

(define-v-type-class v-real (v-number) ())

(define-v-type-class v-rational (v-real) ())

(define-v-type-class v-integer (v-real) ())

(define-v-type-class v-int (v-integer)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "int" :reader v-glsl-string)
   (casts-to :initform '(v-uint v-float v-double))
   (default-value :initform 0)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uint (v-integer)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uint" :reader v-glsl-string)
   (casts-to :initform '(v-float v-double))
   (default-value :initform 0)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-float (v-real)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "float" :reader v-glsl-string)
   (casts-to :initform '(v-double))
   (default-value :initform 0f0)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-short-float (v-real)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "short-float" :reader v-glsl-string)
   (default-value :initform 0s0)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-double (v-real)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "double" :reader v-glsl-string)
   (default-value :initform 0d0)
   (tertiary-score :initform 0.9 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-matrix (v-container) ())

(define-v-type-class v-dmatrix (v-matrix)
  ((tertiary-score :initform 0.9 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat2x2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat2x3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 3) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x3))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat2x4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 4) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x4))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat3x2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 2) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x2))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat3x3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat3x4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 4) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x4))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat4x2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 2) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x2))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat4x3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 3) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x3))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-mat4x4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-dmat2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)))

(define-v-type-class v-dmat3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)))

(define-v-type-class v-dmat4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)))

(define-v-type-class v-dmat2x2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat2x2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)))

(define-v-type-class v-dmat2x3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat2x3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 3) :reader v-dimensions)
   (glsl-size :initform 2)))

(define-v-type-class v-dmat2x4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat2x4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 4) :reader v-dimensions)
   (glsl-size :initform 2)))

(define-v-type-class v-dmat3x2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat3x2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 2) :reader v-dimensions)
   (glsl-size :initform 3)))

(define-v-type-class v-dmat3x3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat3x3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)))

(define-v-type-class v-dmat3x4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat3x4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 4) :reader v-dimensions)
   (glsl-size :initform 3)))

(define-v-type-class v-dmat4x2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat4x2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 2) :reader v-dimensions)
   (glsl-size :initform 4)))

(define-v-type-class v-dmat4x3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat4x3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 3) :reader v-dimensions)
   (glsl-size :initform 4)))

(define-v-type-class v-dmat4x4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dmat4x4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)))

(define-v-type-class v-vector (v-container) ())

(define-v-type-class v-fvector (v-vector) ())

(define-v-type-class v-vec2 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-dvec2))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-vec3 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-dvec3))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-vec4 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-dvec4))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-bvector (v-vector) ())

(define-v-type-class v-bvec2 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec2" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(2) :reader v-dimensions)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-bvec3 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec3" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(3) :reader v-dimensions)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-bvec4 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec4" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(4) :reader v-dimensions)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uvector (v-vector) ())

(define-v-type-class v-uvec2 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec2" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-dvec2 v-vec2))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uvec3 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec3" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-dvec3 v-vec3))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uvec4 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec4" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-dvec4 v-vec4))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-ivector (v-vector) ())

(define-v-type-class v-ivec2 (v-ivector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec2" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-uvec2 v-vec2 v-dvec2))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-ivec3 (v-ivector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec3" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-uvec3 v-vec3 v-dvec3))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-ivec4 (v-ivector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec4" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-uvec4 v-vec4 v-dvec4))
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-dvector (v-vector)
  ((tertiary-score :initform 0.9 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-dvec2 (v-dvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dvec2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-uvec2 v-vec2 v-dvec2))))

(define-v-type-class v-dvec3 (v-dvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dvec3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-uvec3 v-vec3 v-dvec3))))

(define-v-type-class v-dvec4 (v-dvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dvec4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-uvec4 v-vec4 v-dvec4))))

(define-v-type-class v-isampler-1d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler1D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-1d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler1DArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-2d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-2d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-2d-ms (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DMS" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-2d-ms-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-2d-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DRect" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-3d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler3D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-buffer (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerBuffer" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-cube (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerCube" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-cube-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-1d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-1d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1DArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-1d-array-shadow
    (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1DArrayShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-1d-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1DShadow" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-array-shadow
    (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DArrayShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-ms (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DMS" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-ms-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DRect" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-rect-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DRectShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-2d-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DShadow" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-3d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler3D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-buffer (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerBuffer" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-cube (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCube" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-cube-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-cube-array-shadow
    (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCubeArrayShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-cube-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCubeShadow" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-1d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler1D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-1d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler1DArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-2d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-2d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-2d-ms (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DMS" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-2d-ms-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-2d-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-3d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler3D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-buffer (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerBuffer" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-cube (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerCube" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-cube-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isampler-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usampler-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------

(define-v-type-class v-atomic-uint (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "atomic_uint" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------


(define-v-type-class v-image-1d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image1D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-1d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage1D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-1d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage1D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-2d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-2d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-2d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-3d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image3D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-3d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage3D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-3d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage3D" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageRect" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageRect" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageRect" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-2d-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DRect" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-2d-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DRect" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-2d-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DRect" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-cube (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageCube" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-cube (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageCube" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-cube (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageCube" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-buffer (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageBuffer" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-buffer (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageBuffer" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-buffer (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageBuffer" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-1d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image1DArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-1d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage1DArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-1d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage1DArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-2d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-2d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-2d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-cube-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageCubeArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-cube-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageCubeArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-cube-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageCubeArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-2d-ms (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DMS" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-2d-ms (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DMS" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-2d-ms (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DMS" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-image-2d-ms-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DMSArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-iimage-2d-ms-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DMSArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-uimage-2d-ms-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DMSArray" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------
;; Sampler (Vulkan only)

(define-v-type-class v-sampler-vulkan (v-sampler-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-sampler-shadow-vulkan (v-sampler-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerShadow" :reader v-glsl-string)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------
;; Subpass Input (Vulkan only)

(define-v-type-class v-subpass-input (v-subpass-input-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "subpassInput" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-subpass-input-ms (v-subpass-input-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "subpassInputMS" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isubpass-input (v-subpass-input-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isubpassInput" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-isubpass-input-ms (v-subpass-input-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isubpassInputMS" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usubpass-input (v-subpass-input-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usubpassInput" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-usubpass-input-ms (v-subpass-input-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usubpassInputMS" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------
;; Texture (Vulkan only)

(define-v-type-class v-itexture-1d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture1D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-1d-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture1DArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-2d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture2D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-2d-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture2DArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-2d-ms (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture2DMS" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-2d-ms-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-2d-rect (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture2DRect" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-3d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itexture3D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-buffer (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itextureBuffer" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-cube (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itextureCube" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-cube-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itextureCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-1d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture1D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-1d-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture1DArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-2d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture2D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-2d-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture2DArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-2d-ms (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture2DMS" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-2d-ms-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-2d-rect (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture2DRect" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-3d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "texture3D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-buffer (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "textureBuffer" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-cube (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "textureCube" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-cube-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "textureCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-1d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture1D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-1d-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture1DArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-2d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture2D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-2d-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture2DArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-2d-ms (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture2DMS" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-2d-ms-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-2d-rect (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture2DRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-3d (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utexture3D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-buffer (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utextureBuffer" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-cube (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utextureCube" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-cube-array (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utextureCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-texture-rect (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "textureRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-itexture-rect (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "itextureRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

(define-v-type-class v-utexture-rect (v-texture)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "utextureRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------
;; accelerationStructureEXT (Vulkan only)

(define-v-type-class v-acceleration-structure-ext (v-opaque-vulkan-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "accelerationStructureEXT" :reader v-glsl-string)
   (element-type :initform 'v-type)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)))

;;----------------------------------------------------------------------
;; rayQueryEXT (Vulkan only)

(define-v-type-class v-ray-query-ext (v-opaque-vulkan-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "rayQueryEXT" :reader v-glsl-string)
   (element-type :initform 'v-type)
   (tertiary-score :initform 1 :initarg :tertiary-score
                   :reader tertiary-score)
   (allow-unboundp :initform t :reader allow-unboundp)))

;;----------------------------------------------------------------------
;; Type Stubs
;;
;; {TODO} look into these

(define-v-type-class v-buffer-image (v-type) ())

(define-v-type-class v-ibuffer-image (v-type) ())

(define-v-type-class v-ubuffer-image (v-type) ())

;;----------------------------------------------------------------------

(defun vec-of (type len)
  (assert (and (>= len 2) (<= len 4)))
  (type-spec->type
   (etypecase type
     (v-uint (nth (- len 2) '(:uvec2 :uvec3 :uvec4)))
     (v-int (nth (- len 2) '(:ivec2 :ivec3 :ivec4)))
     (v-float (nth (- len 2) '(:vec2 :vec3 :vec4)))
     (v-double (nth (- len 2) '(:dvec2 :dvec3 :dvec4))))))
