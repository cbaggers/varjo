(in-package :varjo.internals)

;;----------------------------------------------------------------------

(v-deftype-internal v-bool (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bool" :reader v-glsl-string)))

(v-deftype-internal v-number (v-type) ())

(v-deftype-internal v-int (v-number)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "int" :reader v-glsl-string)
   (casts-to :initform '(v-uint v-float v-double))
   (default-value :initform 0)))

(v-deftype-internal v-uint (v-number)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uint" :reader v-glsl-string)
   (casts-to :initform '(v-float v-double))
   (default-value :initform 0)))

(v-deftype-internal v-float (v-number)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "float" :reader v-glsl-string)
   (casts-to :initform '(v-double))
   (default-value :initform 0f0)))

(v-deftype-internal v-short-float (v-number)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "short-float" :reader v-glsl-string)
   (default-value :initform 0s0)))

(v-deftype-internal v-double (v-number)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "double" :reader v-glsl-string)
   (default-value :initform 0d0)))

(v-deftype-internal v-matrix (v-container) ())

(v-deftype-internal v-dmatrix (v-matrix) ())

(v-deftype-internal v-mat2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2))))

(v-deftype-internal v-mat3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3))))

(v-deftype-internal v-mat4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4))))

(v-deftype-internal v-mat2x2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)))

(v-deftype-internal v-mat2x3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 3) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x3))))

(v-deftype-internal v-mat2x4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 4) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x4))))

(v-deftype-internal v-mat3x2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 2) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x2))))

(v-deftype-internal v-mat3x3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)))

(v-deftype-internal v-mat3x4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 4) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x4))))

(v-deftype-internal v-mat4x2 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 2) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x2))))

(v-deftype-internal v-mat4x3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 3) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x3))))

(v-deftype-internal v-mat4x4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)))

(v-deftype-internal v-dmat2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2))))

(v-deftype-internal v-dmat3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3))))

(v-deftype-internal v-dmat4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4))))

(v-deftype-internal v-dmat2x2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)))

(v-deftype-internal v-dmat2x3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 3) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x3))))

(v-deftype-internal v-dmat2x4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2x4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 4) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x4))))

(v-deftype-internal v-dmat3x2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 2) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x2))))

(v-deftype-internal v-dmat3x3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)))

(v-deftype-internal v-dmat3x4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3x4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 4) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x4))))

(v-deftype-internal v-dmat4x2 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 2) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x2))))

(v-deftype-internal v-dmat4x3 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 3) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x3))))

(v-deftype-internal v-dmat4x4 (v-dmatrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4x4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)))

(v-deftype-internal v-vector (v-container) ())

(v-deftype-internal v-fvector (v-vector) ())

(v-deftype-internal v-vec2 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-dvec2))))

(v-deftype-internal v-vec3 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-dvec3))))

(v-deftype-internal v-vec4 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-dvec4))))

(v-deftype-internal v-bvector (v-vector) ())

(v-deftype-internal v-bvec2 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec2" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(2) :reader v-dimensions)))

(v-deftype-internal v-bvec3 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec3" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(3) :reader v-dimensions)))

(v-deftype-internal v-bvec4 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec4" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(4) :reader v-dimensions)))

(v-deftype-internal v-uvector (v-vector) ())

(v-deftype-internal v-uvec2 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec2" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-dvec2 v-vec2))))

(v-deftype-internal v-uvec3 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec3" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-dvec3 v-vec3))))

(v-deftype-internal v-uvec4 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec4" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-dvec4 v-vec4))))

(v-deftype-internal v-ivector (v-vector) ())

(v-deftype-internal v-ivec2 (v-ivector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec2" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-uvec2 v-vec2 v-dvec2))))

(v-deftype-internal v-ivec3 (v-ivector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec3" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-uvec3 v-vec3 v-dvec3))))

(v-deftype-internal v-ivec4 (v-ivector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec4" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-uvec4 v-vec4 v-dvec4))))

(v-deftype-internal v-dvector (v-vector) ())

(v-deftype-internal v-dvec2 (v-dvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dvec2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-uvec2 v-vec2 v-dvec2))))

(v-deftype-internal v-dvec3 (v-dvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dvec3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-uvec3 v-vec3 v-dvec3))))

(v-deftype-internal v-dvec4 (v-dvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "dvec4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-uvec4 v-vec4 v-dvec4))))

(v-deftype-internal v-isampler-1d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler1D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-1d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler1DArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-2d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-2d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-2d-ms (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DMS" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-2d-ms-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-2d-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler2DRect" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-3d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isampler3D" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-buffer (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerBuffer" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-cube (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerCube" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-isampler-cube-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-ivec4)))

(v-deftype-internal v-sampler-1d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-1d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1DArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-1d-array-shadow
    (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1DArrayShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-1d-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler1DShadow" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-array-shadow
    (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DArrayShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-ms (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DMS" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-ms-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DRect" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-rect-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DRectShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-2d-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler2DShadow" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-3d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "sampler3D" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-buffer (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerBuffer" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-cube (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCube" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-cube-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-cube-array-shadow
    (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCubeArrayShadow" :reader
                glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-sampler-cube-shadow (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerCubeShadow" :reader v-glsl-string)
   (element-type :initform 'v-vec4)))

(v-deftype-internal v-usampler-1d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler1D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-1d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler1DArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-2d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-2d-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-2d-ms (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DMS" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-2d-ms-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-2d-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler2DRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-3d (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usampler3D" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-buffer (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerBuffer" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-cube (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerCube" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-cube-array (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-sampler-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "samplerRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-isampler-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "isamplerRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

(v-deftype-internal v-usampler-rect (v-sampler)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "usamplerRect" :reader v-glsl-string)
   (element-type :initform 'v-uvec4)))

;;----------------------------------------------------------------------

(v-deftype-internal v-atomic-uint (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "atomic_uint" :reader v-glsl-string)))

;;----------------------------------------------------------------------


(v-deftype-internal v-image-1d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image1D" :reader v-glsl-string)))

(v-deftype-internal v-iimage-1d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage1D" :reader v-glsl-string)))

(v-deftype-internal v-uimage-1d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage1D" :reader v-glsl-string)))

(v-deftype-internal v-image-2d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2D" :reader v-glsl-string)))

(v-deftype-internal v-iimage-2d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2D" :reader v-glsl-string)))

(v-deftype-internal v-uimage-2d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2D" :reader v-glsl-string)))

(v-deftype-internal v-image-3d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image3D" :reader v-glsl-string)))

(v-deftype-internal v-iimage-3d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage3D" :reader v-glsl-string)))

(v-deftype-internal v-uimage-3d (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage3D" :reader v-glsl-string)))

(v-deftype-internal v-image-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageRect" :reader v-glsl-string)))

(v-deftype-internal v-iimage-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageRect" :reader v-glsl-string)))

(v-deftype-internal v-uimage-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageRect" :reader v-glsl-string)))

(v-deftype-internal v-image-2d-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DRect" :reader v-glsl-string)))

(v-deftype-internal v-iimage-2d-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DRect" :reader v-glsl-string)))

(v-deftype-internal v-uimage-2d-rect (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DRect" :reader v-glsl-string)))

(v-deftype-internal v-image-cube (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageCube" :reader v-glsl-string)))

(v-deftype-internal v-iimage-cube (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageCube" :reader v-glsl-string)))

(v-deftype-internal v-uimage-cube (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageCube" :reader v-glsl-string)))

(v-deftype-internal v-image-buffer (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageBuffer" :reader v-glsl-string)))

(v-deftype-internal v-iimage-buffer (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageBuffer" :reader v-glsl-string)))

(v-deftype-internal v-uimage-buffer (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageBuffer" :reader v-glsl-string)))

(v-deftype-internal v-image-1d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image1DArray" :reader v-glsl-string)))

(v-deftype-internal v-iimage-1d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage1DArray" :reader v-glsl-string)))

(v-deftype-internal v-uimage-1d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage1DArray" :reader v-glsl-string)))

(v-deftype-internal v-image-2d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DArray" :reader v-glsl-string)))

(v-deftype-internal v-iimage-2d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DArray" :reader v-glsl-string)))

(v-deftype-internal v-uimage-2d-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DArray" :reader v-glsl-string)))

(v-deftype-internal v-image-cube-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "imageCubeArray" :reader v-glsl-string)))

(v-deftype-internal v-iimage-cube-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimageCubeArray" :reader v-glsl-string)))

(v-deftype-internal v-uimage-cube-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimageCubeArray" :reader v-glsl-string)))

(v-deftype-internal v-image-2d-ms (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DMS" :reader v-glsl-string)))

(v-deftype-internal v-iimage-2d-ms (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DMS" :reader v-glsl-string)))

(v-deftype-internal v-uimage-2d-ms (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DMS" :reader v-glsl-string)))

(v-deftype-internal v-image-2d-ms-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "image2DMSArray" :reader v-glsl-string)))

(v-deftype-internal v-iimage-2d-ms-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "iimage2DMSArray" :reader v-glsl-string)))

(v-deftype-internal v-uimage-2d-ms-array (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uimage2DMSArray" :reader v-glsl-string)))

;;----------------------------------------------------------------------
;; Type Stubs
;;
;; {TODO} look into these

(v-deftype-internal v-buffer-image (v-type) ())

(v-deftype-internal v-ibuffer-image (v-type) ())

(v-deftype-internal v-ubuffer-image (v-type) ())

;;----------------------------------------------------------------------

(defun vec-of (type len)
  (assert (and (>= len 2) (<= len 4)))
  (type-spec->type
   (etypecase type
     (v-uint (nth (- len 2) '(:uvec2 :uvec3 :uvec4)))
     (v-int (nth (- len 2) '(:ivec2 :ivec3 :ivec4)))
     (v-float (nth (- len 2) '(:vec2 :vec3 :vec4)))
     (v-double (nth (- len 2) '(:dvec2 :dvec3 :dvec4))))))
