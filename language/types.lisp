
(in-package :varjo)

;;----------------------------------------------------------------------

(defclass v-t-type () ())
(defclass v-type (v-t-type) 
  ((core :initform nil :reader core-typep)
   (place :initform t :initarg :place :accessor v-placep)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1)
   (casts-to :initform nil)
   (uniform-string-gen :initform nil :initarg :uniform-string-gen 
                       :reader uniform-string-gen)))

(defclass v-stemcell (v-type) ())
(defmethod v-dimensions ((object v-stemcell)) 0)
(defun make-stem-cell (symbol)
  (let ((string-name (string (safe-glsl-name-string symbol)))
        (original-name symbol))
    (make-instance
     'code
     :type 'v-stemcell 
     :current-line string-name
     :stemcells `((,original-name ,string-name '|unknown-type|)))))

(defun stemcellp (x)
  (typep x 'v-stemcell))

(defmethod set-place-t ((type v-type))
  (setf (v-placep type) t) type)

(defmethod core-typep ((type v-t-type))
  nil)

;; [TODO] ensure all cast lists have the correct order.

;; spec types are to handle the manifest ugliness of the glsl spec.
;; dear god just one txt file with every permutation of every glsl
;; function would have save me so many hours work.
(defclass v-spec-type () 
  ((place :initform t :initarg :place :reader v-placep)))
(defclass v-tfd (v-spec-type) ())
(defclass v-tf (v-tfd) ()) ;; float vec*
(defclass v-td (v-tfd) ()) ;; double dvec*
(defclass v-tb (v-spec-type) ()) ;; bool bvec*
(defclass v-tiu (v-spec-type) ())
(defclass v-i-ui (v-spec-type) ()) ;; int uint
(defclass v-ti (v-tiu) ()) ;; int ivec*
(defclass v-tu (v-tiu) ()) ;; uint uvec*
(defclass v-tvec (v-spec-type) ()) ;;vec* uvec* ivec* [notice no dvec]

(defclass v-array (v-container) 
  ((element-type :initform nil :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)))
(defmethod v-glsl-string ((object v-array))
  (format nil "~a ~~a~{[~a]~}" (v-glsl-string (v-element-type object)) (v-dimensions object)))

(defclass v-none (v-t-type) ())

(defclass v-function (v-type)
  ((restriction :initform nil :initarg :restriction :accessor v-restriction)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (required-glsl :initform nil :initarg :required-glsl :accessor v-required-glsl)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)
   (place :initform nil :initarg :place :accessor v-placep)
   (glsl-spec-matching :initform nil :initarg :glsl-spec-matching :reader v-glsl-spec-matchingp)
   (multi-return-vars :initarg nil :initarg :multi-return-vars :reader multi-return-vars)))

(defclass v-struct (v-type)
  ((restriction :initform nil :initarg :restriction :accessor v-restriction)
   (signature :initform nil :initarg :signature :accessor v-signature)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (slots :initform nil :initarg :slots :reader v-slots)
   (true-type :initform nil :initarg :true-type :reader v-true-type)
   (fake-type :initform nil :initarg :fake-type :reader v-fake-type)))

(defclass v-user-struct (v-struct) ())

(defclass v-error (v-type) 
  ((payload :initform nil :initarg :payload :accessor v-payload)))

(defclass v-void (v-t-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "void" :reader v-glsl-string)))

(defclass v-bool (v-type v-tb) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bool" :reader v-glsl-string)))

(defclass v-number (v-type) ())
(defclass v-int (v-number v-ti v-i-ui)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "int" :reader v-glsl-string)
   (casts-to :initform '(v-uint v-float v-double))))
(defclass v-uint (v-number v-tu v-i-ui)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uint" :reader v-glsl-string)
   (casts-to :initform '(v-float v-double))))
(defclass v-float (v-number v-tf)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "float" :reader v-glsl-string)
   (casts-to :initform '(v-double))))
(defclass v-short-float (v-number) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "short-float" :reader v-glsl-string)))
(defclass v-double (v-number v-td) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "double" :reader v-glsl-string)))

(defclass v-container (v-type)
  ((element-type :initform nil)
   (dimensions :initform nil :accessor v-dimensions)))

(defclass v-matrix (v-container) ())
(defclass v-mat2 (v-matrix) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2))))
(defclass v-mat3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3))))
(defclass v-mat4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4))))
(defclass V-MAT2X2 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat2x2" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)))
(defclass V-MAT2X3 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat2x3" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(2 3) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x3))))
(defclass V-MAT2X4 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat2x4" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(2 4) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x4))))
(defclass V-MAT3X2 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat3x2" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(3 2) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x2))))
(defclass V-MAT3X3 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat3x3" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)))
(defclass V-MAT3X4 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat3x4" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(3 4) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x4))))
(defclass V-MAT4X2 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat4x2" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(4 2) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x2))))
(defclass V-MAT4X3 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat4x3" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(4 3) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x3))))
(defclass V-MAT4X4 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat4x4" :reader v-glsl-string)
   (element-type :initform 'V-FLOAT)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)))
(defclass v-dmat2 (v-matrix) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat2" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2))))
(defclass v-dmat3 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat3" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3))))
(defclass v-dmat4 (v-matrix)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "mat4" :reader v-glsl-string)
   (element-type :initform 'v-double)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4))))
(defclass V-DMAT2X2 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat2x2" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(2 2) :reader v-dimensions)
   (glsl-size :initform 2)))
(defclass V-DMAT2X3 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat2x3" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(2 3) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x3))))
(defclass V-DMAT2X4 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat2x4" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(2 4) :reader v-dimensions)
   (glsl-size :initform 2)
   (casts-to :initform '(v-dmat2x4))))
(defclass V-DMAT3X2 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat3x2" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(3 2) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x2))))
(defclass V-DMAT3X3 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat3x3" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(3 3) :reader v-dimensions)
   (glsl-size :initform 3)))
(defclass V-DMAT3X4 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat3x4" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(3 4) :reader v-dimensions)
   (glsl-size :initform 3)
   (casts-to :initform '(v-dmat3x4))))
(defclass V-DMAT4X2 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat4x2" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(4 2) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x2))))
(defclass V-DMAT4X3 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat4x3" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(4 3) :reader v-dimensions)
   (glsl-size :initform 4)
   (casts-to :initform '(v-dmat4x3))))
(defclass V-DMAT4X4 (v-matrix)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "mat4x4" :reader v-glsl-string)
   (element-type :initform 'V-DOUBLE)
   (dimensions :initform '(4 4) :reader v-dimensions)
   (glsl-size :initform 4)))

(defclass v-vector (v-container) ())
(defclass v-fvector (v-vector v-tf v-tvec) ())

(defclass v-vec2 (v-fvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec2" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-dvec2))))
(defclass v-vec3 (v-fvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec3" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-dvec3))))
(defclass v-vec4 (v-fvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "vec4" :reader v-glsl-string)
   (element-type :initform 'v-float)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-dvec4))))

(defclass v-bvector (v-vector v-tb) ())
(defclass v-bvec2 (v-bvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec2" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(2) :reader v-dimensions)))
(defclass v-bvec3 (v-bvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec3" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(3) :reader v-dimensions)))
(defclass v-bvec4 (v-bvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "bvec4" :reader v-glsl-string)
   (element-type :initform 'v-bool)
   (dimensions :initform '(4) :reader v-dimensions)))

(defclass v-uvector (v-vector v-tu) ())
(defclass v-uvec2 (v-uvector v-tvec)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec2" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-dvec2 v-vec2))))
(defclass v-uvec3 (v-uvector)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec3" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-dvec3 v-vec3))))
(defclass v-uvec4 (v-uvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "uvec4" :reader v-glsl-string)
   (element-type :initform 'v-uint)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-dvec4 v-vec4))))

(defclass v-ivector (v-vector v-ti) ())
(defclass v-ivec2 (v-ivector v-tvec) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec2" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-uvec2 v-vec2 v-dvec2))))
(defclass v-ivec3 (v-ivector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec3" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-uvec3 v-vec3 v-dvec3))))
(defclass v-ivec4 (v-ivector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec4" :reader v-glsl-string)
   (element-type :initform 'v-int)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-uvec4 v-vec4 v-dvec4))))

(defclass v-dvector (v-vector) ())
(defclass v-dvec2 (v-dvector v-td) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec2" :reader v-glsl-string)
   (element-type :initform 'v-dnt)
   (dimensions :initform '(2) :reader v-dimensions)
   (casts-to :initform '(v-uvec2 v-vec2 v-dvec2))))
(defclass v-dvec3 (v-dvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec3" :reader v-glsl-string)
   (element-type :initform 'v-dnt)
   (dimensions :initform '(3) :reader v-dimensions)
   (casts-to :initform '(v-uvec3 v-vec3 v-dvec3))))
(defclass v-dvec4 (v-dvector) 
  ((core :initform t :reader core-typep)
   (glsl-string :initform "ivec4" :reader v-glsl-string)
   (element-type :initform 'v-dnt)
   (dimensions :initform '(4) :reader v-dimensions)
   (casts-to :initform '(v-uvec4 v-vec4 v-dvec4))))

(defclass v-sampler (v-type) ())
(defclass V-ISAMPLER-1D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler1D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-1D-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler1DArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-2D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler2D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-2D-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler2DArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-2D-MS (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler2DMS" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-2D-MS-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-2D-RECT (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler2DRect" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-3D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isampler3D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-BUFFER (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isamplerBuffer" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-CUBE (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isamplerCube" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-ISAMPLER-CUBE-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "isamplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-1D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler1D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-1D-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler1DArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-1D-ARRAY-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler1DArrayShadow" :reader
                glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-1D-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler1DShadow" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-ARRAY-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DArrayShadow" :reader
                glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-MS (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DMS" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-MS-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-RECT (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DRect" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-RECT-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DRectShadow" :reader
                glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-2D-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler2DShadow" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-3D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "sampler3D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-BUFFER (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "samplerBuffer" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-CUBE (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "samplerCube" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-CUBE-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "samplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-CUBE-ARRAY-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "samplerCubeArrayShadow" :reader
                glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass v-sampler-CUBE-SHADOW (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "samplerCubeShadow" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-1D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler1D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-1D-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler1DArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-2D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler2D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-2D-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler2DArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-2D-MS (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler2DMS" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-2D-MS-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler2DMSArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-2D-RECT (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler2DRect" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-3D (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usampler3D" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-BUFFER (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usamplerBuffer" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-CUBE (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usamplerCube" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))
(defclass V-USAMPLER-CUBE-ARRAY (v-sampler)
  ((core :initform T :reader core-typep)
   (glsl-string :initform "usamplerCubeArray" :reader v-glsl-string)
   (element-type :initform 'V-VEC4)))

;;----------------------------------------------------------------------

;;[TODO] Implement the following, check for others we need

;; (%vdefstruct vgl-per-vertex-v (:slot-prefix per-vertex
;;                                             :context-restriction ((:330) :vertex))
;;   (position :vec4 "gl_Position")
;;   (point-size :float "gl_PointSize")
;;   (clip-distance (:float t) "gl_ClipDistance")
;;   (clip-vertex :vec4 "gl_ClipVertex"))

;; (%vdefstruct vgl-per-vertex-g (:slot-prefix per-vertex
;;                                             :context-restriction ((:330) :fragment))
;;   (position :vec4 "gl_Position")
;;   (point-size :float "gl_PointSize")
;;   (clip-distance (:float t) "gl_ClipDistance"))

