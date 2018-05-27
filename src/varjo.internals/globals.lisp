(in-package :varjo.internals)
(in-readtable fn:fn-reader)

(defvar *global-env-form-bindings* (make-hash-table :test #'eq))
(defvar *global-env-compiler-macros* (make-hash-table :test #'eq))

(defvar *supported-versions* '(:140 :150 :330 :400 :410 :420 :430 :440 :450 :460))

(defvar *stage-names*
  '(:vertex
    :tessellation-control
    :tessellation-evaluation
    :geometry
    :fragment
    :compute))

(defvar *stage-type-names*
  '(vertex-stage
    tessellation-control-stage
    tessellation-evaluation-stage
    geometry-stage
    fragment-stage
    compute-stage))

(defvar *unshadowable-names* '(;; special
                               and
                               flet
                               for
                               function
                               vari.cl:glsl-expr
                               if
                               labels
                               vari.cl:labels-no-implicit
                               let
                               multiple-value-bind
                               or
                               progn
                               setq
                               vari.glsl:switch
                               vari.cl:swizzle
                               the
                               values
                               varjo.internals:values-safe
                               vari.glsl:while
                               ;; macros
                               let*
                               prog1
                               setf
                               symbol-macrolet
                               unless
                               when))

(defvar *default-version* :460)


(defvar *stemcell-infer-hook*
  (lambda (name)
    (declare (ignore name))
    nil))

(defvar *constant-inject-hook*
  (lambda (name)
    (declare (ignore name))
    nil))

(defvar *registered-types*
  (make-hash-table))

(defvar +ascii-alpha-num+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defparameter *glsl-qualifiers*
  '((:attribute "attribute")
    (:buffer "buffer")
    (:centroid "centroid")
    (:coherent "coherent")
    (:const "const")
    (:flat "flat")
    (:highp "highp")
    (:in "in")
    (:invariant "invariant")
    (:lowp "lowp")
    (:mediump "mediump")
    (:noperspective "noperspective")
    (:out "out")
    (:in/out "inout")
    (:packed "packed")
    (:readonly "readonly")
    (:restrict "restrict")
    (:sample "sample")
    (:shared "shared")
    (:smooth "smooth")
    (:std-140 "std140")
    (:std-430 "std430")
    (:uniform "uniform")
    (:varying "varying")
    (:volatile "volatile")
    (:writeonly "writeonly")))

(defparameter *varjo-qualifiers*
  (append
   *glsl-qualifiers*
   '((:ssbo "ssbo")
     (:ubo "ubo")
     (:feedback nil))))

(defvar *glsl-variables* nil)

(defvar *fallback-block-name* :in_block)
(defvar *in-block-name* "v_in")
(defvar *out-block-name* "v_out")
(defvar *emit-var-name-base* "emit")
(defvar *return-var-name-base* "return")

(defvar *type-shorthand*
  ;; WARNING ORDER SENSITIVE
  ;; int32 & uint32 must come before :int & :uint otherwise
  ;; (type->type-spec (type-spec->type :int)) resolves to
  ;; :int32, when it should be :int
  '((:bool . v-bool)
    (:int32 . v-int)
    (:uint32 . v-uint)
    (:int . v-int)
    (:uint . v-uint)
    (:float . v-float)
    (:short-float . v-short-float)
    (:double . v-double)
    (:mat2 . v-mat2)
    (:mat3 . v-mat3)
    (:mat4 . v-mat4)
    (:mat2x2 . v-mat2x2)
    (:mat2x3 . v-mat2x3)
    (:mat2x4 . v-mat2x4)
    (:mat3x2 . v-mat3x2)
    (:mat3x3 . v-mat3x3)
    (:mat3x4 . v-mat3x4)
    (:mat4x2 . v-mat4x2)
    (:mat4x3 . v-mat4x3)
    (:mat4x4 . v-mat4x4)
    (:dmat2 . v-dmat2)
    (:dmat3 . v-dmat3)
    (:dmat4 . v-dmat4)
    (:dmat2x2 . v-dmat2x2)
    (:dmat2x3 . v-dmat2x3)
    (:dmat2x4 . v-dmat2x4)
    (:dmat3x2 . v-dmat3x2)
    (:dmat3x3 . v-dmat3x3)
    (:dmat3x4 . v-dmat3x4)
    (:dmat4x2 . v-dmat4x2)
    (:dmat4x3 . v-dmat4x3)
    (:dmat4x4 . v-dmat4x4)
    (:vec2 . v-vec2)
    (:vec3 . v-vec3)
    (:vec4 . v-vec4)
    (:bvec2 . v-bvec2)
    (:bvec3 . v-bvec3)
    (:bvec4 . v-bvec4)
    (:uvec2 . v-uvec2)
    (:uvec3 . v-uvec3)
    (:uvec4 . v-uvec4)
    (:ivec2 . v-ivec2)
    (:ivec3 . v-ivec3)
    (:ivec4 . v-ivec4)
    (:dvec2 . v-dvec2)
    (:dvec3 . v-dvec3)
    (:dvec4 . v-dvec4)
    (:isampler-1d . v-isampler-1d)
    (:isampler-1d-array . v-isampler-1d-array)
    (:isampler-2d . v-isampler-2d)
    (:isampler-2d-array . v-isampler-2d-array)
    (:isampler-2d-ms . v-isampler-2d-ms)
    (:isampler-2d-ms-array . v-isampler-2d-ms-array)
    (:isampler-2d-rect . v-isampler-2d-rect)
    (:isampler-3d . v-isampler-3d)
    (:isampler-buffer . v-isampler-buffer)
    (:isampler-cube . v-isampler-cube)
    (:isampler-cube-array . v-isampler-cube-array)
    (:sampler-1d . v-sampler-1d)
    (:sampler-1d-array . v-sampler-1d-array)
    (:sampler-1d-array-shadow . v-sampler-1d-array-shadow)
    (:sampler-1d-shadow . v-sampler-1d-shadow)
    (:sampler-2d . v-sampler-2d)
    (:sampler-2d-array . v-sampler-2d-array)
    (:sampler-2d-array-shadow . v-sampler-2d-array-shadow)
    (:sampler-2d-ms . v-sampler-2d-ms)
    (:sampler-2d-ms-array . v-sampler-2d-ms-array)
    (:sampler-2d-rect . v-sampler-2d-rect)
    (:sampler-2d-rect-shadow . v-sampler-2d-rect-shadow)
    (:sampler-2d-shadow . v-sampler-2d-shadow)
    (:sampler-3d . v-sampler-3d)
    (:sampler-buffer . v-sampler-buffer)
    (:sampler-cube . v-sampler-cube)
    (:sampler-cube-array . v-sampler-cube-array)
    (:sampler-cube-array-shadow . v-sampler-cube-array-shadow)
    (:sampler-cube-shadow . v-sampler-cube-shadow)
    (:usampler-1d . v-usampler-1d)
    (:usampler-1d-array . v-usampler-1d-array)
    (:usampler-2d . v-usampler-2d)
    (:usampler-2d-array . v-usampler-2d-array)
    (:usampler-2d-ms . v-usampler-2d-ms)
    (:usampler-2d-ms-array . v-usampler-2d-ms-array)
    (:usampler-2d-rect . v-usampler-2d-rect)
    (:usampler-3d . v-usampler-3d)
    (:usampler-buffer . v-usampler-buffer)
    (:usampler-cube . v-usampler-cube)
    (:usampler-cube-array . v-usampler-cube-array)
    (:sampler-rect . v-sampler-rect)
    (:isampler-rect . v-isampler-rect)
    (:usampler-rect . v-usampler-rect)
    (:atomic-uint . v-atomic-uint)
    (:image-1d . v-image-1d)
    (:iimage-1d . v-iimage-1d)
    (:uimage-1d . v-uimage-1d)
    (:image-2d . v-image-2d)
    (:iimage-2d . v-iimage-2d)
    (:uimage-2d . v-uimage-2d)
    (:image-3d . v-image-3d)
    (:iimage-3d . v-iimage-3d)
    (:uimage-3d . v-uimage-3d)
    (:image-rect . v-image-rect)
    (:iimage-rect . v-iimage-rect)
    (:uimage-rect . v-uimage-rect)
    (:image-2d-rect . v-image-2d-rect)
    (:iimage-2d-rect . v-iimage-2d-rect)
    (:uimage-2d-rect . v-uimage-2d-rect)
    (:image-cube . v-image-cube)
    (:iimage-cube . v-iimage-cube)
    (:uimage-cube . v-uimage-cube)
    (:image-buffer . v-image-buffer)
    (:iimage-buffer . v-iimage-buffer)
    (:uimage-buffer . v-uimage-buffer)
    (:image-1d-array . v-image-1d-array)
    (:iimage-1d-array . v-iimage-1d-array)
    (:uimage-1d-array . v-uimage-1d-array)
    (:image-2d-array . v-image-2d-array)
    (:iimage-2d-array . v-iimage-2d-array)
    (:uimage-2d-array . v-uimage-2d-array)
    (:image-cube-array . v-image-cube-array)
    (:iimage-cube-array . v-iimage-cube-array)
    (:uimage-cube-array . v-uimage-cube-array)
    (:image-2d-ms . v-image-2d-ms)
    (:iimage-2d-ms . v-iimage-2d-ms)
    (:uimage-2d-ms . v-uimage-2d-ms)
    (:image-2d-ms-array . v-image-2d-ms-array)
    (:iimage-2d-ms-array . v-iimage-2d-ms-array)
    (:uimage-2d-ms-array . v-uimage-2d-ms-array)
    (:buffer-image . v-buffer-image)
    (:ibuffer-image . v-ibuffer-image)
    (:ubuffer-image . v-ubuffer-image)
    (:void . v-void)
    (:shadow-type . v-shadow-type)))

(defvar *base-reserved*
  '("active"
    "asm"
    "atomic_uint"
    "attribute"
    "bool"
    "break"
    "buffer"
    "bvec2"
    "bvec3"
    "bvec4"
    "case"
    "cast"
    "centroid"
    "class"
    "coherent"
    "common"
    "const"
    "continue"
    "default"
    "discard"
    "dmat2"
    "dmat2x2"
    "dmat2x3"
    "dmat2x4"
    "dmat3"
    "dmat3x2"
    "dmat3x3"
    "dmat3x4"
    "dmat4"
    "dmat4x2"
    "dmat4x3"
    "dmat4x4"
    "do"
    "double"
    "dvec2"
    "dvec3"
    "dvec43"
    "else"
    "enum"
    "extern"
    "external"
    "false"
    "filter"
    "fixed"
    "flat"
    "float"
    "for"
    "fvec2"
    "fvec3"
    "fvec4"
    "goto"
    "half"
    "highp"
    "hvec2"
    "hvec3"
    "hvec4"
    "if"
    "iimage1D"
    "iimage1DArray"
    "iimage2D"
    "iimage2DArray"
    "iimage2DMS"
    "iimage2DMSArray"
    "iimage2DRect"
    "iimage3D"
    "iimageBuffer"
    "iimageCube"
    "iimageCubeArray"
    "image1D"
    "image1DArray"
    "image2D"
    "image2DArray"
    "image2DMS"
    "image2DMSArray"
    "image2DRect"
    "image3D"
    "imageBuffer"
    "imageCube"
    "imageCubeArray"
    "in"
    "inline"
    "inout"
    "input"
    "int"
    "interface"
    "invariant"
    "isampler1D"
    "isampler1DArray"
    "isampler2D"
    "isampler2DArray"
    "isampler2DMS"
    "isampler2DMSArray"
    "isampler2DRect"
    "isampler3D"
    "isamplerBuffer"
    "isamplerCube"
    "isamplerCubeArray"
    "ivec2"
    "ivec3"
    "ivec4"
    "layout"
    "long"
    "lowp"
    "mat2"
    "mat2x2"
    "mat2x3"
    "mat2x4"
    "mat3"
    "mat3x2"
    "mat3x3"
    "mat3x4"
    "mat4"
    "mat4x2"
    "mat4x3"
    "mat4x4"
    "mediump"
    "namespace"
    "noinline"
    "noperspective"
    "out"
    "output"
    "partition"
    "patch"
    "precise"
    "precision"
    "public"
    "readonly"
    "resource"
    "restrict"
    "return"
    "sample"
    "sampler1D"
    "sampler1DArray"
    "sampler1DArrayShadow"
    "sampler1DShadow"
    "sampler2D"
    "sampler2DArray"
    "sampler2DArrayShadow"
    "sampler2DMS"
    "sampler2DMSArray"
    "sampler2DRect"
    "sampler2DRectShadow"
    "sampler2DShadow"
    "sampler3D"
    "sampler3DRect"
    "samplerBuffer"
    "samplerCube"
    "samplerCubeArray"
    "samplerCubeArrayShadow"
    "samplerCubeShadow"
    "shared"
    "short"
    "sizeof"
    "smooth"
    "static"
    "struct"
    "subroutine"
    "superp"
    "switch"
    "template"
    "this"
    "true"
    "typedef"
    "uimage1D"
    "uimage1DArray"
    "uimage2D"
    "uimage2DArray"
    "uimage2DMS"
    "uimage2DMSArray"
    "uimage2DRect"
    "uimage3D"
    "uimageBuffer"
    "uimageCube"
    "uimageCubeArray"
    "uint"
    "uniform"
    "union"
    "unsigned"
    "usampler1D"
    "usampler1DArray"
    "usampler2D"
    "usampler2DArray"
    "usampler2DMS"
    "usampler2DMSArray"
    "usampler2DRect"
    "usampler3D"
    "usamplerBuffer"
    "usamplerCube"
    "usamplerCubeArray"
    "using"
    "uvec2"
    "uvec3"
    "uvec4"
    "varying"
    "vec2"
    "vec3"
    "vec4"
    "void"
    "volatile"
    "while"
    "writeonly"))
