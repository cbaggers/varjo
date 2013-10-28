(in-package :varjo)

(defparameter *shader-types* '(:vertex :fragment :geometry))
(defparameter -default-version- :330)
(defparameter *shader-context* nil)

(defparameter *struct-definitions* nil)

(defparameter *glsl-functions* nil)
(defparameter *glsl-special-functions* nil)
(defparameter *glsl-substitutions* nil)
(defparameter *glsl-variables*
  '((:core 
     (gl-max-clip-distances :int "gl_MaxClipDistances" t)
     (gl-max-clip-planes :int "gl_MaxClipPlanes" t)
     (gl-max-draw-Buffers :int "gl_MaxDrawBuffers" t)
     (gl-max-texture-units :int "gl_MaxTextureUnits" t)
     (gl-max-texture-coords :int "gl_MaxTextureCoords" t)
     (gl-max-geometry-texture-image-units :int 
      "gl_MaxGeometryTextureImageUnits" t)
     (gl-max-texture-image-units :int "gl_MaxTextureImageUnits" t)
     (gl-max-vertex-attribs :int "gl_MaxVertexAttribs" t)
     (gl-max-vertex-texture-image-units :int 
      "gl_MaxVertexTextureImageUnits" t)
     (gl-max-combined-texture-image-units :int 
      "gl_MaxCombinesTextureImageUnits" t)
     (gl-max-geometry-varying-components :int 
      "gl_MaxGeometryVaryingComponents" t)
     (gl-max-varying-floats :int "gl_MaxVaryingFloats" t)
     (gl-max-geometry-output-vertices :int 
      "gl_MaxGeometryOutputVertices" t)
     (gl-max-fragment-uniform-components :int 
      "gl_MaxFragmentUniformComponents" t)
     (gl-max-geometry-total-output-components :int 
      "gl_MaxGeometryTotalOutputComponents" t)
     (gl-max-geometry-uniform-components :int 
      "gl_MaxGeometryUniformComponents" t)
     (gl-max-vertex-uniform-components :int 
      "gl_MaxVertexUniformComponents" t))
    (:vertex 
     (gl-vertex-id :int "gl_VertexID" t)
     (gl-instance-id :int "gl_InstanceID" t)
     (gl-color :vec4 "gl_Color" t)
     (gl-secondary-color :vec4 "gl_SecondaryColor" t)
     (gl-normal :vec3 "gl_Normal" t)
     (gl-vertex :vec4 "gl_Vertex" t)
     (gl-multi-tex-coord-0 :vec4 "gl_MultiTexCoord0" t)
     (gl-multi-tex-coord-1 :vec4 "gl_MultiTexCoord1" t)
     (gl-multi-tex-coord-2 :vec4 "gl_MultiTexCoord2" t)
     (gl-multi-tex-coord-3 :vec4 "gl_MultiTexCoord3" t)
     (gl-multi-tex-coord-4 :vec4 "gl_MultiTexCoord4" t)
     (gl-multi-tex-coord-5 :vec4 "gl_MultiTexCoord5" t)
     (gl-multi-tex-coord-6 :vec4 "gl_MultiTexCoord6" t)
     (gl-multi-tex-coord-7 :vec4 "gl_MultiTexCoord7" t)
     (gl-fog-coord :float "gl_FogCoord" t)
     (gl-position :vec4 "gl_Position")
     (gl-point-size :float "gl_PointSize")
     (gl-clip-distance (:float t) "gl_ClipDistance")
     (gl-clip-vertex :vec4 "gl_ClipVertex")
     (gl-front-color :vec4 "gl_FrontColor")
     (gl-back-color :vec4 "gl_BackColor")
     (gl-front-secondary-color :vec4 "gl_FrontSecondaryColor")
     (gl-back-secondary-color :vec4 "gl_FrontSecondaryColor")
     (gl-tex-coord (:vec4 t) "gl_TexCoord")
     (gl-fog-frag-coord :float "gl_FogFragCoord"))
    (:fragment 
     (gl-frag-coord :vec4 "gl_FragCoord" t)
     (gl-front-facing :bool  "gl_FrontFacing" t)
     (gl-clip-distance (:float t) "gl_ClipDistance" t)
     (gl-point-coord :vec2  "gl_PointCoord" t)
     (gl-primitive-id :int "gl_PrimitiveID" t)
     (gl-frag-depth :float "gl_FragDepth" nil))
    (:geometry
     (gl-in (vgl-per-vertex-g t) "gl_in" t)
     (gl-primitive-id-in :int "gl_PrimitiveIDIn" t)
     (gl-position :vec4 "gl_Position")
     (gl-point-size :float "gl_PointSize")
     (gl-clip-distance (:float t) "gl_ClipDistance")
     (gl-primitive-id :int "gl_PrimitiveID")
     (gl-layer :int "gl_Layer"))))

