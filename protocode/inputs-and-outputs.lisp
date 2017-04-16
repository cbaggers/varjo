(in-package :varjo)


;;======================================================================


;; -- Vertex shader inputs --

;; Vertex Shaders have the following built-in input variables.

;; in int gl_VertexID;
;; in int gl_InstanceID;

;; gl_VertexID
;;     the index of the vertex currently being processed. When using
;;     non-indexed rendering, it is the effective index of the current
;;     vertex (the number of vertices processed + the first​ value). For
;;     indexed rendering, it is the index used to fetch this vertex from
;;     the buffer.
;;
;; Note: gl_VertexID will have the base vertex applied to it.

;; gl_InstanceID
;;     the index of the current instance when doing some form of
;;     instanced rendering. The instance count always starts at 0, even
;;     when using base instance calls. When not using instanced
;;     rendering, this value will be 0.
;;
;; Warning: This value does not follow the baseinstance​ provided by some
;; instanced rendering functions. gl_InstanceID always falls on the
;; half-open range [0, instancecount​).

;; -- Vertex shader outputs --

;; Vertex Shaders have the following predefined outputs.
;; out gl_PerVertex
;; {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; };

;; gl_Position
;;     the clip-space output position of the current vertex.

;; gl_PointSize
;;     the pixel width/height of the point being rasterized. It only has
;;     a meaning when rendering point primitives. It will be clamped to
;;     the GL_POINT_SIZE_RANGE.

;; gl_ClipDistance
;;     allows the shader to set the distance from the vertex to each
;;     user-defined clipping half-space. A non-negative distance means
;;     that the vertex is inside/behind the clip plane, and a negative
;;     distance means it is outside/in front of the clip plane. Each
;;     element in the array is one clip plane. In order to use this
;;     variable, the user must manually redeclare it with an explicit
;;     size.


;;======================================================================


;; -- Tessellation control shader inputs --

;; Tessellation Control Shaders provide the following built-in input variables:

;; in int gl_PatchVerticesIn;
;; in int gl_PrimitiveID;
;; in int gl_InvocationID;

;; gl_PatchVerticesIn
;;     the number of vertices in the input patch.
;;
;; gl_PrimitiveID
;;     the index of the current patch within this rendering command.
;;
;; gl_InvocationID
;;     the index of the TCS invocation within this patch. A TCS
;;     invocation writes to per-vertex output variables by using this to
;;     index them.

;; The TCS also takes the built-in variables output by the vertex shader:

;; in gl_PerVertex
;; {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; } gl_in[gl_MaxPatchVertices];

;; Note that just because gl_in is defined to have gl_MaxPatchVertices
;; entries does not mean that you can access beyond gl_PatchVerticesIn
;; and get reasonable values. These variables have only the meaning the
;; vertex shader that passed them gave them

;; -- Tessellation control shader outputs --

;; Tessellation Control Shaders have the following built-in patch output variables:

;; patch out float gl_TessLevelOuter[4];
;; patch out float gl_TessLevelInner[2];

;; These define the outer and inner tessellation levels used by the
;; tessellation primitive generator. They define how much tessellation to
;; apply to the patch. Their exact meaning depends on the type of patch
;; (and other settings) defined in the Tessellation Evaluation Shader.

;; Note: If any of the outer levels used by the abstract patch type is 0
;; or negative (or NaN), then the patch will be discarded by the
;; generator, and no TES invocations for this patch will result.

;; As with any other patch variable, multiple TCS invocations for the
;; same patch can write to the same tessellation level variable, so long
;; as they are all computing and writing the exact same value.

;; TCS's also provide the following optional per-vertex output variables:

;; out gl_PerVertex
;; {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; } gl_out[];

;; The use of any of these in a TCS is completely optional. Indeed, their
;; semantics will generally be of no practical value to the TCS. They
;; have the same general meaning as for vertex shaders, but since a TCS
;; must always be followed by an evaluation shader, the TCS never has to
;; write to any of them.


;;======================================================================


;; -- Tessellation evaluation shader inputs --

;; Tessellation Evaluation Shaders have the following built-in inputs.

;; in vec3 gl_TessCoord;
;; in int gl_PatchVerticesIn;
;; in int gl_PrimitiveID;

;; gl_TessCoord
;;     the location within the tessellated abstract patch for this
;;     particular vertex. Every input parameter other than this one will
;;     be identical for all TES invocations within a patch.
;;     Which components of this vec3 that have valid values depends on
;;     the abstract patch type. For isolines and quads, only the XY
;;     components have valid values. For triangles, all three components
;;     have valid values. All valid values are normalized floats (on the
;;     range [0, 1]).

;; gl_PatchVerticesIn
;;     the vertex count for the patch being processed. This is either the
;;     output vertex count specified by the TCS, or the patch vertex size
;;     specified by glPatchParameter if no TCS is active. Attempts to
;;     index per-vertex inputs by a value greater than or equal to
;;     gl_PatchVerticesIn results in undefined behavior.

;; gl_PrimitiveID
;;     the index of the current patch in the series of patches being
;;     processed for this draw call. Primitive Restart, if used, has no
;;     effect on the primitive ID.
;;
;; Note: The tessellation primitive generator will cull patches that have
;; a zero for one of the active outer tessellation levels. The intent of
;; the specification seems to be that gl_PrimitiveID will still be
;; incremented for culled patches. So the primitive ID for the TES is
;; equivalent to the ID for the TCS invocations that generated that
;; patch. But this is not entirely clear from the spec itself.


;; The TES also has access to the tessellation levels provided for the
;; patch by the TCS or by OpenGL:

;; patch in float gl_TessLevelOuter[4];
;; patch in float gl_TessLevelInner[2];

;; Only the outer and inner levels actually used by the abstract patch
;; are valid. For example, if this TES uses isolines, only
;; gl_TessLevelOuter[0] and gl_TessLevelOuter[1] will have valid values.

;; The TES also takes the built-in per-vertex variables output by the TCS:

;; in gl_PerVertex
;; {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; } gl_in[gl_MaxPatchVertices];
;;
;; Note that just because gl_in is defined to have gl_MaxPatchVertices
;; entries does not mean that you can access beyond gl_PatchVerticesIn
;; and get reasonable values.


;; -- Tessellation evaluation shader outputs --

;; Tessellation Evaluation Shaders have the following built-in outputs.

;; out gl_PerVertex {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; };

;; gl_PerVertex defines an interface block for outputs. The block is
;; defined without an instance name, so that prefixing the names is not
;; required.

;; These variables only take on the meanings below if this shader is the
;; last active Vertex Processing stage, and if rasterization is still
;; active (ie: GL_RASTERIZER_DISCARD is not enabled). The text below
;; explains how the Vertex Post-Processing system uses the
;; variables. These variables may not be redeclared with interpolation
;; qualifiers.

;; gl_Position
;;     the clip-space output position of the current vertex.

;; gl_PointSize
;;     the pixel width/height of the point being rasterized. It only has
;;     a meaning when rendering point primitives, which in a TES requires
;;     using the point_mode​ input layout qualifier.

;; gl_ClipDistance
;;     allows the shader to set the distance from the vertex to each
;;     User-Defined Clip Plane. A positive distance means that the vertex
;;     is inside/behind the clip plane, and a negative distance means it
;;     is outside/in front of the clip plane. Each element in the array
;;     is one clip plane. In order to use this variable, the user must
;;     manually redeclare it with an explicit size.


;;======================================================================


;; -- Geometry shader inputs --

;; Geometry Shaders provide the following built-in input variables:

;; in gl_PerVertex
;; {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; } gl_in[];

;; These variables have only the meaning the prior shader stage(s) that
;; passed them gave them.

;; There are some GS input values that are based on primitives, not
;; vertices. These are not aggregated into arrays. These are:

;; in int gl_PrimitiveIDIn;
;; in int gl_InvocationID;  //Requires GLSL 4.0 or ARB_gpu_shader5

;; gl_PrimitiveIDIn
;;     the current input primitive's ID, based on the number of
;;     primitives processed by the GS since the current drawing
;;     command started.
;;
;; gl_InvocationID
;;     the current instance, as defined when instancing geometry
;;     shaders.


;; -- Geometry shader outputs --

;; Geometry Shaders have the following built-in outputs.

;; out gl_PerVertex
;; {
;;   vec4 gl_Position;
;;   float gl_PointSize;
;;   float gl_ClipDistance[];
;; };

;; gl_PerVertex defines an interface block for outputs. The block is
;; defined without an instance name, so that prefixing the names is not
;; required.

;; The GS is the final Vertex Processing stage. Therefore, unless
;; rasterization is being turned off, you must write to some of these
;; values. These outputs are always associated with stream 0. So if
;; you're emitting vertices to a different stream, you don't have to
;; write to them.

;; gl_Position
;;     the clip-space output position of the current vertex. This
;;     value must be written if you are emitting a vertex to stream 0,
;;     unless rasterization is off.
;;
;; gl_PointSize
;;     the pixel width/height of the point being rasterized. It is
;;     only necessary to write to this when outputting point
;;     primitives.
;;
;; gl_ClipDistance
;;     allows the shader to set the distance from the vertex to each
;;     User-Defined Clip Plane. A positive distance means that the
;;     vertex is inside/behind the clip plane, and a negative distance
;;     means it is outside/in front of the clip plane. In order to use
;;     this variable, the user must manually redeclare it (and
;;     therefore the interface block) with an explicit size.

;; Certain predefined outputs have special meaning and semantics.

;; out int gl_PrimitiveID;

;; The primitive ID will be passed to the fragment shader. The primitive
;; ID for a particular line/triangle will be taken from the provoking
;; vertex of that line/triangle, so make sure that you are writing the
;; correct value for the right provoking vertex.

;; The meaning for this value is whatever you want it to be. However,
;; if you want to match the standard OpenGL meaning (ie: what the
;; Fragment Shader would get if no GS were used), you must do this for
;; each vertex before emitting it:

;; gl_PrimitiveID = gl_PrimitiveIDIn;

;; This naturally assumes that the number of primitives output by the
;; GS equals the number of primitives received by the GS.

;; Layered rendering in the GS works via two special output variables:

;; out int gl_Layer;
;; out int gl_ViewportIndex; //Requires GL 4.1 or ARB_viewport_array.

;; The gl_Layer output defines which layer in the layered image the
;; primitive goes to. Each vertex in the primitive must get the same
;; layer index. Note that when rendering to cubemap arrays, the
;; gl_Layer value represents layer-faces (the faces within a layer),
;; not the layers of cubemaps.

;; gl_ViewportIndex, which requires GL 4.1 or ARB_viewport_array,
;; specifies which viewport index to use with this primitive.
;; Note:
;; ARB_viewport_array, while technically a 4.1 feature, is widely
;; available on 3.3 hardware, from both NVIDIA and AMD.


;;======================================================================


;; -- Fragment shader inputs --

;; Fragment Shaders have the following built-in input variables.

;; in vec4 gl_FragCoord;
;; in bool gl_FrontFacing;
;; in vec2 gl_PointCoord;

;; gl_FragCoord
;;     The location of the fragment in window space. The X, Y and Z components are the window-space position of the fragment. The Z value will be written to the depth buffer if gl_FragDepth is not written to by this shader stage. The W component of gl_FragCoord is 1/Wclip, where Wclip is the interpolated W component of the clip-space vertex position output to gl_Position from the last Vertex Processing stage.
;;     The space of gl_FragCoord can be modified by redeclaring gl_FragCoord with special input layout qualifiers:

;; layout(origin_upper_left) in vec4 gl_FragCoord;

;;     This means that the origin for gl_FragCoord's window-space will be the upper-left of the screen, rather than the usual lower-left.

;; layout(pixel_center_integer​) in vec4 gl_FragCoord;

;;     OpenGL window space is defined such that pixel centers are on half-integer boundaries. So the center of the lower-left pixel is (0.5, 0.5). Using pixel_center_integer​ adjust gl_FragCoord such that whole integer values represent pixel centers.
;;     Both of these exist to be compatible with D3D's window space. Unless you need your shaders to have this compatibility, you are advised not to use these features.
;; gl_FrontFacing
;;     This is true if this fragment was generated by the front-face of the primitive; it is false if it was generated by the back-face. Only triangle Primitives have a back face; fragments generated by all other primitives will always have this be set to true.
;; gl_PointCoord
;;     The location within a point primitive that defines the position of the fragment relative to the side of the point. Points are effectively rasterized as window-space squares of a certain pixel size. Since points are defined by a single vertex, the only way to tell where in that square a particular fragment is is with gl_PointCoord.
;;     The values of gl_PointCoord's coordinates range from [0, 1]. OpenGL uses a upper-left origin for point-coordinates by default, so (0, 0) is the upper-left. However, the origin can be switched to a bottom-left origin by calling glPointParameteri(GL_POINT_SPRITE_COORD_ORIGIN, GL_LOWER_LEFT);

;; OpenGL 4.0 and above define additional system-generated input values:

;; in int gl_SampleID;
;; in vec2 gl_SamplePosition;
;; in int gl_SampleMaskIn[];

;; gl_SampleID
;;     This is an integer identifier for the current sample that this
;;     fragment is rasterized for.

;; Warning: Any use of this variable at all will force this shader to
;; be evaluated per-sample. Since much of the point of multisampling
;; is to avoid that, you should use it only when you must.

;; gl_SamplePosition
;;     This is the location of the current sample for the fragment
;;     within the pixel's area, with values on the range [0, 1]. The
;;     origin is the bottom-left of the pixel area.

;; Warning: Any use of this variable at all will force this shader to
;; be evaluated per-sample. Since much of the point of multisampling
;; is to avoid that, you should use it only when you must.

;; gl_SampleMaskIn
;;     When using multisampling, this variable contains a bitfield for
;;     the sample mask of the fragment being generated. The array is
;;     as long as needed to fill in the number of samples supported by
;;     the GL implementation.

;; Some Fragment shader built-in inputs will take values specified by
;; OpenGL, but these values can be overridden by user control.

;; in float gl_ClipDistance[];
;; in int gl_PrimitiveID;

;; gl_ClipDistance
;;     This array contains the interpolated clipping plane
;;     half-spaces, as output for vertices from the last Vertex
;;     Processing stage.
;;
;; gl_PrimitiveID
;;     This value is the index of the current primitive being rendered
;;     by this drawing command. This includes any Tessellation applied
;;     to the mesh, so each individual primitive will have a unique
;;     index.
;;
;;     However, if a Geometry Shader is active, then the
;;     gl_PrimitiveID is exactly and only what the GS provided as
;;     output. Normally, gl_PrimitiveID is guaranteed to be unique, so
;;     if two FS invocations have the same primitive ID, they come
;;     from the same primitive. But if a GS is active and outputs
;;     non-unique values, then different fragment shader invocations
;;     for different primitives will get the same value. If the GS did
;;     not output a value for gl_PrimitiveID, then the fragment shader
;;     gets an undefined value.

;; GL 4.3 provides the following additional inputs:

;; in int gl_Layer;
;; in int gl_ViewportIndex;

;; gl_Layer
;;     This is either 0 or the layer number for this primitive output
;;     by the Geometry Shader.
;;
;; gl_ViewportIndex
;;     This is either 0 or the viewport index for this primitive
;;     output by the Geometry Shader.

;; -- Fragment shader outputs --

;; Fragment Shaders have the following built-in output variables.

;; out float gl_FragDepth;

;; gl_FragDepth
;;     This output is the fragment's depth. If the shader does not
;;     statically write this value, then it will take the value of
;;     gl_FragCoord.z.
;;     To "statically write" to a variable means that you write to it
;;     anywhere in the program. Even if the writing code is
;;     technically unreachable for some reason, if there is a
;;     gl_FragDepth = ... expression anywhere in the shader, then it
;;     is statically written.

;; Warning:
;; If the fragment shader statically writes to gl_FragDepth, then it
;; is the responsibility of the shader to statically write to the
;; value in all circumstances. No matter what branches may or may not
;; be taken, the shader must ensure that the value is written. So, if
;; you conditionally write to it in one place, you should at least
;; make sure that there is a single non-conditional write sometime
;; before that.


;; GLSL 4.20 or ARB_conservative_depth allows the user to specify that
;; modifications to gl_FragDepth (relative to the gl_FragCoord.z value
;; it would have otherwise had) will happen in certain ways. This
;; allows the implementation the freedom to not turn off Early Depth
;; Tests in certain situations.

;; This is done by re-declaring gl_FragDepth with a special layout qualifier:

;; layout (depth_<condition>) out float gl_FragDepth;

;; The condition​ can be one of the following:

;; any
;;     The default. You may freely change the depth, but you lose the
;;     most potential performance.
;; greater
;;     You will only make the depth larger, compared to gl_FragDepth.z.
;; less
;;     You will only make the depth smaller, compared to gl_FragDepth.z.
;; unchanged
;;     If you write to gl_FragDepth, you will write exactly gl_FragDepth.z.

;; Violating the condition​ yields undefined behavior.

;; GLSL 4.00 or ARB_sample_shading brings us:

;; out int gl_SampleMask[];

;; gl_SampleMask
;;     This defines the sample mask for the fragment when performing
;;     mutlisampled rendering. If a shader does not statically write
;;     to it, then it will be filled in by gl_SampleMaskIn. The sample
;;     mask output here will be logically AND'd with the sample mask
;;     computed by the rasterizer.

;; Warning: Just as with gl_FragDepth, if a fragment shader writes to
;; gl_SampleMask at all, it must make sure to write to the value for
;; all execution paths. But it must also make sure to write to each
;; element in the array. The array has the same size as
;; gl_SampleMaskIn.


;;======================================================================


;; -- Shader uniforms --

;; Shaders define the following uniforms.

;; struct gl_DepthRangeParameters
;; {
;;     float near;
;;     float far;
;;     float diff;
;; };
;; uniform gl_DepthRangeParameters gl_DepthRange;

;; uniform int gl_NumSamples; //GLSL 4.20

;; This struct provides access to the glDepthRange near and far
;; values. The diff value is the far value minus the near value. Do
;; recall that OpenGL makes no requirement that far is greater than
;; near. With regard to multiple Viewports, gl_DepthRange only stores
;; the range for viewport 0.

;; gl_NumSamples is the number of samples in the current
;; Framebuffer. If the framebuffer is not multisampled, then this
;; value will be 1.


;;======================================================================
