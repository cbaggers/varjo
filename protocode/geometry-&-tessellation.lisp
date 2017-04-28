#||

The tessellation control processor is a programmable unit that operates
on a patch of incoming vertices and their associated data, emitting a
new output patch

[p.49] Since tessellation control, tessellation evaluation, and geometry
shaders operate on a set of vertices, each input variable (or input
block) needs to be declared as an array

For example:
in float foo[]; // geometry shader input for vertex “out float foo”

Each element of such an array corresponds to one vertex of the
primitive being processed. Each array can optionally have a size
declared. For geometry shaders, the array size will be set by, (or if
provided must be consistent with) the input layout declaration(s)
establishing the type of input primitive.

Geometry shader inputs, tessellation control shader inputs and
outputs, and tessellation evaluation inputs all have an additional
level of arrayness relative to other shader inputs and outputs.

Additionally, tessellation evaluation shaders support per-patch input
variables declared with the patch and in qualifiers. Per-patch input
variables are filled with the values of per-patch output variables
written by the tessellation control shader. Per-patch inputs may be
declared as one-dimensional arrays, but are not indexed by vertex
number. Applying the patch qualifier to inputs can only be done in
tessellation evaluation shaders. As with other input variables,
per-patch inputs must be declared using the same type and
qualification as per-patch outputs from the previous (tessellation
control) shader stage. It is a compile-time error to use patch with
inputs in any other stage.

Vertex, tessellation evaluation, and geometry output variables output
per-vertex data and are declared using the out storage
qualifier. Applying patch to an output can only be done in a
tessellation control shader

Interface blocks allow simpler addition of arrays to the interface
from vertex to geometry shader. They also allow a fragment shader to
have the same input interface as a geometry shader for a given vertex
shader.

Tessellation control shader output variables are may be used to output
per-vertex and per-patch data. Per-vertex output variables are
arrayed and declared using the out qualifier without the patch qualifier.

Per-patch output variables are declared using the patch and out
qualifiers.  Since tessellation control shaders produce an arrayed
primitive comprising multiple vertices, each per-vertex output
variable (or output block, see interface blocks below) needs to be
declared as an array. For example:

out float foo[]; // feeds next stage input “in float foo[]”

Each tessellation control shader invocation has a corresponding output
patch vertex, and may assign values to per-vertex outputs only if they
belong to that corresponding vertex. If a per-vertex output variable
is used as an l-value, it is a compile-time or link-time error if the
expression indicating the vertex index is not the identifier
gl_InvocationID.

Other than for the transform feedback layout qualifiers, tessellation
control shaders allow output layout qualifiers only on the interface
qualifier out, not on an output block, block member, or variable
declaration

In geometry shaders the primitive type and vertex count identifiers
are allowed only on the interface qualifier out, not on an output
block, block member, or variable declaration.

||#

#|| Tessellation Control.

Unlike Geometry Shaders, where each invocation can output multiple
primitives, each TCS invocation is only responsible for producing a
single vertex of output to the output patch.

The number of vertices in the output patch is defined with an output
layout qualifier:

layout(vertices = patch_size​) out;

patch_size​ must be an integral constant expression greater than zero
and less than the patch limit (see below). The output patch size does
not have to match the input patch size.

TCS inputs may have interpolation qualifiers on them. They have no
actual function however. (so we can remove them in Varjo)

TCS output variables are passed directly to the Tessellation
Evaluation Shader, without any form of interpolation (that's the TES's
main job). These can be per-vertex outputs and per-patch outputs.

Per-vertex outputs are aggregated into arrays:

out vec2 vertexTexCoord[];

(So Varjo should array per-vert out's for you)

The length of the array (vertexTexCoord.length() will always be the
size of the output patch. So you don't need to restate it in the
definition.

A TCS can only ever write to the per-vertex output variable that
corresponds to their invocation. So writes to per-vertex outputs must
be of the form vertexTexCoord[gl_InvocationID]. Any expression that
writes to a per-vertex output that doesn't index it with exactly
"gl_InvocationID" results in a compile-time error. Silly things like
vertexTexCoord[gl_InvocationID - 1 + 1] will also error.

Per-patch output variables are not aggregated into arrays (unless you
want them to be, in which case you must specify a size). All TCS
invocations for this patch see the same patch variables. They are
declared with the patch keyword:

patch out vec4 data;

Any TCS invocation can write to a per-patch output; indeed, all TCS
invocations will generally write to a per-patch output. As long as
they all write the same value, everything is fine.

||#

#|| Geometry: built-in variables are intrinsically declared as:

in gl_PerVertex {
    vec4 gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
} gl_in[];

in int gl_PrimitiveIDIn;
in int gl_InvocationID;

out gl_PerVertex {
    vec4 gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
};

out int gl_PrimitiveID;
out int gl_Layer;
out int gl_ViewportIndex;

||#

#|| Tessellation Control: built-in variables are intrinsically declared as:

in gl_PerVertex {
    vec4 gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
} gl_in[gl_MaxPatchVertices];

in int gl_PatchVerticesIn;
in int gl_PrimitiveID;
in int gl_InvocationID;

out gl_PerVertex {
    vec4 gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
} gl_out[];

patch out float gl_TessLevelOuter[4];
patch out float gl_TessLevelInner[2];

||#

#|| Tessellation Evaluation: built-in variables are intrinsically declared as:

in gl_PerVertex {
    vec4 gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
} gl_in[gl_MaxPatchVertices];

in int gl_PatchVerticesIn;
in int gl_PrimitiveID;
in vec3 gl_TessCoord;
patch in float gl_TessLevelOuter[4];
patch in float gl_TessLevelInner[2];

out gl_PerVertex {
    vec4 gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
};

||#

#|| Extra shiz

Fragment shader inputs that are signed or unsigned integers, integer
vectors, or any double-precision floating-point type must be qualified
with the interpolation qualifier flat.

It is a compile-time error to declare a vertex, tessellation
evaluation, tessellation control, or geometry shader output that
contains any of the following:
• A Boolean type (bool, bvec2, bvec3, bvec4)
• An opaque type

||#
