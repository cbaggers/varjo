#|| Interface block pain

So much ugliness & inconsistancy comes about becuase of how we have to use
interface blocks

[p.35 - glsl4.10]
Since tessellation control, tessellation evaluation, and geometry
shaders operate on a set of vertices, each input variable (or input
block) needs to be declared as an array. For example:
```
    in float foo[]; // geometry shader input for vertex “out float foo”
```

[p.41 - glsl4.10]
Block names have no other use within a shader beyond interface
matching; it is an error to use a block name at global scope for
anything other than as a block name


[p.41 - glsl4.10]
[only talking about arrayd uniform blocks here but meh]
For uniform blocks declared as an array, each individual array element
corresponds to a separate buffer object backing one instance of the
block. As the array size indicates the number of buffer objects
needed, uniform block array declarations must specify an array
size. Any integral expression can be used to index a uniform block
array, as per section 4.1.9 "Arrays"

[p.36 - glsl4.40]
[Notice that it's only for SSBOs, not UBOs]
All arrays are inherently homogeneous; made of elements all having the
same type and size, with one exception. The exception is a shader
storage block having an unsized array as its last member (run-time
sized); an array can be formed from such a shader storage block, even
if the storage blocks have differing lengths for their last member.

||#


#||------------------------------------------------------------
 || Failure due to our use of blocks

 UBO-1 []:
Unexpected Error: #<SIMPLE-ERROR "Error compiling ~(~a~): ~%~a~%~%~a" {10027AA973}>
Error compiling vertex-shader:
ERROR: 0:18: You must access a specific variable of the interface block 'THE_DATA'
ERROR: 0:19: Use of undeclared identifier 'g_with_slots_tmp1215'
ERROR: 0:20: Use of undeclared identifier 'g_GEXPR0_1218'


// vertex-stage
#version 410

struct SOME_DATA {
    int INTS[(1000)];
};

layout(location = 0)  in vec3 fk_vert_position;
layout(location = 1)  in vec4 fk_vert_color;

layout(std140) uniform _UBO_THE_DATA
{
    int[1000] INTS;
} THE_DATA;

void main()
{
    SOME_DATA g_with_slots_tmp1215 = THE_DATA;
    vec4 g_GEXPR0_1218 = vec4(float(g_with_slots_tmp1215.INTS[1]),float(2),float(3),float(4));
    gl_Position = g_GEXPR0_1218;
    return;
}
||#

;;------------------------------------------------------------
;; Valid pipeline using blocks

"// vertex-stage
#version 410

layout(location = 0)  in vec4 POSITION;

out _FROM_VERTEX_STAGE_
{
     out vec3 _VERTEX_STAGE_OUT_1;
} v_out;

uniform float TESS_LEVEL_INNER;
uniform float TESS_LEVEL_OUTER;
uniform mat4 PROJECTION;
uniform mat4 MODEL_TO_CLIP;
uniform mat3 NORMAL_MAT;
uniform vec3 LIGHT_POSITION;
uniform vec3 DIFFUSE_MATERIAL;
uniform vec3 AMBIENT_MATERIAL;

void main()
{
    vec4 g_PROG1_TMP2337 = POSITION;
    v_out._VERTEX_STAGE_OUT_1 = POSITION.xyz;
    vec4 g_GEXPR0_2338 = g_PROG1_TMP2337;
    gl_Position = g_GEXPR0_2338;
    return;
}

"



"// tessellation-control-stage
#version 410

in _FROM_VERTEX_STAGE_
{
     in vec3 _VERTEX_STAGE_OUT_1;
} v_in[gl_MaxPatchVertices];

layout (vertices = 3) out;

out _FROM_TESSELLATION_CONTROL_STAGE_
{
     out vec3 _TESSELLATION_CONTROL_STAGE_OUT_0;
} v_out[3];

uniform float TESS_LEVEL_INNER;
uniform float TESS_LEVEL_OUTER;
uniform mat4 PROJECTION;
uniform mat4 MODEL_TO_CLIP;
uniform mat3 NORMAL_MAT;
uniform vec3 LIGHT_POSITION;
uniform vec3 DIFFUSE_MATERIAL;
uniform vec3 AMBIENT_MATERIAL;

void main()
{
    float TESS_LEVEL_INNER0 = 5.0f;
    float TESS_LEVEL_OUTER0 = 5.0f;
    if ((gl_InvocationID == 0))
    {
        gl_TessLevelInner[0] = TESS_LEVEL_INNER0;
        gl_TessLevelOuter[0] = TESS_LEVEL_OUTER0;
        gl_TessLevelOuter[1] = TESS_LEVEL_OUTER0;
        gl_TessLevelOuter[2] = TESS_LEVEL_OUTER0;
    }
    vec3 g_GEXPR0_2339 = v_in[gl_InvocationID]._VERTEX_STAGE_OUT_1;
    v_out[gl_InvocationID]._TESSELLATION_CONTROL_STAGE_OUT_0 = g_GEXPR0_2339;
    return;
}

"




"// tessellation-evaluation-stage
#version 410

in _FROM_TESSELLATION_CONTROL_STAGE_
{
     in vec3 _TESSELLATION_CONTROL_STAGE_OUT_0;
} v_in[gl_MaxPatchVertices];

layout (triangles, equal_spacing, ccw) in;

out _FROM_TESSELLATION_EVALUATION_STAGE_
{
     out vec3 _TESSELLATION_EVALUATION_STAGE_OUT_1;
     out vec3 _TESSELLATION_EVALUATION_STAGE_OUT_2;
} v_out;

uniform float TESS_LEVEL_INNER;
uniform float TESS_LEVEL_OUTER;
uniform mat4 PROJECTION;
uniform mat4 MODEL_TO_CLIP;
uniform mat3 NORMAL_MAT;
uniform vec3 LIGHT_POSITION;
uniform vec3 DIFFUSE_MATERIAL;
uniform vec3 AMBIENT_MATERIAL;

void main()
{
    vec3 P0 = (gl_TessCoord.x * v_in[0]._TESSELLATION_CONTROL_STAGE_OUT_0);
    vec3 P1 = (gl_TessCoord.y * v_in[1]._TESSELLATION_CONTROL_STAGE_OUT_0);
    vec3 P2 = (gl_TessCoord.z * v_in[2]._TESSELLATION_CONTROL_STAGE_OUT_0);
    vec3 POS = normalize((P0 + (P1 + P2)));
    vec4 g_PROG1_TMP2340 = (MODEL_TO_CLIP * vec4(POS,float(1)));
    v_out._TESSELLATION_EVALUATION_STAGE_OUT_1 = POS;
    v_out._TESSELLATION_EVALUATION_STAGE_OUT_2 = gl_TessCoord;
    vec4 g_GEXPR0_2341 = g_PROG1_TMP2340;
    gl_Position = g_GEXPR0_2341;
    return;
}

"




"// geometry-stage
#version 410

layout (triangles) in;

in _FROM_TESSELLATION_EVALUATION_STAGE_
{
     in vec3 _TESSELLATION_EVALUATION_STAGE_OUT_1;
     in vec3 _TESSELLATION_EVALUATION_STAGE_OUT_2;
} v_in[3];

layout (triangle_strip, max_vertices = 3) out;

out _FROM_GEOMETRY_STAGE_
{
     out vec3 _GEOMETRY_STAGE_OUT_0;
     out vec3 _GEOMETRY_STAGE_OUT_1;
     out vec3 _GEOMETRY_STAGE_OUT_2;
} v_out;

uniform float TESS_LEVEL_INNER;
uniform float TESS_LEVEL_OUTER;
uniform mat4 PROJECTION;
uniform mat4 MODEL_TO_CLIP;
uniform mat3 NORMAL_MAT;
uniform vec3 LIGHT_POSITION;
uniform vec3 DIFFUSE_MATERIAL;
uniform vec3 AMBIENT_MATERIAL;

void main()
{
    vec3 A = (v_in[2]._TESSELLATION_EVALUATION_STAGE_OUT_1 - v_in[0]._TESSELLATION_EVALUATION_STAGE_OUT_1);
    vec3 B = (v_in[2]._TESSELLATION_EVALUATION_STAGE_OUT_1 - v_in[0]._TESSELLATION_EVALUATION_STAGE_OUT_1);
    vec3 FACET_NORMAL = (NORMAL_MAT * normalize(cross(A,B)));
    gl_Position = gl_in[0].gl_Position;
    v_out._GEOMETRY_STAGE_OUT_0 = v_in[0]._TESSELLATION_EVALUATION_STAGE_OUT_2;
    v_out._GEOMETRY_STAGE_OUT_1 = FACET_NORMAL;
    v_out._GEOMETRY_STAGE_OUT_2 = vec3(float(1),float(0),float(0));
    EmitVertex();
    gl_Position = gl_in[1].gl_Position;
    v_out._GEOMETRY_STAGE_OUT_0 = v_in[1]._TESSELLATION_EVALUATION_STAGE_OUT_2;
    v_out._GEOMETRY_STAGE_OUT_1 = FACET_NORMAL;
    v_out._GEOMETRY_STAGE_OUT_2 = vec3(float(0),float(1),float(0));
    EmitVertex();
    gl_Position = gl_in[2].gl_Position;
    v_out._GEOMETRY_STAGE_OUT_0 = v_in[2]._TESSELLATION_EVALUATION_STAGE_OUT_2;
    v_out._GEOMETRY_STAGE_OUT_1 = FACET_NORMAL;
    v_out._GEOMETRY_STAGE_OUT_2 = vec3(float(0),float(0),float(1));
    EmitVertex();
    EndPrimitive();
    return;
}

"




"// fragment-stage
#version 410

in _FROM_GEOMETRY_STAGE_
{
     in vec3 _GEOMETRY_STAGE_OUT_0;
     in vec3 _GEOMETRY_STAGE_OUT_1;
     in vec3 _GEOMETRY_STAGE_OUT_2;
} v_in;

layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

uniform float TESS_LEVEL_INNER;
uniform float TESS_LEVEL_OUTER;
uniform mat4 PROJECTION;
uniform mat4 MODEL_TO_CLIP;
uniform mat3 NORMAL_MAT;
uniform vec3 LIGHT_POSITION;
uniform vec3 DIFFUSE_MATERIAL;
uniform vec3 AMBIENT_MATERIAL;

float AMPLIFY(float D, float SCALE, float OFFSET);

float AMPLIFY(float D, float SCALE, float OFFSET)
{
    float D0 = ((SCALE * D) + OFFSET);
    float D1 = clamp(D0,float(0),float(1));
    return (1 - pow((D1 * (D1 * -2)), float(2)));
}

void main()
{
    vec3 LIGHT_POSITION0 = vec3(float(0),float(1),float(0));
    vec3 DIFFUSE_MATERIAL0 = vec3(float(1),float(0),float(0));
    vec3 AMBIENT_MATERIAL0 = vec3(0.2f,0.2f,0.2f);
    vec3 N = normalize(v_in._GEOMETRY_STAGE_OUT_1);
    vec3 L = LIGHT_POSITION0;
    float DF = abs(dot(N,L));
    vec3 COLOR = (AMBIENT_MATERIAL0 + (DF * DIFFUSE_MATERIAL0));
    float D10 = min(min(v_in._GEOMETRY_STAGE_OUT_2.x,v_in._GEOMETRY_STAGE_OUT_2.y),v_in._GEOMETRY_STAGE_OUT_2.z);
    float D2 = min(min(v_in._GEOMETRY_STAGE_OUT_0.x,v_in._GEOMETRY_STAGE_OUT_0.y),v_in._GEOMETRY_STAGE_OUT_0.z);
    vec3 COLOR0 = ((AMBIENT_MATERIAL0 + (DF * DIFFUSE_MATERIAL0)) * (AMPLIFY(D10,float(40),-0.5f) * AMPLIFY(D2,float(50),-0.5f)));
    vec4 g_GEXPR0_2342 = vec4(COLOR0,float(1));
    _FRAGMENT_STAGE_OUT_0 = g_GEXPR0_2342;
    return;
}

"
