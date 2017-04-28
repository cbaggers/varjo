Primitives and the Pipeline

Ok so the overall goal is to add lispy support geometry shaders. This requires
handling the inputs & outputs which are arrayed versions of the previous stage.

They are arrayed to the 'length' of the primitive, so we need to have concept
of the primitives used in the stages.

What is tricky is that geometry and tessellation-eval stages both have different
allowed input & output primitives and it's not always clear how these map.

Also there is the concept of draw-mode which is specified in GL when calling
draw-arrays or draw-elements. This again has overlapping but different choices.

I really like Varjo passing/checking data between stages and I also want this
for primitives.

Let's just start running through what we have in different points in the
pipeline.

-- Draw Modes --

We have a few options here:

    points
    line-strip, line-strip-adjacency, line-loop lines, lines-adjacency
    triangle-strip, triangle-strip-adjacency, triangle-fan, triangles,
    triangles-adjacency
    quads (not in core)
    patches (I have no idea why this is a draw-mode, isn't this only
    tessellation related?)

These modes tell the gpu about what it will be drawing and, in the case
of adjacency, something of the relationship between vertices.

We can see groupings in the above which may help later

-- Vertex Shader --

Very little of interest (from a primitive perspective) happens here.
The next programmable stage after this will be either: tessellation, geometry or
fragment.

The fragment stage isn't concerned with geometry so can be ignored for now.

-- Tessellation --

Although we aren't focusing on this yet we will need to eventually so let's
consider it.

    Control Stage

Takes a 'patch' in.. we will have to look into this

Unless transform feedback is support (which for now it is not) then the only
output of tessellation-control shaders is via the 'out' variable.

    Evaluation Stage

primitives in: point-mode, triangles, quads, isolines

The primitives here are specifying how the tessellation engine will split up the
incoming primitives:

    point-mode means every vertex is a point
    triangles are subdivided in smaller triangles
    quads are subdivided into triangles
    isolines means the primitive is a quad but will be split into lines

-- Geometry --

Valid input primitive kinds:

    points
    lines
    lines-adjacency
    triangles
    triangles-adjacency

A much more restricted set. Adjacency here is weird as, if the primitive is from
the tessellation stages does it still have adjacency? I wouldn't have thought so.

So here is something we can sanity check. Also we have 3 categories:

points, lines, triangles

This should be something we can infer in the simple cases. Looking at the list
of draw modes these fall into the camps fairly well. Except quads, which seem
to only be valid up to the tessellation stage, and patches which ¯\_(ツ)_/¯

The allowed output primitives for this stage are:

    points
    line-strip
    triangle-strip

This makes sense. I'm hoping that we will be able to infer the primitive based
on some other function used in the shader body, but I'm not sure quite how yet (perhaps #'emit-primitive).

------------------------------------------------------------

djeis97 commented

Patches are used as a draw primitive so that you can supply a variable
number of verticies per primitive as input to the tessellation
stage. For example, in the tessellation branch of djeis97/bezier I'm
using 4 verticies per patch to draw a bezier curve. The first and last
are the start and end points of the curve, while the two in the middle
are the control points. Then all of those vertices are available at
tessellation The number of verticies per patch is set at
(%gl:patch-parameter-i :patch-vertices 4).

------------------------------------------------------------
