# Using the Compiler

GLSL programs are made of [stage](https://www.khronos.org/opengl/wiki/Shader#Stages)s. Varjo can compile stages both individually or as a 'pipeline'.

- To compile an individual stage you use the [translate](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ATRANSLATE) function
- To compile multiple stages as a pipeline you use the [rolling-translate](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3AROLLING-TRANSLATE) function

When using `rolling-translate` Varjo will check the data flowing from stage to stage, this can help catch type mismatches and other stage specific issues.

`translate` will produce a [compiled-stage](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ACOMPILED-STAGE) object, `rolling-translate` will produce a list of `compiled-stage`s

## Making a Stage

Stages are made using the [make-stage](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3AMAKE-STAGE) function.

Vari supports all of the kinds of GLSL stages. When calling make-stage the stage kind is passed one of the following keywords:

- `:vertex`
- `:tessellation-control`
- `:tessellation-evaluation`
- `:geometry`
- `:fragment`
- `:compute`

Input & uniform variables are provided as separate lists, along with a list containing the keyword name of the GLSL version and a list containing the forms that make up the body of the stage (as in a `progn`).

### Input-Variables

These are the parameters to the stage which change per element that is being processed, whether that be vertex, fragment, patch, etc.

GLSL does not allow an input variable to a shader stage to be a struct. We found this annoying so Vari allows it and will break down the input structs as you would have to do usually.

The input variables are defined as lists in the following fashion:

    `(variable-name vari-type &rest optional-qualifiers "explicit-glsl-name")

Usually this looks like:

    (vert :vec4)
    (data lighting-data)
    (length :int :flat) <- flat is the qualifier in this case

If the last element of the list is a string, then that string is used as the name of the variable in the compiled GLSL. This is very rarely used and should be avoided.

Here is a simple example showing some inputs:

    TESTS> (glsl-code
            (translate
             (make-stage :fragment '((a :int :flat)
                                     (b :float))
                         nil
                         '(:450)
                         '((vec4 (+ a b))))))
    "// fragment-stage
    #version 450

    in _IN_BLOCK_
    {
         flat in int A;
         in float B;
    } v_in;

    layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

    void main()
    {
        _FRAGMENT_STAGE_OUT_0 = vec4((v_in.A + v_in.B));
        return;
    }
    "

Varjo used an interface block to group of the inputs. When [rolling-translate](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3AROLLING-TRANSLATE) is used these blocks will be named appropriately so that the GLSL can be compiled as a single program.

### Uniforms

These are the parameters that stay the same (are uniform) for the duration of the pipeline. The format is the same as for input-variables, however there are a few more qualifiers of note:

#### :ssbo

This tells Varjo that the data will be coming from an SSBO. The type of this parameter must be a struct. You can write to the slots of this uniform.

#### :ubo

This tells Varjo that the data will be coming from an SSBO. The type of this parameter must be a struct. This is read only

#### :packed / :std-140 / :std-430

These are only valid if the type is a struct. They tell Varjo the data layout of the data, more details can be found [on the GLWiki here](https://www.khronos.org/opengl/wiki/Interface_Block_(GLSL)#Memory_layout)

### Stage Outputs

In regular GLSL you have to define the names of the output variables in your stage (and qualify them with `out`) in Vari we take the more lisp like approach of using the values returned from the body of the stage. If you wish to pass multiple values onto the next stage you can use the `values` just like in regular Common Lisp. One notable variation from Common Lisp's `values` form is that we allow you to add qualifiers to the value as follows:

    TESTS> (glsl-code
            (translate
             (make-stage :vertex '((a :int)
                                   (b :float))
                         nil
                         '(:450)
                         '((let ((c (* a 2)))
                             (values
                              (vec4 (+ a b))
                              (:flat c))))))) ;;<<<< a qualifier in the values form
    "// vertex-stage
    #version 450

    layout(location = 0)  in int A;
    layout(location = 1)  in float B;

    out _FROM_VERTEX_STAGE_
    {
         flat out int _VERTEX_STAGE_OUT_1;  <<<<< The output value with the 'flat' qualifier
    } v_out;

    void main()
    {
        int C = (A * 2);
        vec4 g_PROG1_TMP815 = vec4((A + B));
        v_out._VERTEX_STAGE_OUT_1 = C;
        vec4 g_GEXPR0_816 = g_PROG1_TMP815;
        gl_Position = g_GEXPR0_816;
        return;
    }
    "

You can also use `return` but naturally the types returned from each `return` clause must match.

For the `geometry` stage, which returns no values you must return `:void` which is `(values)` in Vari, and use `emit` & `end-primitive` to create the geometry that will be passed onto the next stage.

The `compute` stage also has no next stage, so you are required to return `(values)`

`discard` is also supported, this stops the execution for that element without returning any values.

### Qualifiers

Theoretically we support the following qualifiers, however this is one areas where Varjo is incredibly weak, you can track this issue [here](https://github.com/cbaggers/varjo/issues/179)

- `:flat`
- `:smooth`
- `:noperspective`
- `:centroid`
- `:coherent`
- `:const`
- `:invariant`
- `:readonly`
- `:restrict`
- `:sample`
- `:shared`
- `:volatile`
- `:writeonly`

### User Defined Structs & Stage Parameters

We also allow you to pass UBO variables & SSBO variables to functions, usually this is not allowed as they are represented as interface blocks, Varjo will modify the code as required to make this work.

### Stage Specifics

More details on the stages can be found [here](stage-specifics.md)

## Using the Result

When you have your `compiled-stage` (or list of them) you will want to get the GLSL from them. This is done using the [glsl-code](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3AGLSL-CODE) function. Given a `compiled-stage` it will return a string containing the GLSL. Given a list of `compiled-stage`s it will return a list of strings containing the GLSL.

## Defining Functions

Whilst Vari does support local functions, it is nice to be able to share code between stages; to this end we have a few ways of defining Vari functions outside of a stage.

### define-vari-function

The first is [define-vari-function](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ADEFINE-VARI-FUNCTION), it let's you define a function with the body being written in Vari. For example:

    (define-vari-function test ((x :float))
      (* x x))

After this is compiled you can write calls such as `(test 3.0)` in any stage as the function will be included in the glsl automatically.

Please note that, other than checking the types of the parameters, Vari will do no checks on the validity of the code. Doing so would requires knowing the context that the function would be used in (e.g. which stage, which version etc). The first time you will know if the code was valid is when it is called from a stage being compiled.

If the lack of checking is an issue (as it would be in CEPL) it would be advisable to have your code use the [test-translate-function-split-details](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ATEST-TRANSLATE-FUNCTION-SPLIT-DETAILS) function. This will make a dummy stage an compile it in order to find potential issues. To test the above `test` function we would do the following:

    (varjo:test-translate-function-split-details
      'test '((x :float)) nil '(:450) '((* x x)))

See the reference docs for [test-translate-function-split-details](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ATEST-TRANSLATE-FUNCTION-SPLIT-DETAILS) for what each parameter is for.

If you are wrapping Varjo (which is the most likely usecase) the rather than using `define-vari-function` itself, you can simply use the function call it expands to.

    (add-external-function 'test '((x :float)) nil '((* x x)))

Again see the reference docs for [add-external-function](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3AADD-EXTERNAL-FUNCTION) to see how this should be used.

In short however, `test-translate-function-split-details` & `add-external-function` together allow you to make a more robust experience for your users than `define-vari-function` provides.

### define-glsl-template-function

This is used to define a GLSL expression as a function. For example

    (define-glsl-template-fun foo ((x :int)) :int
      "foo(~a)"
      :pure t)

See [here](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ADEFINE-GLSL-TEMPLATE-FUNCTION) for the info on how to use this and it's restrictions.

### A quick note on v-def-glsl-template-fun and v-defun

`v-defun` & `v-def-glsl-template-fun` are macros that also is exported from Varjo. They are used heavily in a number of my projects and are older, uglier versions of `define-glsl-template-fun` and `define-vari-function`.

They are not removed as I don't want to break existing projects but use of the newer versions is preferable.

## User Defined Types

Unlike GLSL, Vari lets you define types outside of stages, further assisting code reuse. There are two ways to define new types for Vari, [define-vari-struct](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ADEFINE-VARI-STRUCT) & [define-vari-type](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API%3ADEFINE-VARI-TYPE).

### define-vari-struct

This is the most commonly used of the two constructs. A struct is defined as follows:

    ;;              [0]↓
    (define-vari-struct some-struct () ; ←[1]
      (near :float :accessor near) ; ←[2]
      (far :float :accessor far)
      (diff :float :accessor diff))

`[0]` - The name of struct: The name can be any non-keyword symbol;

`[1]` - Optional context information;

`[2]` - A slot definition;

A slot definition takes the form: `(slot-name slot-type)`

Or optionally: `(slot-name slot-type :accessor accessor-name)`

As with regular lisp structs, `define-vari-struct` will create a number of `structname-slotname` accessor functions for the struct.  For the above example, we would get `#'some-struct-near`, `#'some-struct-far` & `#'some-struct-diff`.

As these names can be fustratingly long, the optional `:accessor` field may be used to specify a more favorable name. In Common Lisp, class 'accessors' are methods and such are subject to dynamic dispatch based on the argument types. However, as Vari is statically typed and supports function overloading we do not have this limitation.

#### with-slots on structs

In a departure from Common Lisp, using `with-slots` on a struct is defined. Other than this it used as it would be in Common Lisp.


### define-vari-type

This is the much less used option of the two, but it allows you to define a type that will be represented in the glsl as an existing core GLSL type. For example let's define our own 'complex' type (`complex` is already available in Vari but this is a neat exercise).

    (define-vari-type my-complex () :vec2)

here we have said that `my-complex` is a type that will be represented by a vec2 in glsl. Even though it will be compiled to a vec2, it is not one in Vari and cannot be cast to one without defining new functions to do that.

At this moment we cannot even create one. Let's make a constructor:

    (define-glsl-template-fun my-complex ((r :float) (i :float)) my-complex
      "vec2(~a, ~a)"
      :pure t)

Now the `my-complex` function will make an instance of the `my-complex` type when called with 2 `:float`s. Now let's define some accessors:

    (define-glsl-template-fun real-part ((r my-complex)) :float
      "~a.x"
      :pure t)

    (define-glsl-template-fun imag-part ((r my-complex)) :float
      "~a.y"
      :pure t)

And then we can see how it looks when used in a stage:


    TESTS> (glsl-code
            (translate
             (make-stage
              :fragment nil nil '(:450)
              '((let ((a (my-complex 1 2)))
                  (vec4 (imag-part a)))))))

    "// fragment-stage
    #version 450

    layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

    void main()
    {
        vec2 A = vec2(float(1), float(2));
        vec4 g_GEXPR0_788 = vec4(A.y);
        _FRAGMENT_STAGE_OUT_0 = g_GEXPR0_788;
        return;
    }
    "

Just with this we already have enough that we could go back to using `define-vari-function` and write functions to work with our new type.

### A quick note of v-defstruct

As with `v-defun` there is a `v-defstruct` macro which is still commonly used. I'm trying to move to more consistent naming as we prepare to leave beta and so I advise preferring `define-vari-struct` over `v-defstruct`. It gives you nicer highlighting when using slime and will stay more consistent with the rest of public api as I update them.

## Extensions

We allow you to enable (or disable) GLSL extensions by passing them to Varjo as a sublist of the `context` argument of `make-stage`.
While all keywords in the `context` list are interpreted as version specifiers, the last list whose first element is `:extensions` is used to enable or disable extensions.

To enable extensions you can simply add the name of the extension as a string like this:

    '(:400 (:extensions "GL_ARB_separate_shader_objects"))

To specifiy a specific behavior for an extension you can use a list containing the name of the extension as its first element and the behavior as a keyword as the second element:

    '(:400 (:extensions ("GL_ARB_shading_language_420pack" :require)))

Allowed behaviors are `:enable`, `:disable`, `:warn` and `:require`.

This is how extensions look when used in a stage:

    TESTS> (glsl-code
            (translate
             (make-stage :fragment '((color :vec4))
                         nil
                         '(:400
                           (:extensions
                            "GL_ARB_separate_shader_objects"
                            ("GL_ARB_shading_language_420pack" :require)))  
                         '(color))))
                         
    "// fragment-stage
    #version 400

    #extension GL_ARB_separate_shader_objects : enable
    #extension GL_ARB_shading_language_420pack : require

    in _IN_BLOCK_
    {
         in vec4 COLOR;
    } v_in;

    layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

    void main()
    {
        _FRAGMENT_STAGE_OUT_0 = v_in.COLOR;
        return;
    }

    "

## Experimental Vulkan Support

Varjo offers experimental support for Vulkan (or SPIR-V).
Currently only new qualifiers and types added for Vulkan are supported, but new GLSL functions, variables and shader stages (ray tracing and mesh shaders) are still missing.

To enable Vulkan specific features you need to pass the target environment `:vulkan` to `make-stage` within the `context` argument.
E.g. like this:

    '(:440 (:target-environment :vulkan))

This will add `location` qualifiers to members of interface blocks (e.g. `v_out`), since SPIR-V matches in/out variable only by their `location` and not their names.
Because GLSL versions < 4.40 don't support `location` qualifiers for interface blocks, you must specifiy at least version `:440` when targetting Vulkan.
E.g.:

    TESTS> (glsl-code
            (translate
             (make-stage :fragment '((color :vec4))
                         nil
                         '(:440
                           (:target-environment :vulkan))  
                         '(color))))
    "// fragment-stage
    #version 440

    in _IN_BLOCK_
    {
        layout(location = 0)  in vec4 COLOR;
    } v_in;

    layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

    void main()
    {
        _FRAGMENT_STAGE_OUT_0 = v_in.COLOR;
        return;
    }
    "

### Descriptor Sets

To specify the descriptor set and binding (within the set) for a uniform, you can pass them as extra qualifiers (`:set` and `:binding`) to the uniform definition.
Since both qualifiers require a value the qualifiers must be specified as lists where the first element is the qualifier and the second one is the value.
E.g.:

    '((some-buffer my-uniform-buffer :ubo :std-140 (:set 0) (:binding 0)))

Used in a vertex shader:

    TESTS> (define-vari-struct uniform-buffer ()
             (mvpc :mat4))
    UNIFORM-BUFFER
    TESTS> (glsl-code
            (translate
             (make-stage :vertex '((pos :vec4)
                                   (color :vec4))
                         '((uniform-buffer uniform-buffer :ubo :std-140 (:set 0) (:binding 0)))
                         '(:440
                           (:target-environment :vulkan))
                         '((with-slots (mvpc) uniform-buffer
                             (values
                              (* mvpc pos)
                              color))))))
    "// vertex-stage
    #version 440

    layout(location = 0)  in vec4 POS;
    layout(location = 1)  in vec4 COLOR;

    out _FROM_VERTEX_STAGE_
    {
        layout(location = 0)  out vec4 _VERTEX_STAGE_OUT_1;
    } v_out;

    layout(std140, set = 0, binding = 0) uniform _UBO_UNIFORM_BUFFER
    {
        mat4 MVPC;
    } UNIFORM_BUFFER;

    void main()
    {
        vec4 g_PROG1_TMP1531 = (UNIFORM_BUFFER.MVPC * POS);
        v_out._VERTEX_STAGE_OUT_1 = COLOR;
        vec4 g_GEXPR0_1532 = g_PROG1_TMP1531;
        gl_Position = g_GEXPR0_1532;
        return;
    }
    "

### Push Constants

UBOs can be specified specified as push constants using the `:push-constant` qualifier.
Like for other UBOs the default layout for push constants is `:std-140`:

    TESTS> (glsl-code
            (translate
             (make-stage :vertex '((pos :vec4)
                                   (color :vec4))
                         '((uniform-buffer uniform-buffer :ubo :push-constant))
                         '(:440
                           (:target-environment :vulkan))
                         '((with-slots (mvpc) uniform-buffer
                             (values
                              (* mvpc pos)
                              color))))))
    "// vertex-stage
    #version 440

    layout(location = 0)  in vec4 POS;
    layout(location = 1)  in vec4 COLOR;

    out _FROM_VERTEX_STAGE_
    {
        layout(location = 0)  out vec4 _VERTEX_STAGE_OUT_1;
    } v_out;

    layout(std140, push_constant) uniform _UBO_UNIFORM_BUFFER
    {
        mat4 MVPC;
    } UNIFORM_BUFFER;

    void main()
    {
        vec4 g_PROG1_TMP1556 = (UNIFORM_BUFFER.MVPC * POS);
        v_out._VERTEX_STAGE_OUT_1 = COLOR;
        vec4 g_GEXPR0_1557 = g_PROG1_TMP1556;
        gl_Position = g_GEXPR0_1557;
        return;
    }
    "

### Textures & Samplers

Vulkan allows splitting textures and samplers by introducting the new sampler types `:sampler` and `:sampler-shadows` which can be used to sample different texture types (e.g. `:itexture-2d`).

However, the [glsl-spec](https://github.com/cbaggers/glsl-spec) doesn't yet specify the functions needed to construct a texture sampler from a sampler and a texture (e.g. `sampler2D(texture, sampler)`, so you'll have to use combined texture samplers (e.g. `:sampler-2d`) for now.

### Subpass Inputs

Subpass inputs require `input_attachment_index` to be set. You can specify this qualifier like `set` or `binding`:

    '((albedo :subpass-input (:input-attachment-index 1) (:set 0) (:binding 1))

Since the GLSL functions for reading subpass inputs (i.e. `subpassLoad`) are not yet specified in [glsl-spec](https://github.com/cbaggers/glsl-spec), they are pretty useless at the moment.

### Vulkan Types

The following new types are allowed when targetting Vulkan:

- `:sampler`
- `:sampler-shadow`
- `:subpass-input`
- `:subpass-input-ms`
- `:isubpass-input`
- `:isubpass-input-ms`
- `:usubpass-input`
- `:usubpass-input-ms`
- `:texture-1d`
- `:texture-1d-array`
- `:texture-2d`
- `:texture-2d-array`
- `:texture-2d-ms`
- `:texture-2d-ms-array`
- `:texture-2d-rect`
- `:texture-3d`
- `:texture-buffer`
- `:texture-cube`
- `:texture-cube-array`
- `:itexture-1d`
- `:itexture-1d-array`
- `:itexture-2d`
- `:itexture-2d-array`
- `:itexture-2d-ms`
- `:itexture-2d-ms-array`
- `:itexture-2d-rect`
- `:itexture-3d`
- `:itexture-buffer`
- `:itexture-cube`
- `:itexture-cube-array`
- `:utexture-1d`
- `:utexture-1d-array`
- `:utexture-2d`
- `:utexture-2d-array`
- `:utexture-2d-ms`
- `:utexture-2d-ms-array`
- `:utexture-2d-rect`
- `:utexture-3d`
- `:utexture-buffer`
- `:utexture-cube`
- `:utexture-cube-array`
