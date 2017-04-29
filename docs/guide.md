# Varjo User Guide

Varjo is a compiler that can transform a typed subset of Common Lisp into GLSL.

This document will explain how to use Varjo. It presumes that you have the basic understanding of Common Lisp and the shader-based approach to modern OpenGL. Varjo is just a compiler; uploading GLSL to the gpu and running the result is outside the scope of this article.

## Compiling Code

Working with Varjo mainly involves using a single function: `#'v-compile`

This function takes lists that describe what needs compiling, and returns an instance of the `compiled-stage` class. That `compiled-stage` object contains the resulting GLSL, as well as the metadata about the compilation task.

As stages in a pipeline are closely related in GL, Varjo can compile them all in one call. At the same time Varjo can check the boundaries between the stages, and make sure the rules around types, names, and other GL restrictions, are held for your code.

_Note_ Varjo's support for some shader stages is very young, and therefore, many GL Spec mandated behaviours are not yet enforced. If you see something that should be added to the boundary checks, please don't hesitate to raise an `enhancement` issue for it on [Varjo's github page](https://github.com/cbaggers/varjo).

The `v-compile` function takes the following arguments:
- a list of the `uniforms` available to all stages being compiled;
- the GLSL version to compile this code for, specified as a keyword;
- one or more of the following key arguments for each desired stage.  Each corresponding value must be a list, where the first element is the list of `in` arguments for that stage, and the rest of the list is the code for that stage.
 - `:vertex`
 - `:tessellation-control`
 - `:tessellation-evaluation`
 - `:geometry`
 - `:fragment`

The elements of the `uniforms` & `in` arguments lists must be follow the `stage argument format` laid out below.

The code to be compiled must follow the format detailed in the `Varjo CL Subset` section below.

`tessellation-evaluation` argument must be used with a `tessellation-control` argument.

Here is an example of a call to `#'v-compile`:

    (v-compile '((a :float)) :330
               :vertex '(((pos :vec3))
                         (values (v! pos 1.0) a))
               :fragment '(((hmm :float))
                           (labels ((fun ((x :float))
                                      (* x x)))
                             (v! 1.0 1.0 hmm (fun a)))))

In the above code:
- `((a :float))` is the uniform argument. In this case, a single uniform variable named `a` with the type `:float` is specified;
- `:330` is the GLSL version we are compiling for;
- `((pos :vec3))` is the `in` argument list for the vertex stage. In this case, the vertex stage has one argument named `pos` with a type of `:vec3`;
- `(values (v! pos 1.0) a)` is the lisp code for the `vertex` stage. Note that as we have specified both the `vertex` & `fragment` stages, Varjo is able to check the multiple;

## Varjo CL Subset

### Design Philosophy

Varjo tries to be pragmatic in its choices. It wants to be as close as possible to CL, whilst being realistic about performance and not decieving its users.

High-end graphics is one of the areas of programming where performance still truly matters; you are not restricted by the resources themselves, but instead, the standards that are expected in the industry at large.

For example, let us assume we wanted provide support for `fill-pointer` in Varjo. Well, we could certainly provide the 'experience' & syntax of using extendable arrays.  However, as GLSL doesnt have an equivalent, we would only be providing an illusion of the feature, and behind the scenes, we would still be creating a new array on every `push`. Now imagine the user is trying to debug a performance issue in their shader code. They are forced to look into the implementation, as their tool has lied to them about what it provides.

Varjo heartily agrees that there is value to such abstractions, but opts to only provide functionality that it can map directly to GLSL (and leave extension & abstraction to the libraries built on top of itself.)

### Context

The `context` in Varjo is one of the messier concepts, and is likely to be cleaned up at some point. However, for now, this is how it works.

Context is analagous to the `*features*` list in lisp, and is seen in two places:
- On definitions of functions & types, to specify what GLSL versions (etc) can use this function/type;
- As an argument to v-compile, to providing the compiler extra metadata about the build;


### Types

Varjo is statically typed and uses basic type inference, in order to minimize the requirement to specify types directly.

#### Built in types

The built in types are the ones that GLSL understands natively. They are written as keywords, as this was the style I saw for primitive foreign types in CFFI.

Please see [./built-in-types.md](./built-in-types.md) for a full list of the built in types.

#### User defined types

Users can define new types by using the `v-defstruct` macro:

    ;;              [0]↓
    (v-defstruct some-struct () ; ←[1]
      (near :float :accessor near) ; ←[2]
      (far :float :accessor far)
      (diff :float :accessor diff))

`[0]` - The name of struct: The name can be any non-keyword symbol;

`[1]` - Optional context information;

`[2]` - A slot definition;

A slot definition takes the form: `(slot-name slot-type)`

Or optionally: `(slot-name slot-type :accessor accessor-name)`

As with regular lisp structs, `v-defstruct` will create a number of `structname-slotname` accessor functions for the struct.  For the above example, we would get `#'some-struct-near`, `#'some-struct-far` & `#'some-struct-diff`.

As these names can be fustratingly long, the optional `:accessor` field may be used to specify a more favorable name. In the case of `defclass` 'accessors' are methods, subject to dynamic dispatch based on the argument types. However, as Varjo is statically typed and supports function overloading, we do not have this limitation.


### Multiple Return Values

Varjo supports multiple return values by transforming them into `out` parameters in GLSL.

### Stage & Function Argument Formats


### Futher Extending Varjo

If you find something missing, or would just like to see how the internal and functions are defined, please see [./futher-extending-varjo.md](./futher-extending-varjo.md) for details.

Also, see the [glsl-spec](http://github.com/cbaggers/glsl-spec) project which Varjo uses to find the definitions on almost all GLSL functions & variables. Contributions there benefit all projects that use it, and are very welcome.
