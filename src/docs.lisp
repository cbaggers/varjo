(in-package :varjo.internals)

;;------------------------------------------------------------

(docs:define-docs
  ;;
  ;; globals
  (defvar *default-version*
    "
`*default-version*` is a variable that holds the version of GLSL that
will be used for a compile task if a version is not specified in the
context
")

  (defvar *stage-names*
    "
`*stage-names*` is a variable that holds the list of keyword stage names that
Varjo recognises
")
  (defvar *stage-type-names*
    "
`*stage-type-names*` is a variable that holds a list of type-names that map to
the stages named in `*stage-names*`
")
  (defvar *supported-versions*
    "
`*supported-versions*` is a variable that holds the list of GLSL version Varjo
ostensibly supports.

Note: That 'ostensibly' is there as I'm very sure that the lower levels of GLSL
varjo is currently doing a very poor job. If you have the time please file
issues that you find at https://github.com/cbaggers/varjo/issues
")
  ;;
  ;; stages
  (defclass vertex-stage
      "
`vertex-stage` is the class that represents a uncompiled vertex stage.

`compiled-vertex-stage` inherits from this class
")
  (defclass tessellation-control-stage
      "
`tessellation-control-stage` is the class that represents a uncompiled
tessellation-control stage.

`compiled-tessellation-control-stage` inherits from this class
")
  (defclass tessellation-evaluation-stage
      "
`tessellation-evaluation-stage` is the class that represents a uncompiled
tessellation-evaluation stage.

`compiled-tessellation-evaluation-stage` inherits from this class
")
  (defclass geometry-stage
      "
`geometry-stage` is the class that represents a uncompiled geometry stage.

`compiled-geometry-stage` inherits from this class
")
  (defclass fragment-stage
      "
`fragment-stage` is the class that represents a uncompiled fragment stage.

`compiled-fragment-stage` inherits from this class
")
  (defclass compute-stage
      "
`compute-stage` is the class that represents a uncompiled compute stage

`compiled-compute-stage` inherits from this class
")

  ;;
  ;; primitives, etc
  (defclass dynamic
      "
`dynamic` is the class that represents the simarlaly
named GL primitive.
")
  (defclass line-loop
      "
`line-loop` is the class that represents the simarlaly
named GL primitive.
")
  (defclass line-strip
      "
`line-strip` is the class that represents the simarlaly
named GL primitive.
")
  (defclass line-strip-adjacency
      "
`line-strip-adjacency` is the class that represents the simarlaly
named GL primitive.
")
  (defclass lines
      "
`lines` is the class that represents the simarlaly
named GL primitive.
")
  (defclass lines-adjacency
      "
`lines-adjacency` is the class that represents the similarly
named GL primitive.
")
  (defclass patches
      "
`patches` is the class that represents the similarly
named GL primitive.
")
  (defclass points
      "
`points` is the class that represents the similarly
named GL primitive.
")
  (defclass quads
      "
`quads` is the class that represents the similarly
named GL primitive.
")
  (defclass triangle-fan
      "
`triangle-fan` is the class that represents the similarly
named GL primitive.
")
  (defclass triangle-strip
      "
`triangle-strip` is the class that represents the similarly
named GL primitive.
")
  (defclass triangle-strip-adjacency
      "
`triangle-strip-adjacency` is the class that represents the similarly
named GL primitive.
")
  (defclass triangles
      "
`triangles` is the class that represents the similarly
named GL primitive.
")
  (defclass triangles-adjacency
      "
`triangles-adjacency` is the class that represents the similarly
named GL primitive.
")
  (defun vertex-count
      "
When passed an instance of `primitive` this function returns the number
of vertices that make up this kind of primitive.
")

  (defun lisp-name
      "
When passed an instance of `primitive` this function returns the keyword
name of the primitive. This is the name one would pass to functions like
`make-stage`.
")

  ;;
  ;; functions/macros
  (defun add-external-function
      "
`add-external-function` is a function that takes the following:

- a name as a symbol
- a list of input parameters declartions in the form:
  `(,name ,type-spec ,@qualifiers)`
- a list of uniforms in the form:
  `(,name ,type-spec ,@qualifiers)`
- a list of forms that make up the body of the function
- [optional] as list of glsl versions this function is valid for.
             nil means it is valid for all versions

After doing basic checks on the arguments the function will be added to the
'external function registry' such that it call be called from any future
compilation task (assuming appropriate glsl versions).

Please note that this function does not attempt to validate the body of the
function as doing so would require knowledge of the context in which it is
used. If you need to test that the function could be valid, refer to the
`test-translate-function-split-details` function.
")
  (defun delete-external-function
      "
`delete-external-function` removes a function from the 'external function
registry' such that any attempt to use it in a functure compilation task will
fail.

The 'in-arg-types' parameter expects a list of Vari `type-spec`s.
")
  (defmacro define-glsl-template-function
      "
`define-glsl-template-function` which lets you define a snippet of
glsl code as a function in Vari. Here is an example definition:

                                   [0]
                                    ↓
    (define-glsl-template-function my-func ((x :float)) ←[1]
      :float ←[2]
      \"(1.0f / ~a)\" ←[3]
      :pure t) ←[4]

`[0]` Here we are defining a function called 'my-func'

`[1]` It takes 1 argument, called 'x' which has the type `:float`

`[2]` It returns a single `:float` value

`[3]` The string pattern is what will be injected into the final glsl. It
      behaves as though this pattern and the glsl for the arguments were
      passed to `format`, e.g. `(format nil \"(1.0f / ~a)\" \"2.3f\") assuming
      the glsl of the argument passed to `x` was \"2.3f\".

      The glsl for the arguments is passed to format in the order specified
      in `[1]`

      As it currently uses `format` it is legal to use other `directives` such
      as `~s` but it is not advisable as this will result in invalid glsl.
")
  (defmacro define-vari-compiler-macro
      "
`define-vari-compiler-macro` lets you define a compiler-macro that works in
Vari.

The parameters in the lambda-list must be defined in form `(,name ,type-spec)`

The compiler macro will only expand for functions with an exactly matching name
and lambda-list.

Vari's compiler macros behave the same as regular Common Lisp compiler-macros
with the exception that you can make queries against the environment to find
data (such as type) from the arguments (See [here]() for details).

Due to having to compile the argument forms before being able to identify the
compiler macro, any form returned from the compiler-macro (other than the value
from `&whole` is going to result in recompilation. This make Vari's compiler
macros more 'expensive' than Common Lisps in the general sense.
")
  (defmacro define-vari-function
      "
`define-vari-function` lets you define a function that can be used in
any Vari code compiled in the future.

that takes the following:

- a name as a symbol
- a 'vari function lambda-list' (see [here]() for details)
- a body made of Vari code

Please note that, other than performing some basic validation on the arguments,
this function does not attempt any validation on the body of the function.
Doing so would require knowledge of the context in which it is used.

 If you need to test that the function could be valid, refer to the
`test-translate-function-split-details` & `add-external-function` functions.
")
  (defmacro define-vari-macro
      "
`define-vari-macro` lets you define a macro that can be used in any Vari code
compiled in the future.

Vari's macros behave the same as regular Common Lisp macros with the
exception that you can make queries against the environment to find
data (such as type) from the arguments (See [here]() for details).
")
  (defmacro v-def-glsl-template-fun
      "
`v-def-glsl-template-fun` is the older syntax for `define-glsl-template-function`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defmacro v-define-compiler-macro
      "
`v-define-compiler-macro` is the older syntax for `define-vari-compiler-macro`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defmacro v-defmacro
      "
`v-defmacro` is the older syntax for `define-vari-macro`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defmacro v-defstruct
      "
`v-defstruct` is the older syntax for `define-vari-struct`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defmacro v-defun
      "
`v-defun` is the older syntax for `define-vari-function`.

This macro is still exported in order to maintain compatibility with existing
code.
")

  ;;
  ;; types
  (defun add-alternate-type-name
      "
`add-alternate-type-name`
")
  (defun add-equivalent-name
      "
`add-equivalent-name`
")

  (defmacro define-alternate-type-name
      "
`define-alternate-type-name` lets you define
")
  (defmacro define-shadow-type-constructor
      "
`define-shadow-type-constructor` lets you define
")
  (defmacro define-metadata-infer
      "
`define-metadata-infer` lets you define
")
  (defmacro define-metadata-kind
      "
`define-metadata-kind` lets you define
")
  (defmacro define-vari-struct
      "
`define-vari-struct` lets you define
")
  (defmacro define-vari-type
      "
`define-vari-type` lets you define
")
  (defmacro def-shadow-type-constructor
      "
`def-shadow-type-constructor` is the older syntax for
`define-shadow-type-constructor`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defmacro def-metadata-infer
      "
`def-metadata-infer` is the older syntax for `define-metadata-infer`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defmacro def-metadata-kind
      "
`def-metadata-kind` is the older syntax for `define-metadata-kind`.

This macro is still exported in order to maintain compatibility with existing
code.
")

  (defun core-typep
      "
`core-typep` returns T if the Varjo type object provided is a core GLSL type
")
  (defun combine-metadata
      "
`combine-metadata` takes 2 metadata (or null) objects and attempts to combine
them if possible. It is valid for the system to return nil for non-null
arguments.
")
  (defun ephemeral-p
      "
`ephemeral-p` returns T is the Varjo type object represents an `ephemeral` type
")
  (defun feedback-group
      "
`feedback-group` given an instance of `feedback-qualifier` this function
returns the group ID from the qualifier.
")
  (defclass feedback-qualifier
      "
`feedback-qualifier` is the class that holds data specified in a transform-
feedback qualifier.
")
  (defun find-mutual-cast-type
      "
`find-mutual-cast-type` when given a number of Varjo type objects, this function
will return a type object that they can all mutually be cast to according to
GLSL's rules.
")
  (defun make-type-set
      "
`make-type-set` takes Varjo type objects or `type-spec`s and returns an objects
that respresents a set of types. `type-set`s are used by a number of internal
Varjo functions.
")
  (defun type->type-spec
      "
`type->type-spec` takes a Varjo type object and returns a Vari `type-spec`.

As (during compilation) type objects can carry metadata that may not be
representable in a `type-spec` this conversion can be 'lossy'.
")
  (defun type-spec->type
      "
`type-spec->type` takes a Vari `type-spec` and returns a new instance of
Varjo type object that represents it.
")
  (defun type-specp
      "
`type-specp` returns T if the `type-spec` provided represents valid Vari type.
")

  (defun v-casts-to
      "
The poorly names `v-casts-to` takes 2 Varjo type objects and attempts to cast
one to the other. If successful it returns the new type object and otherwise
returns NIL.
")
  (defun v-casts-to-p
      "
`v-casts-to-p` returns T if the first Varjo type object could be cast to the second
")
  (defun v-deftype
      "
`v-deftype` is the older syntax for `define-vari-type`.

This macro is still exported in order to maintain compatibility with existing
code.
")
  (defun v-dimensions
      "
`v-dimensions` given an instance of the `v-array` type object this returns the
list of it's dimensions.
")
  (defun v-element-type
      "
`v-element-type` given an instance of the `v-container` type object this returns the
type object of it elements.
")
  (defun v-errorp
      "
`v-errorp` returns T if the provided objects is of type `v-error`
")
  (defun v-special-functionp
      "
`v-special-functionp` returns T if the Varjo function object provided is a
special form.
")
  (defun v-type-eq
      "
`v-type-eq` returns T if the Varjo type object provided represents the given
Vari type.

The second argument can be a Varjo type object or a Vari `type-spec`
")
  (defun v-typep
      "
`v-typep` returns T if the Varjo type object provided represents the given
Vari type or a subtype of that type.

The second argument can be a Varjo type object or a Vari `type-spec`
")

  (defclass qualifier
      "
`qualifier` is the class that other classes subclass in order to represent
various GLSL qualifiers.
")

  ;;
  ;; stages
  (defun make-stage
      "
`make-stage` is the function that create an object that can be passed to the
`translate` function. It takes the following arguments:

### kind

This must be one of the keywords in *stage-names*:

- :vertex
- :tessellation-control
- :tessellation-evaluation
- :geometry
- :fragment
- :compute

### in-args

Must be a list of input parameters to the stage. These are the values to which change
per element that is being processed, whether that be per vertex, fragment, patch,
instance, etc.

The input variables are defined as lists in the following fashion:

    (variable-name
     type-spec
     &rest optional-qualifier-designators [\"explicit-glsl-name\"])

Usually this looks like:

    (vert :vec4)
    (data lighting-data)
    (length :int :flat) <- flat is the qualifier in this case

If the last element of the list is a string, then that string is used as the name of the variable in the compiled GLSL. This is very rarely used and should generally be avoided.

### uniforms

This is a list of the uniforms to stage. These are the values that stay the same (are
uniform) for the duration of the pipeline.

They are defined in the same way as the above 'in-args' except that, if the type-spec
specified a struct type the following additional qualifiers are allowed:

- `:ssbo`
- `:ubo`
- `:std-140`
- `:std-430`

The result of `make-stage` is an instance of one of the subclasses of the
`stage` type.

### context

The context argument must be a list that may contain any number of symbols from
*supported-versions*. Context is used to specify the GLSL version to compile the
stage.

NOTE: The name 'context' is legacy at this point as it is only used to specify
GLSL versions.

### code

This must be a list containing the forms that make up the 'body' of the stage.

It is not legal for this argument to be nil.

### stemcells-allowed (optional)

If this argument is not NIL then the compiler will allow the capture of globally
scoped variables from Common Lisp and use of `add-lisp-form-as-uniform` from
within macros. For details on how to support 'global variable capture' please see
the documentation for `with-stemcell-infer-hook` & `with-constant-inject-hook`

### primitive (optional)

This can be nil, a instance of the type 'primitive or a valid 'primitive
designator' as specified by the `valid-primitive-name-p` function.

Primitive designators are either one of the following keywords..

- `:dynamic`
- `:points`
- `:lines`
- `:iso-lines`
- `:line-loop`
- `:line-strip`
- `:lines-adjacency`
- `:line-strip-adjacency`
- `:triangles`
- `:triangle-fan`
- `:triangle-strip`
- `:triangles-adjacency`
- `:triangle-strip-adjacency`
- `:quads`

.. or a list whos first element is `:patches` and the second (and final) element
is a positive integer that is greater than 1. This specifies the length of
the patch.

Whilst this is optional in `make-stage` it must be set before being passed to
translate if the stage kind is one of the following:

- tessellation-control-stage
- tessellation-evaluation-stage
- geometry-stage

*Or* if it the first stage in the list of stages passed to `rolling-translate`
and if some of the other stages are of the above kinds.
")
  (defun lisp-code
      "
When passed an instance of `stage`, `lisp-code` returns the Vari code that
acts as the body of the stage.
")
  (defun input-variables
      "
When passed an instance of `stage` this function returns a list of
`input-variable` objects which respresent the input paramaters to
the stage
")
  (defun uniform-variables
      "
When passed an instance of `stage` this function returns a list of
`uniform-variable` objects which respresent the uniform paramaters to
the stage
")
  (defun context
      "
When passed an instance of `stage` this function returns a list of
GLSL versions which this stage was/is to be compiled with
")
  (defun stemcells-allowed
      "
When passed an instance of `stage` this function returns a `generalized-boolean`

If this argument is not NIL then the compiler will allow the capture
of globally scoped variables from Common Lisp and use of `add-lisp-form-as-uniform`
from within macros.

For details on how to support 'global variable capture' please see the documentation
for `with-stemcell-infer-hook` & `with-constant-inject-hook`
")
  (defun primitive-in
      "
When passed an instance of `stage` this function returns an instance of `primitive`
or nil.
")

  (defclass input-variable
      "
`input-variable` is a class which holds data about the input parameters to the stage

You can call the following functions on it to retrieve the data:

- `name`
- `v-type-of`
- `glsl-name`
")
  (defclass uniform-variable
      "
`uniform-variable` is a class which holds data about the uniform parameters to the stage

You can call the following functions on it to retrieve the data:

- `name`
- `v-type-of`
- `glsl-name`
")
  (defclass implicit-uniform-variable
      "
`implicit-uniform-variable` is a class which holds data about the uniforms that added
implicitly (whether by globally scoped variable capture or by use of
`add-lisp-form-as-uniform`)

You can call the following functions on it to retrieve the data:

- `name`
- `v-type-of`
- `glsl-name`
")
  ;;
  ;; compilation
  (defun translate
      "
When called with an instance of `stage` `translate` compiles it, returning
an instance of `compiled-stage`
"
    )
  (defun rolling-translate
      "
When called with a list of `stage` objects, `rolling-translate` compiles
them returning a list of `compiled-stage` objects.

`rolling-translate` does not simply `mapcar` the list over `translate` it
propegates information like the primitive and ensures that outputs of one
stage match up with the inputs of the next. This means that you only have
to specify the primitive on the vertex stage when compiling a whole
pipeline and any geometry/tessellation stages in the list will receive the
correct information.
"
    )
  (defun v-compile
      "
> NOTE: It is the authors recommendation to use `make-stage` & `translate`
>       rather than this function

This function takes lisp code as lists and returns the results of compiling
that code to glsl.

Each result is an object of type 'compiled-stage.

The stages must be defined in the following way.

- The first element of the list is the input args to the stage as pairs of
  names and types.
- The rest of the list is the body code of that stage.

Example:

    (v-compile '((a :float)) :330
               :vertex '(((pos :vec3))
                         (values (vec4 pos 1.0) a))
               :fragment '(((hmm :float))
                           (labels ((fun ((x :float))
                                      (* x x)))
                             (vec4 1.0 1.0 hmm (fun a)))))
")

  ;;
  ;; test compilation
  (defun test-translate-function-split-details
      "
")

  ;;
  ;; compiled stages
  (defclass compiled-stage
      "
`compiled-stage` is a type which is used to respresent the compiled equivalent of. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defclass compiled-vertex-stage
      "
`compiled-vertex-stage` is a type which is used to respresent the compiled equivalent of `vertex-stage`. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defclass compiled-tessellation-control-stage
      "
`compiled-tessellation-control-stage` is a type which is used to respresent the compiled equivalent of `tessellation-control-stage`. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defclass compiled-tessellation-evaluation-stage
      "
`compiled-tessellation-evaluation-stage` is a type which is used to respresent the compiled equivalent of `tessellation-evaluation-stage`. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defclass compiled-geometry-stage
      "
`compiled-geometry-stage` is a type which is used to respresent the compiled equivalent of `geometry-stage`. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defclass compiled-fragment-stage
      "
`compiled-fragment-stage` is a type which is used to respresent the compiled equivalent of `fragment-stage`. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defclass compiled-compute-stage
      "
`compiled-compute-stage` is a type which is used to respresent the compiled equivalent of `compute-stage`. Along with the data stored for `stage` you can also query:

- The compiled glsl-code using the `glsl-code` function
- The `output-variable`s usings the `output-variables` function
- The uniforms implicitly adding during compilation by calling the
 `implicit-uniforms` function
- The `primitive` output from the stage by calling the `primitive-out`
  function
- The `external-function`s used from this stage by calling the
  `external-functions` function
"
    )
  (defun glsl-code
      "
When passed an instance of `compiled-stage` this function returns a string
containing the glsl code that resulted from the compilation.
")
  (defun output-variables
      "
When passed an instance of `compiled-stage` this function returns a list
of instances of `output-variable` which represent the data output from
the given stage.
")
  (defun implicit-uniforms
      "
When passed an instance of `compiled-stage` this function returns a list
of instances of `implicit-uniform-variable` which represent the uniforms
which were added implicitly during the compilation of the given stage.
")
  (defun used-external-functions
      "
When passed an instance of `compiled-stage` this function returns a list
of `external-function` objects which were used by the given stage.
")
  (defun primitive-out
      "
When passed an instance of `compiled-stage` this function returns either
NIL or an instance of 'primitive'. This represent the primitive that
could/was be passed to the next stage.
")
  (defun block-name-string
      "
When passed an instance of `output-variable` this will return the name of
the interface-block that contained the variable as a string.
")
  ;;
  ;; compiled vars
  (defgeneric name
      "
When passed an instance of `qualfiier` `shader-variable or `external-function`
the function returns the name of the qualfiier/variable/function respectively.
")
  (defgeneric qualifiers
      "
When passed an instance of `shader-variable` or Varjo type object this function
returns a list of any qualifiers that apply to the value (of the variable or
the value to which the type applies)
")
  (defgeneric glsl-name
      "
When passed an instance of `shader-variable` this function returns a string
containing the name that the variable was given in the glsl code.
")
  (defclass shader-variable
      "
This class is the superclass of input-variable, uniform-variable and
output-variable.

You can call the following functions on it to retrieve the data:

- `name`
- `v-type-of`
- `glsl-name`
")
  (defgeneric v-type-of
      "
When called with an instance of `shader-variable` this will return the
Varjo type object which represents the variables Vari type.
")
  (defun location
      "
When called with an instance of `output-variable` this returns the location[0]
of the variable. These will only be populated from `output-variable`s from a
`fragment-stage`.

> `[0]` For info on the fragment output locations please see:
>       https://www.khronos.org/opengl/wiki/Fragment_Shader#Output_buffers
")
  (defun v-glsl-size
      "
Given a Varjo type object this function will return it's 'glsl size'. By this
we mean the number of 'positions' the value would take up (for example when
being passed into a `vertex-stage`)
")
  (defun cpu-side-transform
      "
When passed an instance of `implicit-uniform-variable` this returns the lisp
code that was to be used to populate the implicit uniform.

See `add-lisp-form-as-uniform` for more details.
")
  ;;
  ;; hooks
  (defmacro with-stemcell-infer-hook
      "
When `translate` is called within the dynamic extern of the body of this form,
if there is a use of a variable which is not in scope in the Vari code, but
is not constant in the Common Lisp environment then the user provided function
will be called with the symbol naming the variable.
The user provided function can:
- Return a Vari type-spec. This will become the type of the form.
- Return nil to have Varjo tell the user that the symbol is unbound.
- Throw an error. If you wish to inform the user that something else is awry.
")
  (defmacro with-constant-inject-hook
      "
When `translate` is called within the dynamic extern of the body of this form,
if there is a use of a variable which is not in scope in the Vari code, but
is `boundp` & constant in the Common Lisp environment then the user
provided  function will be called with the symbol naming the variable.
The user provided function must  return a Vari type-spec or throw an error to
inform the user of the issue.
")
  ;;
  ;; testing
  (defmacro with-unknown-first-class-functions-allowed
      "
When `translate` is called within the dynamic extern of the body of this form,
it will not throw an error if there is a use of `function` with an unknown
function name/signature. In the event of such a use the compiler will generate
a dummy function to satify the typechecker.

This is only useful for testing and the resulting GLSL will (most likely) not
be legal
"))
