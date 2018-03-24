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

  (defvar *draw-modes*
    "
`*draw-modes*` is a variable that holds the list of draw-modes Varjo recognises
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
  (defvar *valid-contents-symbols*
    "
`*valid-contents-symbols*` is a variable that holds the symbols that are valid
to use in a context declaration.
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
  ;; primitives, draw-modes, etc
  (defclass dynamic
      "
`dynamic` is the class that represents the simarlaly
named GL draw-mode.
")
  (defclass line-loop
      "
`line-loop` is the class that represents the simarlaly
named GL draw-mode.
")
  (defclass line-strip
      "
`line-strip` is the class that represents the simarlaly
named GL draw-mode.
")
  (defclass line-strip-adjacency
      "
`line-strip-adjacency` is the class that represents the simarlaly
named GL draw-mode.
")
  (defclass lines
      "
`lines` is the class that represents the simarlaly
named GL draw-mode.
")
  (defclass lines-adjacency
      "
`lines-adjacency` is the class that represents the similarly
named GL draw-mode.
")
  (defclass patches
      "
`patches` is the class that represents the similarly
named GL draw-mode.
")
  (defclass points
      "
`points` is the class that represents the similarly
named GL draw-mode.
")
  (defclass quads
      "
`quads` is the class that represents the similarly
named GL draw-mode.
")
  (defclass triangle-fan
      "
`triangle-fan` is the class that represents the similarly
named GL draw-mode.
")
  (defclass triangle-strip
      "
`triangle-strip` is the class that represents the similarly
named GL draw-mode.
")
  (defclass triangle-strip-adjacency
      "
`triangle-strip-adjacency` is the class that represents the similarly
named GL draw-mode.
")
  (defclass triangles
      "
`triangles` is the class that represents the similarly
named GL draw-mode.
")
  (defclass triangles-adjacency
      "
`triangles-adjacency` is the class that represents the similarly
named GL draw-mode.
")
  (defclass vertex-count
      "
`vertex-count` is the class that represents the similarly
named GL draw-mode.
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
`make-stage`
")
  (defun lisp-code
      "
`lisp-code`
")
  (defun input-variables
      "
`input-variables`
")
  (defun uniform-variables
      "
`uniform-variables`
")
  (defun context
      "
`context`
")
  (defun stemcells-allowed
      "
`stemcells-allowed`
")
  (defun primitive-in
      "
`primitive-in `
")

  input-variable
  uniform-variable
  implicit-uniform-variable
  )


;;
;; compilation
translate
rolling-translate
v-compile
;;
;; test compilation
test-translate-function-split-details
;;
;; compiled stages
compiled-stage
compiled-vertex-stage
compiled-tessellation-control-stage
compiled-tessellation-evaluation-stage
compiled-geometry-stage
compiled-fragment-stage
compiled-compute-stage
glsl-code
output-variables
implicit-uniforms
used-external-functions
primitive-out
block-name-string
;;
;; compiled vars
name
qualifiers
glsl-name
v-type-of
location
v-glsl-size
cpu-side-transform
;;
;; hooks
with-stemcell-infer-hook
with-constant-inject-hook
;;
;; testing
with-unknown-first-class-functions-allowed
;;
;; to sort
lisp-name
