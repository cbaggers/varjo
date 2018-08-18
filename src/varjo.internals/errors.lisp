(in-package :varjo.internals)

(define-bug problem-with-the-compiler () (target)
    "This shouldnt have been possible so this needs a bug report. Sorry about that~%~s" target)

(define-error cannot-compile () (code)
    "Cannot compile the following code:~%~a" code)

(define-error invalid-form-list () (code)
    "Tried to compile form however the first element of the form was a list:
~s" code)

(define-error no-function-returns () (name return-set)
    "Function '~a' did not specify any return types" name)

(define-error not-core-type-error () (type-name)
    "Type ~a is not a core type and thus cannot end up in glsl src code
   It must end up being used or converted to something that resolves
   to a glsl type." type-name)

(define-error invalid-function-return-spec () (func spec)
    "Return type spec of function ~a is invalid:~%~a" func spec)

(define-error unknown-type-spec () (type-spec)
    "Could not find the correct type for type-spec ~s~@[ ~a~]~%~a"
  type-spec
  (when (typep type-spec 'v-type)
    (format nil "~%It seems we recieved a type object instead of type spec"))
  (let ((found (find-alternative-types-for-spec type-spec)))
    (if found
        (format nil "~%Perhaps you meant one of these types?:~%~(~{~s~%~}~)"
                found)
        "")))

(define-error duplicate-name () (name)
    "This name appears more than once in this form list ~a" name)

(define-error clone-global-env-error () ()
    "Cannot clone the global environment")

(define-error clean-global-env-error () ()
    "Cannot clean the global environment")

(define-error could-not-find-function (:error-type varjo-critical-error) (name)
    "No function called '~s' was found in this environment" name)

(define-error could-not-find-any (:error-type varjo-critical-error) (name)
    "No function, macro or compiler-macro called '~a' could be found in this environment" name)

(define-error no-valid-function () (name types form)
    "There is no applicable method for the glsl function '~s'
when called with ~a

~@[Form: ~a~]"
  name
  (if types
      (format nil "argument types:~%~s" (mapcar #'type->type-spec types))
      "no arguments.")
  form)

(define-error return-type-mismatch () (sets)
    "Some of the return statements return different types:~{~%~a~}"
  (mapcar (lambda (x) (map 'list #'type->type-spec x))
          sets))

(define-error conditional-return-type-mismatch () (sets)
    "Due to a conditional the function could return any of the following:~{~%~a~}

This stopped us working out the correct return types for this function"
  (mapcar (lambda (x) (or (map 'list #'type->type-spec x) :void))
          sets))

(define-error emit-type-mismatch () (sets)
    "Some of the emit statements emit different types:~{~%~a~}"
  (mapcar (lambda (x) (map 'list #'type->type-spec x))
          sets))

(define-error setq-readonly () (var-name code)
    "You cannot setq ~a as it is readonly ~%This was attempted as follows ~a"
  var-name
  code)

(define-error assigning-to-readonly () (var-name)
    "Assignment failed as ~a is readonly"
  var-name)

(define-error assignment-type-match (:error-type varjo-critical-error)
    (code-obj-a code-obj-b form)
    "Currently varjo cannot handle changing the type through an assignment due
to the static nature of glsl.
place: ~a
value: ~a
Problematic form: ~s"
  (primary-type code-obj-a) (primary-type code-obj-b) form)

(define-error setq-type-match (:error-type varjo-critical-error)
    (var-name old-value new-value)
    "Currently varjo cannot handle changing the type through a setq
due to the static nature of glsl.

var name: ~a
type-of ~a: ~a
type-of new-value: ~a"
  var-name var-name (v-type-of old-value) (primary-type new-value))

(define-error cannot-not-shadow-core () ()
    "You cannot shadow or replace core macros or special functions.")

(define-error out-var-name-taken () (out-var-name)
    "The variable name '~a' is already taken and so cannot be used~%for an out variable"
  out-var-name)

(define-error unknown-variable-type () (name)
    "Could not establish the type of the variable: ~s" name)

(define-error var-type-mismatch () (var-type code-obj)
    "Type specified does not match the type of the form~%~s~%~s"
  (primary-type code-obj) var-type)

(define-error switch-type-error () (form)
    "Currently the clause forms of a switch must all be either ints, uints, floats or doubles:~%Code:~%~a"
  form)

(define-error loop-will-never-halt () (test-code test-obj)
    "The loop is using the following code as it's test.~%~a~%~%This will only ever result in a ~a which means the loop will never halt" test-code (type->type-spec (primary-type test-obj)))

(define-error for-loop-simple-expression () ()
    "Only simple expressions are allowed in the condition and update slots of a for loop")

(define-error for-loop-only-one-var () ()
    "for loops can only iterate over one variable")

(define-error invalid-for-loop-type () (decl-obj)
    "Invalid type ~a used as counter for for-loop"
  (primary-type decl-obj))

(define-error no-version-in-context () (env)
    "No supported version found in context:~%~a"
  (v-context env))

(define-error name-unsuitable () (name)
    "Names of variables and functions must start with an alpha char.
They also may not start with 'gl-' 'fk-' or 'sym-'
Also, naturally, t and nil are not valid names
Supplied Name: ~a~%" name)

(define-error unable-to-resolve-func-type () (func-name args)
    "Unable to resolve the result type of function '~a' when called~%with the argument types:~%~a~%"
  func-name
  (mapcar #'type->type-spec (mapcar #'primary-type args)))

(define-error out-var-type-mismatch () (var-name var-types)
    "The out variable ~a is has been set with different types.~%Types used: ~a" var-name var-types)

(define-error invalid-context-symbol () (context-symb)
    "Sorry but the symbol '~a' is not valid as a context specifier" context-symb)

(define-error invalid-context-symbols () (symbols)
    "Sorry but the following symbols are not valid as a context specifiers:
~{~s~^ ~}"
  symbols)

(define-error args-incompatible () (previous-args current-args)
    "Sorry but the output arguments from one stage are not compatible with the input arguments of the next.~%Out vars from previous stage: ~a~%In args from this stage: ~a"
  previous-args current-args)

(define-error invalid-shader-stage () (stage)
    "Sorry but '~a' is not a valid shader stage" stage)

(define-error swizzle-keyword () (item)
    "Swizzle expects a keyword to specify the components. Recieved ~a instead" item)

(define-error multi-func-stemcells () (func-name)
    "Multiple functions found named ~a that match arguments.~%However varjo cannot decide which function to use because n of the arguments passed in are of stemcell type" func-name)

(define-error uniform-in-sfunc () (func-name)
    "Must not have uniforms in shader functions, only appropriate in shader stages: ~a"
  func-name)

(define-error invalid-v-defun-template () (func-name template)
    "Template passed to v-def-glsl-template-fun must be a format string:
~a~%~a~%"
  func-name template)

(define-error keyword-in-function-position () (form)
    "Keyword cannot appear in function name position : ~s"
  form)

(define-error invalid-symbol-macro-form () (name form)
    "Symbol macros must expand to a list or atom form : ~s -> ~s~%"
  name form)

(define-error stage-order-error () (stage-type)
    "stage of type ~s is not valid at this place in the pipeline, this is either out of order or a stage of this type already exists"
  stage-type)

(define-error multi-val-bind-mismatch () (bindings val-form return-set)
    "Multiple Value Bind - Number of values returned from value form does not match bindings:
Bindings: ~a
Value Form: ~a
Returned Values: ~a"
  bindings val-form (map 'list #'type->type-spec return-set))

(define-error merge-env-func-scope-mismatch () (env-a env-b)
    "Attempting to merge two environments with different function scopes ~s~%~s~%~s"
  (cons (v-function-scope env-a) (v-function-scope env-b)) env-a env-b)

(define-error merge-env-parent-mismatch () (env-a env-b)
    "Attempting to merge two environments with different parent environments ~s~%~s~%~s"
  (cons (v-parent-env env-a) (v-parent-env env-b)) env-a env-b)

(define-error env-parent-context-mismatch () (env-a env-b)
    "Attempting to make an environment with different context to it's parent ~s~%~s~%~s"
  (cons (v-context env-a) (v-context env-b)) env-a env-b)

(define-error symbol-unidentified (:error-type varjo-critical-error) (sym)
    "Symbol '~s' is unidentified." sym)

(define-error if-form-type-mismatch () (test-form then-form then-type
                                              else-form else-type)
    "The result if ~a is true is ~a which has type ~a
however the false case returns ~a which has type ~a
This is incompatible"
  test-form then-form then-type else-form else-type)

(define-error bad-make-function-args () (func-name arg-specs)
    "Trying to define the function ~s but the following argument specifications
are invalid:
~{~s~^~%~}

Ensure the arguments are defined in the format (arg-name arg-type)
e.g. (~a :vec3) and that any array arguments have an exact size."
  func-name arg-specs
  ;; this bit below is so that, where possible, the error uses one of
  ;; your arg names in the example of a valid arg spec.
  ;; I hope this makes the error message a bit more relevent and approachable
  (let* ((potential-spec (first arg-specs))
         (potential-name (cond
                           ((listp potential-spec) (first potential-spec))
                           ((and (symbolp potential-spec)
                                 (not (null potential-spec))
                                 (not (keywordp potential-spec)))
                            potential-spec))))
    (if (and potential-name
             (symbolp potential-name)
             (not (keywordp potential-name)))
        (string-downcase (symbol-name potential-name))
        "x")))

(define-error none-type-in-out-vars () (glsl-name)
    "One of the values being returned from the shader (~s) is of type :none."
  glsl-name)

(define-error body-block-empty () (form-name)
    "In varjo it is not valid to have a ~s with an empty body."
  form-name)

(define-error flow-ids-mandatory (:error-type varjo-critical-error) (for primary-type)
    "~a must be given flow id/s when created: type - ~s" for primary-type)

(define-error flow-id-must-be-specified-vv (:error-type varjo-critical-error) ()
    "v-values must be given a flow id when created")

(define-error flow-id-must-be-specified-co (:error-type varjo-critical-error) ()
    "code objects must be given a flow id when created")

(define-error if-branch-type-mismatch (:error-type varjo-critical-error) (then-obj)
    "Type mismatch: else-case is nil which is of bool type, yet the then form is of ~s type."
  (type->type-spec (primary-type then-obj)))

(define-error if-test-type-mismatch (:error-type varjo-critical-error) (test-obj)
    "The result of the test must be a bool.~%~s" (primary-type test-obj))

(define-error cross-scope-mutate (:error-type varjo-critical-error) (var-name code)
    "It is illegal to assign to variables from outside the function's own scope

Tried to mutate ~s
~s"
  var-name code)

(define-error illegal-implicit-args (:error-type varjo-critical-error) (func-name)
    "Implicit args are not allowed in the function ~s" func-name)

(define-error invalid-flow-id-multi-return (:error-type varjo-critical-error)
    (func-name return-type)
    "Found a multiple-return-func ~s invalid return types:
~{~s~}"
  func-name return-type)

(define-bug loop-flow-analysis-failure (:error-type varjo-critical-error) ()
    "Varjo's flow analyzer has been unable to resolve the variable flow in this
loop within a reasonable ammount of time. This counts as a compiler bug so
please report it on github")

(define-error invalid-env-vars  (:error-type varjo-critical-error) (vars)
    "Attepted to create an environment which has invalid variable data:
~s" vars)

(define-error values-safe-wasnt-safe (:error-type varjo-critical-error) (args)
    "the 'value-safe special form has been used. However the target function it
was run against had more than one argument with multi-value returns.
This is an illegal configuration. If you did not use the 'values-safe form in
your code knowingly, then contact the maintainer of the library that triggered
this. It is certainly a bug on their end.

Compiled Args:
~{~s~%~}"
  args)

(define-error empty-progn (:error-type varjo-critical-error) ()
    "progn with no body found, this is not currently allowed by varjo")

(define-bug name-clash (:error-type varjo-critical-error) (lisp glsl)
    "The glsl name ~s was generated for the lisp-name ~s
However this name was already taken. This is Varjo bug, please raise an issue
on github.
Sorry for the inconvenience"
  glsl lisp)

(define-bug name-mismatch (:error-type varjo-critical-error) (lisp glsl taken)
    "The glsl name ~s was generated for the lisp-name ~s
However this clashes with glsl name for the previous symbol ~s.
This is Varjo bug, please raise an issue on github.
Sorry for the inconvenience"
  glsl lisp taken)

(define-error function-with-no-return-type (:error-type varjo-critical-error)
    (func-name)
    "The function named ~s does not return anything, this is not
currently allowed. The tail position of the function must be an expression with
a return type.

Most likely you currently have one of the following in the tail position:
%if, for or while" func-name)

(define-error external-function-invalid-in-arg-types
    (:error-type varjo-critical-error) (name args)
    "When defining the function ~a we found some args with types that
we didnt recognise:

~{> ~s~}

~@[
Here are some types we think may have been meant:
~{~a~}
~]
"
  name args
  (loop :for a :in args
     :for suggestion := (find-alternative-types-for-spec (second a))
     :when suggestion
     :collect (format nil "~%> ~s~{~%~s~}" (first a) suggestion)))

(define-bug invalid-special-function-arg-spec (:error-type varjo-critical-error)
    (name spec)
    "The special function named ~s has an invalid argument spec:
~a

Please report this bug on github" name spec)

(define-error closures-not-supported (:error-type varjo-critical-error)
    (func details)
    "The function ~s is a closure and currently Varjo doesnt support
passing these around as first class objects.

Sorry for the odd limitation, this will be fixed in a future version.
~{~%~%~a~}"
  func
  (loop :for (func var-names glsl-names) :in details :collect
     (format nil "Problematic function:~%~a~%Captured Var/s: ~{~a~^,~^ ~}
~@[Implicit Arg/s (glsl names): ~{~a~^,~^ ~} ~]"
             func var-names glsl-names)))

(define-error cannot-establish-exact-function (:error-type varjo-critical-error)
    (funcall-form)
    "Could not establish the exact function when compiling:

~s

Because first class functions don't exist in GLSL, Varjo needs to be able to
work out what function is going to be called at compile time. In this case that
was not possible.

Usually Varjo should throw a more descriptive error earlier in the compile
process so if you have time please report this on github. That way we can try
and detect these cases more accurately and hopefully provide better error
messages." funcall-form)

(define-error shared-in-function () (name)
    "We do not support &shared args in functions, only in shader stages: ~a"
  name)

(define-error uniform-in-cmacro () (name)
    "We do not currently support &uniforms args in compiler macros, only in shader stages: ~a"
  name)

(define-error shared-in-cmacro () (name)
    "We do not support &shared args in compiler macros, only in shader stages: ~a"
  name)

(define-error optional-in-cmacro () (name)
    "We do not currently support &optional args in compiler macros: ~a"
  name)

(define-error key-in-cmacro () (name)
    "We do not currently support &key args in compiler macros: ~a"
  name)

(define-error no-types-for-regular-macro-args () (macro-name arg)
    "The type of the argument ~a is unknown at this stage.

The macro named ~a is a regular macro (defined with v-defmacro). This means
that, the arguments passed to it are uncompiled code and as such, we cannot get
their types.

It is however possible to retrieve the argument types for compiler-macros."
  arg macro-name)

(define-error no-metadata-for-regular-macro-args () (macro-name arg)
    "The metadata for the argument ~a is unknown at this stage.

The macro named ~a is a regular macro (defined with v-defmacro). This means
that, the arguments passed to it are uncompiled code and as such, we cannot get
any metadata about them.

It is however possible to retrieve the metadata of arguments in compiler-macros."
  arg macro-name)

(define-error no-tracking-for-regular-macro-args () (macro-name arg)
    "The flow information for the argument ~a is unknown at this stage.

The macro named ~a is a regular macro (defined with v-defmacro). This means
that, the arguments passed to it are uncompiled code and as such, we cannot
trace where they have come from.

It is however possible to retrieve this data for arguments in compiler-macros."
  arg macro-name)

(define-error unknown-macro-argument () (macro-name arg)
    "Could not find an argument named ~a in the macro ~a"
  arg macro-name)

(define-error symbol-macro-not-var () (callee name)
    "~a was asked to find the value bound to the symbol ~a, however ~a is
currently bound to a symbol-macro."
  callee name name)

(define-error unbound-not-var () (callee name)
    "~a was asked to find the value bound to the symbol ~a, however ~a is
currently unbound."
  callee name name)

(define-error not-proved-a-uniform (:error-type varjo-critical-error) (name)
    "We are unable to prove that ~a has come from a uniform"
  name)

(define-error duplicate-varjo-doc-string () (form dup)
    "We have found an illegal duplicate docs string.

Doc string: ~s

Found in form:
~s"
  dup form)

(define-error calling-declare-as-func () (decl)
    "Found a declare expression in an invalid position.

Declaration: ~s

There is no function named DECLARE. References to DECLARE in some contexts (like
the starts of blocks) are unevaluated expressions, but here it is illegal."
  decl)

(define-error treating-declare-as-func () (decl)
    "Found an attempt to take a reference to declare as a function.

Form: ~s

There is no function named DECLARE. References to DECLARE in some contexts (like
the starts of blocks) are unevaluated expressions, but here it is illegal."
  decl)

(define-error v-unsupported-cl-declaration () (decl)
    "Found an unregonised declaration named ~a

Whilst this is valid in standard common-lisp, it is not currently valid in
Varjo.

Full Declaration: ~s"
  (first decl) decl)

(define-error v-only-supporting-declares-on-vars () (targets)
    "We found the following invalid names in the declarations:
~@[~{~%~s~}~%~]
We don't yet support declarations against functions or symbol-macros.
Sorry for the inconvenience."
  targets)

(define-error v-declare-on-symbol-macro () (target)
    "We found a declaration against ~s. However at this point in the
compilation, ~s is bound to a symbol-macro and Varjo does not support
declarations against symbol-macros."
  target target)

(define-error v-declare-on-nil-binding () (target)
    "We found a declaration against ~s. However at this point in the
compilation, ~s is not bound to anything."
  target target)

(define-error v-metadata-missing-args () (name required provided missing)
    "The metadata type ~a requires the following args to be specified
on creation: ~{~a~^, ~}

However, the following was provided instead: ~s

Please provide values for: ~{~a~^, ~}

It is perfectly legal to set the values to nil, but we require them to be
declared to something."
  name required provided missing)

(define-warning cant-shadow-user-defined-func () (funcs)
    "Unfortunately we cannot currently shadow user-defined functions.
The following functions have been skipped:~{~%~s~}"
  funcs)

(define-warning cant-shadow-no-type-match () (shadowed funcs)
    "Was asked to shadow the following functions, however none of the
arguments have the type ~a

The following functions have been skipped:~{~%~s~}"
  shadowed funcs)

(define-error shadowing-user-defined-func () (func)
    "Unfortunately we cannot currently shadow user-defined functions.
The function in question was: ~s"
  func)

(define-error shadowing-no-type-match () (shadowed func)
    "Was asked to shadow the following function, however none of the
arguments have the type ~a

The function in question was: ~s"
  shadowed func)

(define-error shadowing-no-return-matched () (shadowed func)
    "Was asked to shadow the following function, however none of the
returned values have the type ~a

The function in question was: ~s"
  shadowed func)

(define-error shadowing-multiple-constructors () (shadow-type func-id funcs)
    "Was asked to shadow the function with the idenifier ~a  as a
constructor for the shadow-type ~a.

However this function-identifier names multiple functions, which is not
allowed in this form.

The functions in question were:~{~%~a~}"
  func-id shadow-type funcs)

(define-error shadowing-multiple-funcs () (shadow-type pairs)
    "Was asked to shadow the functions for the shadow-type ~a.

However these function-identifiers name multiple functions, which is not
allowed in this form.

The functions in question were:
~{~%Identifier: ~a~%Named functions: ~a~%~}"
  shadow-type pairs)

(define-error shadowing-constructor-no-match () (shadow-type func-id)
    "Was asked to shadow the function with the idenifier ~a as
a constructor for the shadow-type ~a.

However no functions were found that matched this identifier."
  func-id shadow-type)

(define-error def-shadow-non-func-identifier () (name func-ids)
    "~a was ask to shadow some functions, however the following
identifiers are have problems:
~{~%~s~}

The identifiers passed to this macro should be in the format:
#'func-name  - or - #'(func-name arg-type arg-type)}"
  name func-ids)

(define-error shadowing-funcs-for-non-shadow-type () (name shadow-type)
    "~a was ask to shadow some functions for the type ~a,
however the type ~a is not a shadow type."
  name shadow-type shadow-type)

(define-error fell-through-v-typecase () (vtype wanted)
    "~a fell through V-ETYPECASE expression.
Wanted one of the following types: ~s}"
  vtype wanted)

(define-error metadata-conflict () (metadata-kind flow-id new-meta old-meta)
    "~a metadata already found for flow-id ~a.~%Metadata cannot be redefined

Tried to apply: ~a

Metadata already present: ~a"
  metadata-kind flow-id new-meta old-meta)

(define-error metadata-combine-invalid-type () (expected found)
    "Asked for a combined version of the two pieces of metadata of kind
~a, however the metadata returned was of type ~a. When combining metadata it is
only valid to return nil or a piece of metadata of the correct type."
  expected found)

(define-error multiple-external-func-match () (name matches)
    "Multiple externally defined functions found matching the
identifier ~a:

Matches:
~{~s~^~%~}"
  name matches)

(define-bug doesnt-have-dimensions () (vtype)
    "Compiler bug: Attempted to find the dimensions of ~a. If you have
the time, please report this on github."
  vtype)

(define-error cannot-swizzle-this-type () (vtype)
    "Was asked to swizzle a value with the type ~a

However is not a type that can be swizzled. ~a"
  (type->type-spec vtype)
  (if (typep vtype 'v-struct)
      "Perhaps you meant one of this struct's slots?"
      ""))

(define-error dup-name-in-let () (dup-name)
    "The variable ~a occurs more than once in the LET."
  dup-name)

(define-error dup-names-in-let () (names)
    "The following variables occur more than once in the LET
~{~s~}"
  names)

(define-error uninitialized-var () (name)
    "Cannot access the variable ~a as it is currently unbound"
  name)

(define-error global-uninitialized-var () (name)
    "Cannot declare ~s as an unbound variable at global scope"
  name)

(define-bug nil-return-set () (form possible-set)
    "Could not establish return-set.

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic form:
~a

Possible Set: ~a" form possible-set)

(define-bug nil-emit-set () (form possible-set)
    "Could not establish emit-set.

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic form:
~a

Possible Set: ~a" form possible-set)

(define-bug with-fresh-env-scope-missing-env () ()
    "with-fresh-env-scope expects a code object & an environment to
be returned from it's body. However there was no environment returned.")

(define-error stage-primary-type-mismatch () (stage-kind type-found type-expected)
    "The primary return value from ~a must be a ~a.
Instead ~a was found"
  stage-kind
  (type->type-spec type-expected)
  (type->type-spec type-found))

(define-error multi-dimensional-array () (dimensions)
    "We do not yet support multidimensional arrays.
However you can have arrays of arrays.

Problematic dimensions: ~a"
  dimensions)

(define-error make-array-mandatory-args () (args)
    "Cannot compile make-array as required arguments are missing.

Required args: dimensions & element-type
Found args: ~s"
  args)

(define-error make-array-conflicting-args () (args)
    "Cannot compile make-array as found that both initial-element
and initial-contents were specified.

Found args: ~s"
  args)

(define-error make-array-conflicting-lengths () (dims initial-contents)
    "Cannot compile make-array as the declared dimensions did not
match the length of the initial-contents that were specified.

Declared dimensions: ~a
initial-contents: ~s"
  dims initial-contents)

(define-error make-array-cant-cast-args () (element-type initial-contents)
    "Cannot cast the provided initial-contents to the specified
element-type.

element-type: ~a
initial-contents: ~s"
  element-type initial-contents)

(define-error make-array-cant-establish-default-value ()
    (element-type initial-contents)
    "Cannot establish the default element value for the type:
~s"
  element-type)

(define-error should-be-quoted () (thing val)
    "Varjo expected ~a to be quoted, however it was not.
Found: ~s"
  thing val)

(define-error should-be-constant () (thing val)
    "Varjo expected ~a to be constant, however it was not.
Found: ~s"
  thing val)

(define-error stage-in-context () (context)
    "It is not longer valid for the stage to be declared in the context
Please simply use #'v-compile or use #'make-stage with #'rolling-translate if
you need more control.

Context found: ~s" context)

(define-error primitive-in-context () (context)
    "It is not longer valid for the primitive to be declared in the context
Please simply use the primitive argument to make-stage.

Context found: ~s" context)

(define-error invalid-stage-kind () (kind)
    "make-stage called with the invalid stage kind ~s"
  kind)

(define-error no-primitive-found () (stage)
    "Could not establish primitive type for ~a.
Stage: ~a"
  (type-of stage)
  stage)

(define-error invalid-primitive-for-geometry-stage () (prim)
    "~s is not a valid primitive kind for geometry stages. Instead try
points, lines, lines-adjacency, triangles or triangles-adjacency." prim)

(define-error invalid-primitive-for-tessellation-stage () (prim)
    "~s is not a valid primitive kind for tessellation stages. You must use a
patch e.g. (:patch 4) instead" prim)

(define-error rolling-translate-invalid-stage () (invalid)
    "rolling translate expects a list of stages as it's first argument.
However it found ~a invalid ~a in the list:~{~%~s~}"
  (if (> (length invalid) 1) "these" "this")
  (if (> (length invalid) 1) "elements" "element")
  invalid)

(define-error couldnt-convert-primitive-for-geometry-stage () (prim prev-stage)
    "A primitive of type ~a came from the ~a stage, unfortunately we
weren't sure how to convert this to a primitive kind the geometry shader can
use."
  prim prev-stage)

(define-error test-translate-failed () (grouped-errors)
    "Compilation Failed:
~{~%~a~%~}"
  (mapcar (lambda (grp) (format nil "Stages:~{ ~a~}~%~a" (car grp) (cdr grp)))
          grouped-errors))

(define-error returns-in-geometry-stage () (return-set)
    "In geometry stages please do not return values from the main stage
function. Instead use #'emit-data & then #'emit-vertex to attach the data to
the vertex itself.

Like #'return, #'emit-data can handle multiple values.

Found the following return values from the main stage function:~{~%~a~}"
  (map 'list #'type->type-spec return-set))

(define-error emit-not-in-geometry-stage () (stage emit-set)
    "emit, emit-data & friends are only valid in geometry stages, found it's
usage in a ~a stage

Found the following emitted values from the main stage function:~{~%~a~}"
  (type-of stage)
  (mapcar #'type->type-spec emit-set))

(define-error primitives-dont-match () (out-stage out in-stage in)
    "The primitives leaving the ~a stage are of type ~a, however the
~a was expecting ~a"
  out-stage out in-stage in)

(define-error tessellation-control-expects-patches () (primitive)
    "Tried to compile a tessellation-control stage however recieved a primitive
of type ~a rather than a patch."
  (type-of primitive))

(define-error tessellation-evaluation-invalid-primitive () (primitive)
    "Tessellation Evaluation stages can tessellate to points, triangles, quads
or iso-lines. Found ~a instead"
  (type-of primitive))

(define-error inline-glsl-vertex-stage-not-supported () ()
    "Inline glsl vertex stages are not yet supported.")

(define-error clashes-found-between-input-and-output-names ()
    (stage-kind inputs outputs clashes)
    "When compiling a GLSL ~a stage duplicate names we found in the inputs and
outputs. This is not allowed.

In Arguments: ~s
Out Values: ~s

Clashes: ~s
" stage-kind inputs outputs clashes)

(define-bug user-func-invalid-x () (kind name args)
    "Invalid types found in internal user-function construction.

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic Definition:
NAME: ~s
~a: ~s" name kind args)

(define-error invalid-inline-glsl-stage-arg-layout () (arg)
    "Invalid arg layout found in glsl stage. The correct layout for a argument
to a glsl-stage is (\"string-name-of-arg\" arg-type ,@keyword-qualifiers)

Problematic arg was: ~a"
  arg)

(define-bug return-set-mismatch () (form)
    "Type & Return sets don't match

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic Definition:
~s" form)

(define-error funcall-of-special-operator () (code)
    "Cannot FUNCALL ~a as it is a special operator"
  code)

(define-error slot-value-on-non-struct () (type slot-name)
    "Was asked to access the slot ~a on ~a, however this is not a struct"
  slot-name
  (type->type-spec type))

(define-error slot-not-found () (type slot-name)
    "Could not find a slot named ~a on struct of type ~a"
  slot-name
  (type->type-spec type))

(define-error recursive-function-call-detected () (func)
    "Recursive function call detected however recursion (both direct and
indirect) is disallowed by GLSL.

Problematic function: ~s" func)

(define-error probable-recursion () (name func)
    "Compile progress looks suspiciously like ~a is recursive.
Recursion (both direct and indirect) is disallowed by GLSL.

func: ~s

The current check for this in Varjo is currently very weak if you have received
this error and do not think you have recursion in your code please raise an
issue at https://github.com/cbaggers/varjo/issues including the code that
triggered this error." name func)

(define-error invalid-function-arg-format () (name arg)
    "The argument form ~a in the function ~a is invalid.

Expected (arg-name arg-type)"
  arg name)

(define-error underspecified-patch-primitive () ()
    "Whilst 'patches' are a valid form of primitive, the way they are defined
in Varjo is (:patch *) where '*' is the number of vertices in the patch.

For example: (:patch 2) (:patch 3) (:patch 4)")

(define-error non-place-assign () (glsl-code lisp-code)
    "You cannot assign this: ~a ~%This was attempted as follows: ~a"
  glsl-code
  lisp-code)

(define-error v-unrecognized-declaration () (decl)
    "Found an unregonised declaration named ~a
~@[~%Might you have meant one of these?:~{~%~s~}~%~]
Full Declaration: ~s"
  (first decl) (find-alternative-declaration-kinds (first decl)) decl)

(define-error v-def-template-arg-mismatch () (args types)
    "v-def-template-arg-mismatch found a mismatch between
~s
and
~s"
  args types)

(define-error illegal-&rest-in-args () (func-name arg-specs)
    "Illegal &rest found in function definition for ~a

TLDR: This is not allowed in Vari functions.

Arguments: ~a

&rest is tricky for Vari as glsl does not support lists so we cannot
provide the same interface as in CL. Even if we could (or were to use
arrays) it would make for very poorly performing glsl.

The places where &rest can be used are:

- Macros: as there we use the host lisp and thus have lists

- glsl-template-functions: Where we can use ~~{...~~} in the format string"
  func-name arg-specs)

(define-error illegal-&uniform-in-args () (func-name arg-specs)
    "Illegal &uniform found in function called from within stage

The function in question was ~a

TLDR: Vari does not yet support directly calling function taking uniforms.

Arguments: ~a

> The rest of this error report is a discussion of why this feature is not
> currently supported. Feel free to ignore it.

External functions taking uniforms cannot be called directly from
Varjo. This may be surprising as you may have seen that some third
party libraries that use Varjo (like CEPL) use functions for their
stages, however that is all handled internally to those systems.

There has been discussion of whether to support this. One possibility
is to treat them like keyword args, which is nice in that it fits
with the &key style syntax we use. In that case a external function

    (define-vari-function some-func ((x :float) &uniform (y :int))
      ..)

Could be called like:

    (some-func 10 :y 20)

However there are issues:
- keywords are optional, do we then require defaults?
- how do the third party libs handle defaults? we must not force them to
  generate slower code (like extra conditionals)

If we dont use the keyword syntax then we could be like &optional,
however the issue of defaults still remains.

If we require providing all &uniform args then we are diverging further
from Common Lisp behaviour and so are adding more things for users to
learn.

Lastly we would have to define how &uniform interacts with &optional.

In all it's not trivial to add without making something rather foreign.
Suggestions are welcome on github however."
  func-name arg-specs)

(define-error cannot-take-reference-to-&rest-func () (func-name)
    "Cannot take reference to ~a

Functions containing &rest can be taken as values but you must specify the
precise definition of the function.

For example, for a function FOO with args (:int &rest :int) you can do this:

    (let ((fn0 #'(foo :int))
          (fn1 #'(foo :int :int))
          (fn2 #'(foo :int :int :int))
      (+ (funcall fn0 0)
         (funcall fn1 1 2)
         (funcall fn2 1 2 3))
" func-name)

(define-error attempted-transform-feedback-in-fragment-shader () ()
    "The fragment stage in this pipeline was found to contain the :feedback
qualifier.

Transform feedback is only valid in the vertex processing stages and thus
:feedback can only be used from :vertex, :tessellation-control,
:tessellation-evaluation or :geometry stages.")

(define-error transform-feedback-incorrect-stage () (stage)
    "The :feedback qualifier was found in the ~a stage.

This is invalid in this case because only the last vertex processing stage is
allowed to use transform feedback."
  (etypecase stage
    (compiled-vertex-stage "vertex")
    (compiled-tessellation-control-stage "tessellation-control")
    (compiled-tessellation-evaluation-stage "tessellation-evaluation")))

(define-error invalid-feedback-qualifier-form () (form)
    "The :feedback qualifier may be provided just as a keywork or as
a form with a single positive integer specifying the group e.g. (:feedback 0)

However in this case we found ~s which is invalid"
  form)

(define-error invalid-primitive-for-compute-stage () (prim)
    "Whilst compiling a compute stage it was found that primitive had been set
to something other than nil. This is invalid for this stage. Please use nil
instead.

Primitive found: ~a"
  prim)

(define-error compute-pipeline-may-only-contain-one-stage () (stages)
    "A attempt was made to compile a pipeline with the following stages:
~{~a~^, ~}

However, if you are compiling a pipeline that contains a compute stage there
may not be any other stages in that pipeline."
  stages)

(define-error stage-must-have-output-prim-declaration () (stage)
    "The function used as a geometry stage must have a top level
output-primitive declaration

Stage: ~a" stage)

(define-error stage-must-have-output-patch-declaration () (stage)
    "The function used as a tessellation control stage must have a top level
output-patch declaration

Stage: ~a" stage)

(define-error stage-must-have-local-size-declaration () (stage)
    "The function used as a compute stage must have a top level local-size
declaration.

Stage: ~a" stage)

(define-error compute-stage-with-in-args () (args)
    "We were asked to compile a compute stage but have found that it has input
arguments defined for it. GL does not allow input arguments for compute stages,
only uniforms.

Args found: ~a" args)

(define-error uniform-ubo-and-ssbo () (arg)
    "Whilst making a stage object we found a uniform argument which is
specified to be both a UBO and a SSBO. It is not valid for a uniform to be
both of these.

Uniform Argument: ~s" arg)

(define-error ubo-ssbo-type-limitation () (type)
    "Currently UBOs and SSBOs in Varjo must be structs.

If this is impeding your work please feel free to file an issue
at https://github.com/cbaggers/varjo/issues")

(define-error compute-stage-must-be-void () (returns)
    "Compute stages must return (values) however this one returns:
~a"
  returns)

(define-error unknown-layout-specifier () (target-kind name specifier)
    "Unknown layout specifier used for the ~a uniform named ~a.
Was expecting one of std-140 or std-430. Recieved ~a instead."
  target-kind name specifier)

(define-error find-mutual-type-bug () (types)
    "BUG: find-mutual-cast-type take a list of types, not type specs
~a" types)

(define-error void-type-for-conditional-test () (kind form)
    "The test for an '~a' expression cannot be :void.

 (~a ~a
     ..)"
  kind
  kind
  form)

(define-error discarded-for-conditional-test () (kind form)
    "The expression used as the test for one of your '~a' expression is just
discarding the fragment. We arent sure what to emit for this '~a'.

 (~a ~a
     ..)"
  kind
  kind
  kind
  form)

(define-error discard-not-in-fragment-stage () (stage)
    "The discard expression was found in a ~a. This is invalid as it is only
allowed in fragment stages" stage)

(define-error opaque-data-found () (arg-name type-spec)
    "The argument ~a has type ~a which contains opaque data.
This is invalid in this context"
  arg-name type-spec)

(define-error invalid-output-primitive-for-geometry () (kind)
    "The output primitive for the geometry stages was set to be ~s,
however the only valid option is one of the following:

- :points
- :line-strip
- :triangle-strip" kind)

(define-error let-void () (name)
    "Found an invalid attempt to make a local variable ~a with type :void"
  name)

(define-error let-discarded () (name)
    "Found an attempt to 'let' a local variable called ~a, however the form
providing the value has discarded the fragment and as such will never return.

Due to this CEPL can't infer a valid type for this form and this is triggering
this issue in 'let'"
  name)

(define-error let-or () (name type)
    "Found an invalid attempt to make a local variable ~a with the
type ~a

This usually happens due to the use of a conditions (like an 'if') where the
different branches of the conditional have different types. Due to this CEPL
can't infer a single valid type for this form and this is triggering this
issue."
  name (type->type-spec type))

(define-error let-or-functions () (name type form)
    "
Could not statically resolve function when assigning to ~a

Varjo needs to be able to statically resolve where functions are passed as
GLSL itself does not support first class functions.

Varjo could of course pass around an object representing the function (such as
an :int) and add a switch at each call-site, however this would result
in non-obvious performance characteristics (such as the effect on divergence)

This usually happens due to the use of a conditions (like an 'if') where the
different branches of the conditional have different types. Due to this CEPL
can't infer a single valid type for this form and this is triggering this
issue.

Code:
~a

Type:
~a
"
  name
  form
  (type->type-spec type))

(define-error let-returned () (name)
    "Found an attempt to 'let' a local variable called ~a, however the form
providing the value has called return so this expression will never result in a
value.

Due to this CEPL can't infer a valid type for this form and this is triggering
this issue in 'let'"
  name)

(define-error if-form-multiple-vals-mismatch () (then-set else-set)
    "Found an if form where the number of values returned on each branch
do not match.

The then branch returned: ~s
The else branch returned: ~s

This matters as the result of this 'if' is going to be used as a return from a
function or by a multiple-value-bind form"
  (or (map 'list #'type->type-spec then-set) :void)
  (or (map 'list #'type->type-spec else-set) :void))

(define-warning v-deprecated (:error-type style-warning) (old new)
    "'~a' is deprecated, please use '~a'"
  old new)

(define-error fragment-integer-inputs-not-flat () (problem-types inputs)
    "Found ~a ~a not qualfied as :flat. This
is not allowed in glsl.

Outputs:~{~%~s~}"
  (length problem-types)
  (if (= (length problem-types) 1)
      "fragment-stage input which was an integer and was"
      "fragment-stage inputs which were integers and were")
  (mapcar #'type->type-spec inputs))

(define-error invalid-coerce () (src-type dst-type code)
    "Could not coerce ~a to ~a

Code:
~s"
  (type->type-spec src-type)
  (type->type-spec dst-type)
  code)

(define-error conditional-multiple-vals-mismatch () (kind sets)
    "Found an '~a' form where the number of values returned on each branch
do not match.

The branches returned:~{~%~s~}

This matters as the result of this '~a' is going to be used as a return from a
function or by a multiple-value-bind form"
  kind
  (loop :for set :in sets :collect
     (or (map 'list #'type->type-spec set) :void))
  kind)

(define-bug no-args-remove-in-unrep-inlining () (func args args-code)
    "
Sorry that you have hit this bug, if you have time please copy this error,
your stack trace and (if possible) the code that triggered this error and
raise an issue at https://github.com/cbaggers/varjo/issues

Thankyou, and our apologies.

----

When trying to compile the function ~a we knew we had to inline as a local
function to to one of the arguments being of an unrepresentable type. However
No arguments were identified for moving from the function to the surrounding
let.

func: ~a
args: ~a
args-code: ~a
"
  (slot-value func 'name)
  func
  args
  args-code)

(define-error invalid-glsl-numeric-literal () (str)
    "
When a string is found in regular Vari code it is assumed to be a numeric
literal. We provide this option as some numbers are chosen for their exact bit
arrangement and we dont want to risk an implementation affecting that[0].

In this case however we recieved ~s which we don't know how to handle.

We currently accept:

- floats e.g. \"1.23\"
- ints e.g. \"-10\"
- uints e.g. \"12u\"

Sorry for the inconvenience. If you think of another string pattern you would
like supported, please raise an issue at our github page:
https://github.com/cbaggers/varjo/issues

[0] this could happen due to CL not requiring ieee754 floats in the standard"
  str)

(define-error invalid-the-declaration () (form declared-type found-type)
    "
Found a 'the' declaration which appears to be invalid.

The type that was expected was: ~a

However the type found was: ~a

The the form was: ~a
"
  (type->type-spec declared-type)
  (type->type-spec found-type)
  form)

(define-error invalid-type-for-dummy-function () (type form)
    "
Found an attempt to use a ~a in a place a function was expected.

Please see the following code:

~a
"
  (type->type-spec type)
  form)

(define-error struct-cannot-hold-ephemeral-types () (name slots)
    "
The struct ~a could not be defined as it is not legal to store ephemeral types
in structs.

Problematic Slots:~{~%- ~a~}
"
  name
  slots)

(define-error arrays-cannot-hold-ephemeral-types () (form)
    "
make-array has failed as it is not legal for the element-type to be an
ephemeral type.

Form: ~a
" form)

(define-error incorrect-stage-for-shared-variables () (stage)
    "
Shared variables are only valid in compute stages. This is a ~a.
" (type-of stage))

(define-error shared-opaque () (name type)
    "
We found an issue when compiling the shared variable '~a'.

Shared variables may not hold opaque data, however the type of '~a'
is ~a
" name name (type->type-spec type))

(define-error alt-type-name-already-taken () (alt-name src-name)
    "
An attempt was found to define '~a' as an alternate type name for '~a' however
~a names an existing type.
" alt-name src-name alt-name)

(define-error unknown-alt-type-name () (name)
    "
We were asked to remove the alternate type name '~a' however we are unable
to find that alias.
" name)

(define-error invalid-arguments-for-special-op () (name args)
    "
Invalid arguments for special function ~a

Arguments: ~a
" name args)

;;type->type-spec

;;
;; Hi! Don't forget to add the name of your condition to the
;; varjo.conditions package
;;
