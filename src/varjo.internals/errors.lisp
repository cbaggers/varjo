(in-package :varjo.internals)

(defbug problem-with-the-compiler () (target)
    "This shouldnt have been possible so this needs a bug report. Sorry about that~%~s" target)

(deferror cannot-compile () (code)
    "Cannot compile the following code:~%~a" code)

(deferror invalid-form-list () (code)
    "Tried to compile form however the first element of the form was a list:
~s" code)

(deferror no-function-returns () (name return-set)
    "Function '~a' did not specify any return types" name)

(deferror not-core-type-error () (type-name)
    "Type ~a is not a core type and thus cannot end up in glsl src code
   It must end up being used or converted to something that resolves
   to a glsl type." type-name)

(deferror invalid-function-return-spec () (func spec)
    "Return type spec of function ~a is invalid:~%~a" func spec)

(deferror unknown-type-spec () (type-spec)
    "Could not find the correct type for type-spec ~s~@[ ~a~]~%~a"
  type-spec
  (when (typep type-spec 'v-type)
    (format nil "~%It seems we recieved a type object instead of type spec"))
  (let ((found (find-alternative-types-for-spec type-spec)))
    (if found
        (format nil "~%Perhaps you meant one of these types?:~%~(~{~s~%~}~)"
                found)
        "")))

(deferror duplicate-name () (name)
    "This name appears more than once in this form list ~a" name)

(deferror clone-global-env-error () ()
    "Cannot clone the global environment")

(deferror clean-global-env-error () ()
    "Cannot clean the global environment")

(deferror could-not-find-function (:error-type varjo-critical-error) (name)
    "No function called '~a' was found in this environment" name)

(deferror could-not-find-any (:error-type varjo-critical-error) (name)
    "No function, macro or compiler-macro called '~a' could be found in this environment" name)

(deferror no-valid-function () (name types form)
    "There is no applicable method for the glsl function '~s'
when called with ~a

~@[Form: ~a~]"
  name
  (if types
      (format nil "argument types:~%~s" (mapcar #'type->type-spec types))
      "no arguments.")
  form)

(deferror return-type-mismatch () (sets)
    "Some of the return statements return different types:~{~%~a~}"
  (mapcar (lambda (x) (map 'list #'type->type-spec x))
          sets))

(deferror conditional-return-type-mismatch () (sets)
    "Due to a conditional the function could return any of the following:~{~%~a~}

This stopped us working out the correct return types for this function"
  (mapcar (lambda (x) (or (map 'list #'type->type-spec x) :void))
          sets))

(deferror emit-type-mismatch () (sets)
    "Some of the emit statements emit different types:~{~%~a~}"
  (mapcar (lambda (x) (map 'list #'type->type-spec x))
          sets))

(deferror setq-readonly () (var-name code)
    "You cannot setq ~a as it is readonly ~%This was attempted as follows ~a"
  var-name
  code)

(deferror assigning-to-readonly () (var-name)
    "Assignment failed as ~a is readonly"
  var-name)

(deferror assignment-type-match (:error-type varjo-critical-error)
    (op code-obj-a code-obj-b form)
    "Currently varjo cannot handle changing the type through an assignment due
to the static nature of glsl.
operation: ~a
place: ~a
value: ~a

Problematic form: ~s"
  op (primary-type code-obj-a) (primary-type code-obj-b) form)

(deferror setq-type-match (:error-type varjo-critical-error)
    (var-name old-value new-value)
    "Currently varjo cannot handle changing the type through a setq
due to the static nature of glsl.

var name: ~a
type-of ~a: ~a
type-of new-value: ~a"
  var-name var-name (v-type-of old-value) (primary-type new-value))

(deferror cannot-not-shadow-core () ()
    "You cannot shadow or replace core macros or special functions.")

(deferror out-var-name-taken () (out-var-name)
    "The variable name '~a' is already taken and so cannot be used~%for an out variable"
  out-var-name)

(deferror unknown-variable-type () (name)
    "Could not establish the type of the variable: ~s" name)

(deferror var-type-mismatch () (var-type code-obj)
    "Type specified does not match the type of the form~%~s~%~s"
  (primary-type code-obj) var-type)

(deferror switch-type-error () (test-obj keys)
    "In a switch statement the result of the test and the keys must all be either ints or uints:~%Test type: ~a~%Keys used: ~{~a~^,~}" (primary-type test-obj) keys)

(deferror loop-will-never-halt () (test-code test-obj)
    "The loop is using the following code as it's test.~%~a~%~%This will only ever result in a ~a which means the loop will never halt" test-code (type->type-spec (primary-type test-obj)))

(deferror for-loop-simple-expression () ()
    "Only simple expressions are allowed in the condition and update slots of a for loop")

(deferror for-loop-only-one-var () ()
    "for loops can only iterate over one variable")

(deferror invalid-for-loop-type () (decl-obj)
    "Invalid type ~a used as counter for for-loop"
  (primary-type decl-obj))

(deferror no-version-in-context () (env)
    "No supported version found in context:~%~a"
  (v-context env))

(deferror name-unsuitable () (name)
    "Names of variables and functions must start with an alpha char.~%They also may not start with 'gl-' 'fk-' or 'sym-' ~%Supplied Name: ~a~%" name)

(deferror unable-to-resolve-func-type () (func-name args)
    "Unable to resolve the result type of function '~a' when called~%with the argument types:~%~a~%"
  func-name
  (mapcar #'type->type-spec (mapcar #'primary-type args)))

(deferror out-var-type-mismatch () (var-name var-types)
    "The out variable ~a is has been set with different types.~%Types used: ~a" var-name var-types)

(deferror fake-type-global () (env)
    "fake types can not be added to the global environment")

(deferror invalid-context-symbol () (context-symb)
    "Sorry but the symbol '~a' is not valid as a context specifier" context-symb)

(deferror invalid-context-symbols () (symbols)
    "Sorry but the following symbols are not valid as a context specifiers:
~{~s~^ ~}"
  symbols)

(deferror args-incompatible () (previous-args current-args)
    "Sorry but the output arguments from one stage are not compatible with the input arguments of the next.~%Out vars from previous stage: ~a~%In args from this stage: ~a"
  previous-args current-args)

(deferror invalid-shader-stage () (stage)
    "Sorry but '~a' is not a valid shader stage" stage)

(deferror swizzle-keyword () (item)
    "Swizzle expects a keyword to specify the components. Recieved ~a instead" item)

(deferror multi-func-stemcells () (func-name)
    "Multiple functions found named ~a that match arguments.~%However varjo cannot decide which function to use because n of the arguments passed in are of stemcell type" func-name)

(deferror uniform-in-sfunc () (func-name)
    "Must not have uniforms in shader functions, only appropriate in shader stages: ~a"
  func-name)

(deferror invalid-v-defun-template () (func-name template)
    "Template passed to v-def-glsl-template-fun must be a format string:
~a~%~a~%"
  func-name template)

(deferror keyword-in-function-position () (form)
    "Keyword cannot appear in function name position : ~s"
  form)

(deferror invalid-symbol-macro-form () (name form)
    "Symbol macros must expand to a list or atom form : ~s -> ~s~%"
  name form)

(deferror stage-order-error () (stage-type)
    "stage of type ~s is not valid at this place in the pipeline, this is either out of order or a stage of this type already exists"
  stage-type)

(deferror multi-val-bind-mismatch () (bindings val-form return-set)
    "Multiple Value Bind - Number of values returned from value form does not match bindings:
Bindings: ~a
Value Form: ~a
Returned Values: ~a"
  bindings val-form (map 'list #'type->type-spec return-set))

(deferror merge-env-func-scope-mismatch () (env-a env-b)
    "Attempting to merge two environements with different function scopes ~s~%~s~%~s"
  (cons (v-function-scope env-a) (v-function-scope env-b)) env-a env-b)

(deferror merge-env-parent-mismatch () (env-a env-b)
    "Attempting to merge two environements with different parent environments ~s~%~s~%~s"
  (cons (v-parent-env env-a) (v-parent-env env-b)) env-a env-b)

(deferror env-parent-context-mismatch () (env-a env-b)
    "Attempting to make an environment with different context to it's parent ~s~%~s~%~s"
  (cons (v-context env-a) (v-context env-b)) env-a env-b)

(deferror symbol-unidentified (:error-type varjo-critical-error) (sym)
    "Symbol '~s' is unidentified." sym)

(deferror if-form-type-mismatch () (test-form then-form then-type
                                              else-form else-type)
    "The result if ~a is true is ~a which has type ~a
however the false case returns ~a which has type ~a
This is incompatible"
  test-form then-form then-type else-form else-type)

(deferror bad-make-function-args () (func-name arg-specs)
    "Trying to define the function ~s but the following argument specifications
are a bit odd:
~{~s~^~%~}

Generally arguments are defined in the format (arg-name arg-type)
e.g. (~a :vec3)"
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

(deferror none-type-in-out-vars () (glsl-name)
    "One of the values being returned from the shader (~s) is of type :none."
  glsl-name)

(deferror body-block-empty () (form-name)
    "In varjo it is not valid to have a ~s with an empty body."
  form-name)

(deferror flow-ids-mandatory (:error-type varjo-critical-error) (for primary-type)
    "~a must be given flow id/s when created: type - ~s" for primary-type)

(deferror flow-id-must-be-specified-vv (:error-type varjo-critical-error) ()
    "v-values must be given a flow id when created")

(deferror flow-id-must-be-specified-co (:error-type varjo-critical-error) ()
    "code objects must be given a flow id when created")

(deferror if-branch-type-mismatch (:error-type varjo-critical-error) (then-obj)
    "Type mismatch: else-case is nil which is of bool type, yet the then form is of ~s type."
  (type->type-spec (primary-type then-obj)))

(deferror if-test-type-mismatch (:error-type varjo-critical-error) (test-obj)
    "The result of the test must be a bool.~%~s" (primary-type test-obj))

(deferror cross-scope-mutate (:error-type varjo-critical-error) (var-name code)
    "It is illegal to assign to variables from outside the function's own scope

Tried to mutate ~s
~s"
  var-name code)

(deferror illegal-implicit-args (:error-type varjo-critical-error) (func-name)
    "Implicit args are not allowed in the function ~s" func-name)

(deferror invalid-flow-id-multi-return (:error-type varjo-critical-error)
    (func-name return-type)
    "Found a multiple-return-func ~s invalid return types:
~{~s~}"
  func-name return-type)

(defbug loop-flow-analysis-failure (:error-type varjo-critical-error) ()
    "Varjo's flow analyzer has been unable to resolve the variable flow in this
loop within a reasonable ammount of time. This counts as a compiler bug so
please report it on github")

(deferror invalid-env-vars  (:error-type varjo-critical-error) (vars)
    "Attepted to create an environment which has invalid variable data:
~s" vars)

(deferror values-safe-wasnt-safe (:error-type varjo-critical-error) (args)
    "the 'value-safe special form has been used. However the target function it
was run against had more than one argument with multi-value returns.
This is an illegal configuration. If you did not use the 'values-safe form in
your code knowingly, then contact the maintainer of the library that triggered
this. It is certainly a bug on their end.

Compiled Args:
~{~s~%~}"
  args)

(deferror empty-progn (:error-type varjo-critical-error) ()
    "progn with no body found, this is not currently allowed by varjo")

(defbug name-clash (:error-type varjo-critical-error) (lisp glsl)
    "The glsl name ~s was generated for the lisp-name ~s
However this name was already taken. This is Varjo bug, please raise an issue
on github.
Sorry for the inconvenience"
  glsl lisp)

(defbug name-mismatch (:error-type varjo-critical-error) (lisp glsl taken)
    "The glsl name ~s was generated for the lisp-name ~s
However this clashes with glsl name for the previous symbol ~s.
This is Varjo bug, please raise an issue on github.
Sorry for the inconvenience"
  glsl lisp taken)

(deferror function-with-no-return-type (:error-type varjo-critical-error)
    (func-name)
    "The function named ~s does not return anything, this is not
currently allowed. The tail position of the function must be an expression with
a return type.

Most likely you currently have one of the following in the tail position:
%if, for or while" func-name)

(deferror external-function-invalid-in-arg-types
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

(defbug invalid-special-function-arg-spec (:error-type varjo-critical-error)
    (name spec)
    "The special function named ~s has an invalid argument spec:
~a

Please report this bug on github" name spec)

(deferror closures-not-supported (:error-type varjo-critical-error)
    (func)
    "The function ~s is a closure and currently Varjo doesnt support
passing these around as first class objects.

Sorry for the odd limitation, this will be fixed in a future version."
  func)

(deferror cannot-establish-exact-function (:error-type varjo-critical-error)
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

(deferror uniform-in-cmacro () (name)
    "We do not currently support &uniforms args in compiler macros, only in shader stages: ~a"
  name)

(deferror optional-in-cmacro () (name)
    "We do not currently support &optional args in compiler macros: ~a"
  name)

(deferror key-in-cmacro () (name)
    "We do not currently support &key args in compiler macros: ~a"
  name)

(deferror no-types-for-regular-macro-args () (macro-name arg)
    "The type of the argument ~a is unknown at this stage.

The macro named ~a is a regular macro (defined with v-defmacro). This means
that, the arguments passed to it are uncompiled code and as such, we cannot get
their types.

It is however possible to retrieve the argument types for compiler-macros."
  arg macro-name)

(deferror no-metadata-for-regular-macro-args () (macro-name arg)
    "The metadata for the argument ~a is unknown at this stage.

The macro named ~a is a regular macro (defined with v-defmacro). This means
that, the arguments passed to it are uncompiled code and as such, we cannot get
any metadata about them.

It is however possible to retrieve the metadata of arguments in compiler-macros."
  arg macro-name)

(deferror no-tracking-for-regular-macro-args () (macro-name arg)
    "The flow information for the argument ~a is unknown at this stage.

The macro named ~a is a regular macro (defined with v-defmacro). This means
that, the arguments passed to it are uncompiled code and as such, we cannot
trace where they have come from.

It is however possible to retrieve this data for arguments in compiler-macros."
  arg macro-name)

(deferror unknown-macro-argument () (macro-name arg)
    "Could not find an argument named ~a in the macro ~a"
  arg macro-name)

(deferror symbol-macro-not-var () (callee name)
    "~a was asked to find the value bound to the symbol ~a, however ~a is
currently bound to a symbol-macro."
  callee name name)

(deferror unbound-not-var () (callee name)
    "~a was asked to find the value bound to the symbol ~a, however ~a is
currently unbound."
  callee name name)

(deferror not-proved-a-uniform (:error-type varjo-critical-error) (name)
    "We are unable to prove that ~a has come from a uniform"
  name)

(deferror duplicate-varjo-doc-string () (form dup)
    "We have found an illegal duplicate docs string.

Doc string: ~s

Found in form:
~s"
  dup form)

(deferror calling-declare-as-func () (decl)
    "Found a declare expression in an invalid position.

Declaration: ~s

There is no function named DECLARE. References to DECLARE in some contexts (like
the starts of blocks) are unevaluated expressions, but here it is illegal."
  decl)

(deferror treating-declare-as-func () (decl)
    "Found an attempt to take a reference to declare as a function.

Form: ~s

There is no function named DECLARE. References to DECLARE in some contexts (like
the starts of blocks) are unevaluated expressions, but here it is illegal."
  decl)

(deferror v-unsupported-cl-declaration () (decl)
    "Found an unregonised declaration named ~a

Whilst this is valid in standard common-lisp, it is not currently valid in
Varjo.

Full Declaration: ~s"
  (first decl) decl)

(deferror v-only-supporting-declares-on-vars () (targets)
    "We found the following invalid names in the declarations:
~@[~{~%~s~}~%~]
We don't yet support declarations against functions or symbol-macros.
Sorry for the inconvenience."
  targets)

(deferror v-declare-on-symbol-macro () (target)
    "We found a declaration against ~s. However at this point in the
compilation, ~s is bound to a symbol-macro and Varjo does not support
declarations against symbol-macros."
  target target)

(deferror v-declare-on-nil-binding () (target)
    "We found a declaration against ~s. However at this point in the
compilation, ~s is not bound to anything."
  target target)

(deferror v-metadata-missing-args () (name required provided missing)
    "The metadata type ~a requires the following args to be specified
on creation: ~{~a~^, ~}

However, the following was provided instead: ~s

Please provide values for: ~{~a~^, ~}

It is perfectly legal to set the values to nil, but we require them to be
declared to something."
  name required provided missing)

(defwarning cant-shadow-user-defined-func () (funcs)
    "Unfortunately we cannot currently shadow user-defined functions.
The following functions have been skipped:~{~%~s~}"
  funcs)

(defwarning cant-shadow-no-type-match () (shadowed funcs)
    "Was asked to shadow the following functions, however none of the
arguments have the type ~a

The following functions have been skipped:~{~%~s~}"
  shadowed funcs)

(deferror shadowing-user-defined-func () (func)
    "Unfortunately we cannot currently shadow user-defined functions.
The function in question was: ~s"
  func)

(deferror shadowing-no-type-match () (shadowed func)
    "Was asked to shadow the following function, however none of the
arguments have the type ~a

The function in question was: ~s"
  shadowed func)

(deferror shadowing-no-return-matched () (shadowed func)
    "Was asked to shadow the following function, however none of the
returned values have the type ~a

The function in question was: ~s"
  shadowed func)

(deferror shadowing-multiple-constructors () (shadow-type func-id funcs)
    "Was asked to shadow the function with the idenifier ~a  as a
constructor for the shadow-type ~a.

However this function-identifier names multiple functions, which is not
allowed in this form.

The functions in question were:~{~%~a~}"
  func-id shadow-type funcs)

(deferror shadowing-multiple-funcs () (shadow-type pairs)
    "Was asked to shadow the functions for the shadow-type ~a.

However these function-identifiers name multiple functions, which is not
allowed in this form.

The functions in question were:
~{~%Identifier: ~a~%Named functions: ~a~%~}"
  shadow-type pairs)

(deferror shadowing-constructor-no-match () (shadow-type func-id)
    "Was asked to shadow the function with the idenifier ~a as
a constructor for the shadow-type ~a.

However no functions were found that matched this identifier."
  func-id shadow-type)

(deferror def-shadow-non-func-identifier () (name func-ids)
    "~a was ask to shadow some functions, however the following
identifiers are have problems:
~{~%~s~}

The identifiers passed to this macro should be in the format:
#'func-name  - or - #'(func-name arg-type arg-type)}"
  name func-ids)

(deferror shadowing-funcs-for-non-shadow-type () (name shadow-type)
    "~a was ask to shadow some functions for the type ~a,
however the type ~a is not a shadow type."
  name shadow-type shadow-type)

(deferror fell-through-v-typecase () (vtype wanted)
    "~a fell through V-ETYPECASE expression.
Wanted one of the following types: ~s}"
  vtype wanted)

(deferror metadata-conflict () (metadata-kind flow-id new-meta old-meta)
    "~a metadata already found for flow-id ~a.~%Metadata cannot be redefined

Tried to apply: ~a

Metadata already present: ~a"
  metadata-kind flow-id new-meta old-meta)

(deferror metadata-combine-invalid-type () (expected found)
    "Asked for a combined version of the two pieces of metadata of kind
~a, however the metadata returned was of type ~a. When combining metadata it is
only valid to return nil or a piece of metadata of the correct type."
  expected found)

(deferror multiple-external-func-match () (name matches)
    "Multiple externally defined functions found matching the
identifier ~a:

Matches:
~{~s~^~%~}"
  name matches)

(defbug doesnt-have-dimensions () (vtype)
    "Compiler bug: Attempted to find the dimensions of ~a. If you have
the time, please report this on github."
  vtype)

(deferror cannot-swizzle-this-type () (vtype)
    "Was asked to swizzle a value with the type ~a

However is not a type that can be swizzled. ~a"
  (type->type-spec (v-true-type vtype))
  (if (typep vtype 'v-struct)
      "Perhaps you meant one of this struct's slots?"
      ""))

(deferror dup-name-in-let () (dup-name)
    "The variable ~a occurs more than once in the LET."
  dup-name)

(deferror dup-names-in-let () (names)
    "The following variables occur more than once in the LET
~{~s~}"
  names)

(deferror uninitialized-var () (name)
    "Cannot access the variable ~a as it is currently unbound"
  name)

(deferror global-uninitialized-var () (name)
    "Cannot declare ~s as an unbound variable at global scope"
  name)

(defbug nil-return-set () (form possible-set)
    "Could not establish return-set.

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic form:
~a

Possible Set: ~a" form possible-set)

(defbug nil-emit-set () (form possible-set)
    "Could not establish emit-set.

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic form:
~a

Possible Set: ~a" form possible-set)

(defbug with-fresh-env-scope-missing-env () ()
    "with-fresh-env-scope expects a code object & an environment to
be returned from it's body. However there was no environment returned.")

(deferror stage-primary-type-mismatch () (stage-kind type-found type-expected)
    "The primary return value from ~a must be a ~a.
Instead ~a was found"
  stage-kind
  (type->type-spec type-expected)
  (type->type-spec type-found))

(deferror multi-dimensional-array () (dimensions)
    "We do not yet support multidimensional arrays.
However you can have arrays of arrays.

Problematic dimensions: ~a"
  dimensions)

(deferror make-array-mandatory-args () (args)
    "Cannot compile make-array as required arguments are missing.

Required args: dimensions & element-type
Found args: ~s"
  args)

(deferror make-array-conflicting-args () (args)
    "Cannot compile make-array as found that both initial-element
and initial-contents were specified.

Found args: ~s"
  args)

(deferror make-array-conflicting-lengths () (dims initial-contents)
    "Cannot compile make-array as the declared dimensions did not
match the length of the initial-contents that were specified.

Declared dimensions: ~a
initial-contents: ~s"
  dims initial-contents)

(deferror make-array-cant-cast-args () (element-type initial-contents)
    "Cannot cast the provided initial-contents to the specified
element-type.

element-type: ~a
initial-contents: ~s"
  element-type initial-contents)

(deferror make-array-cant-establish-default-value ()
    (element-type initial-contents)
    "Cannot establish the default element value for the type:
~s"
  element-type)

(deferror should-be-quoted () (thing val)
    "Varjo expected ~a to be quoted, however it was not.
Found: ~s"
  thing val)

(deferror should-be-constant () (thing val)
    "Varjo expected ~a to be constant, however it was not.
Found: ~s"
  thing val)

(deferror stage-in-context () (context)
    "It is not longer valid for the stage to be declared in the context
Please simply use #'v-compile or use #'make-stage with #'rolling-translate if
you need more control.

Context found: ~s" context)

(deferror invalid-stage-kind () (kind)
    "make-stage called with the invalid stage kind ~s"
  kind)

(deferror no-primitive-found () (stage)
    "Could not establish primitive type for ~a.
Stage: ~a"
  (type-of stage)
  stage)

(deferror invalid-primitive-for-geometry-stage () (prim)
    "~s is not a valid primitive kind for geometry stages. Instead try
points, lines, lines-adjacency, triangles or triangles-adjacency." prim)

(deferror invalid-primitive-for-tessellation-stage () (prim)
    "~s is not a valid primitive kind for tessellation stages. You must use a
patch e.g. (:patch 4) instead" prim)

(deferror rolling-translate-invalid-stage () (invalid)
    "rolling translate expects a list of stages as it's first argument.
However it found ~a invalid ~a in the list:~{~%~s~}"
  (if (> (length invalid) 1) "these" "this")
  (if (> (length invalid) 1) "elements" "element")
  invalid)

(deferror couldnt-convert-primitive-for-geometry-stage () (prim prev-stage)
    "A primitive of type ~a came from the ~a stage, unfortunately we
weren't sure how to convert this to a primitive kind the geometry shader can
use."
  prim prev-stage)

(deferror test-translate-failed () (grouped-errors)
    "Compilation Failed:
~{~%~a~%~}"
  (mapcar (lambda (grp) (format nil "Stages:~{ ~a~}~%~a" (car grp) (cdr grp)))
          grouped-errors))

(deferror returns-in-geometry-stage () (return-set)
    "In geometry stages please do not return values from the main stage
function. Instead use #'emit-data & then #'emit-vertex to attach the data to
the vertex itself.

Like #'return, #'emit-data can handle multiple values.

Found the following return values from the main stage function:~{~%~a~}"
  (map 'list #'type->type-spec return-set))

(deferror emit-not-in-geometry-stage () (stage emit-set)
    "emit, emit-data & friends are only valid in geometry stages, found it's
usage in a ~a stage

Found the following emitted values from the main stage function:~{~%~a~}"
  (type-of stage)
  (mapcar #'type->type-spec emit-set))

(deferror primitives-dont-match () (out-stage out in-stage in)
    "The primitives leaving the ~a stage are of type ~a, however the
~a was expecting ~a"
  out-stage out in-stage in)

(deferror tessellation-control-expects-patches () (primitive)
    "Tried to compile a tessellation-control stage however recieved a primitive
of type ~a rather than a patch."
  (type-of primitive))

(deferror tessellation-evaluation-invalid-primitive () (primitive)
    "Tessellation Evaluation stages can tessellate to points, triangles, quads
or iso-lines. Found ~a instead"
  (type-of primitive))

(deferror inline-glsl-vertex-stage-not-supported () ()
    "Inline glsl vertex stages are not yet supported.")

(deferror clashes-found-between-input-and-output-names ()
    (stage-kind inputs outputs clashes)
    "When compiling a GLSL ~a stage duplicate names we found in the inputs and
outputs. This is not allowed.

In Arguments: ~s
Out Values: ~s

Clashes: ~s
" stage-kind inputs outputs clashes)

(defbug user-func-invalid-x () (kind name args)
    "Invalid types found in internal user-function construction.

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic Definition:
NAME: ~s
~a: ~s" name kind args)

(deferror invalid-inline-glsl-stage-arg-layout () (arg)
    "Invalid arg layout found in glsl stage. The correct layout for a argument
to a glsl-stage is (\"string-name-of-arg\" arg-type ,@keyword-qualifiers)

Problematic arg was: ~a"
  arg)

(defbug return-set-mismatch () (form)
    "Type & Return sets don't match

Our apologies for this mistake. If you have the time please raise an issue at
https://github.com/cbaggers/varjo/issues including the code that triggered this
issue.

Problematic Definition:
~s" form)

(deferror funcall-of-special-operator () (code)
    "Cannot FUNCALL ~a as it is a special operator"
  code)

(deferror slot-value-on-non-struct () (type slot-name)
    "Was asked to access the slot ~a on ~a, however this is not a struct"
  slot-name
  (type->type-spec type))

(deferror slot-not-found () (type slot-name)
    "Could not find a slot named ~a on struct of type ~a"
  slot-name
  (type->type-spec type))

(deferror recursive-function-call-detected () (func)
    "Recursive function call detected however recursion (both direct and
indirect) is disallowed by GLSL.

Problematic function: ~s" func)

(deferror probable-recursion () (name func)
    "Compile progress looks suspiciously like ~a is recursive.
Recursion (both direct and indirect) is disallowed by GLSL.

func: ~s

The current check for this in Varjo is currently very weak if you have received
this error and do not think you have recursion in your code please raise an
issue at https://github.com/cbaggers/varjo/issues including the code that
triggered this error." name func)

(deferror invalid-function-arg-format () (name arg)
    "The argument form ~a in the function ~a is invalid.

Expected (arg-name arg-type)"
  arg name)

(deferror underspecified-patch-primitive () ()
    "Whilst 'patches' are a valid form of primitive, the way they are defined
in Varjo is (:patch *) where '*' is the number of vertices in the patch.

For example: (:patch 2) (:patch 3) (:patch 4)")

(deferror non-place-assign () (glsl-op place val)
    "You cannot assign this: ~a ~%This was attempted as follows ~a ~a ~a"
  (current-line place)
  place glsl-op val)

(deferror v-unrecognized-declaration () (decl)
    "Found an unregonised declaration named ~a
~@[~%Might you have meant one of these?:~{~%~s~}~%~]
Full Declaration: ~s"
  (first decl) (find-alternative-declaration-kinds (first decl)) decl)

(deferror v-def-template-arg-mismatch () (args types)
    "v-def-template-arg-mismatch found a mismatch between
~s
and
~s"
  args types)

(deferror illegal-&rest-in-args () (func-name arg-specs)
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

(deferror cannot-take-reference-to-&rest-func () (func-name)
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

(deferror attempted-transform-feedback-in-fragment-shader () ()
    "The fragment stage in this pipeline was found to contain the :feedback
qualifier.

Transform feedback is only valid in the vertex processing stages and thus
:feedback can only be used from :vertex, :tessellation-control,
:tessellation-evaluation or :geometry stages.")

(deferror transform-feedback-incorrect-stage () (stage)
    "The :feedback qualifier was found in the ~a stage.

This is invalid in this case because only the last vertex processing stage is
allowed to use transform feedback."
  (etypecase stage
    (compiled-vertex-stage "vertex")
    (compiled-tessellation-control-stage "tessellation-control")
    (compiled-tessellation-evaluation-stage "tessellation-evaluation")))

(deferror invalid-feedback-qualifier-form () (form)
    "The :feedback qualifier may be provided just as a keywork or as
a form with a single positive integer specifying the group e.g. (:feedback 0)

However in this case we found ~s which is invalid"
  form)

(deferror invalid-primitive-for-compute-stage () (prim)
    "Whilst compiling a compute stage it was found that primitive had been set
to something other than nil. This is invalid for this stage. Please use nil
instead.

Primitive found: ~a"
  prim)

(deferror compute-pipeline-may-only-contain-one-stage () (stages)
    "A attempt was made to compile a pipeline with the following stages:
~{~a~^, ~}

However, if you are compiling a pipeline that contains a compute stage there
may not be any other stages in that pipeline."
  stages)

(deferror stage-must-have-output-prim-declaration () (stage)
    "The function used as a geometry stage must have a top level
output-primitive declaration

Stage: ~a" stage)

(deferror stage-must-have-output-patch-declaration () (stage)
    "The function used as a tessellation control stage must have a top level
output-patch declaration

Stage: ~a" stage)

(deferror stage-must-have-local-size-declaration () (stage)
    "The function used as a compute stage must have a top level local-size
declaration.

Stage: ~a" stage)

(deferror compute-stage-with-in-args () (args)
    "We were asked to compile a compute stage but have found that it has input
arguments defined for it. GL does not allow input arguments for compute stages,
only uniforms.

Args found: ~a" args)

(deferror uniform-ubo-and-ssbo () (arg)
    "Whilst making a stage object we found a uniform argument which is
specified to be both a UBO and a SSBO. It is not valid for a uniform to be
both of these.

Uniform Argument: ~s" arg)

(deferror ubo-ssbo-type-limitation () (type)
    "Currently UBOs and SSBOs in Varjo must be structs.

If this is impeding your work please feel free to file an issue
at https://github.com/cbaggers/varjo/issues")

(deferror compute-stage-must-be-void () (name returns)
    "Compute stages must return (values) however ~a returns:
~a"
  (or name "this lambda")
  returns)

(deferror unknown-layout-specifier () (target-kind name specifier)
    "Unknown layout specifier used for the ~a uniform named ~a.
Was expecting one of std-140 or std-430. Recieved ~a instead."
  target-kind name specifier)

(deferror find-mutual-type-bug () (types)
    "BUG: find-mutual-cast-type take a list of types, not type specs
~a" types)

(deferror void-type-for-if-test () (form)
    "The test for an 'if' expression cannot be :void.

 (if ~a
     ..)"
  form)

(deferror discarded-for-if-test () (form)
    "The expression used as the test for one of your 'if' expression is just
discarding the fragment. We arent sure what to emit for this 'if'.

 (if ~a
     ..)"
  form)

(deferror discard-not-in-fragment-stage () (stage)
    "The discard expression was found in a ~a. This is invalid as it is only
allowed in fragment stages" stage)

(deferror opaque-data-found () (arg-name type-spec)
    "The argument ~a has type ~a which contains opaque data.
This is invalid in this context"
  arg-name type-spec)

(deferror invalid-output-primitive-for-geometry () (kind)
    "The output primitive for the geometry stages was set to be ~s,
however the only valid option is one of the following:

- :points
- :line-strip
- :triangle-strip" kind)

(deferror let-void () (name)
    "Found an invalid attempt to make a local variable ~a with type :void"
  name)

(deferror let-discarded () (name)
    "Found an attempt to 'let' a local variable called ~a, however the form
providing the value has discarded the fragment and as such will never return.

Due to this CEPL can't infer a valid type for this form and this is triggering
this issue in 'let'"
  name)

(deferror let-or () (name type)
    "Found an invalid attempt to make a local variable ~a with the
type ~a

This usually happens due to the use of a conditions (like an 'if') where the
different branches of the conditional have different types. Due to this CEPL
can't infer a single valid type for this form and this is triggering this
issue."
  name (type->type-spec type))

(deferror if-form-multiple-vals-mismatch () (then-set else-set)
    "Found an if form where the number of values returned on each branch
do not match.

The then branch returned: ~s
The else branch returned: ~s

This matters as the result of this 'if' is going to be used as a return from a
function or by a multiple-value-bind form"
  (or (map 'list #'type->type-spec then-set) :void)
  (or (map 'list #'type->type-spec else-set) :void))

;;
;; Hi! Don't forget to add the name of your condition to the
;; varjo.conditions package
;;
