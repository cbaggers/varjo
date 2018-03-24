(in-package :varjo.api)

(docs:define-docs
  ;;
  ;; environment
  (defun add-lisp-form-as-uniform
"
`add-lisp-form-as-uniform` is an intersting beast. Its goal it to make it easy
to inject uniforms into the resulting GLSL which will be populated by the
result of a specific Common Lisp form.

You provide:
- a value which will be assumed to be a valid Common lisp form
- a Vari `type-spec` which will be the type of the uniform
- a Varjo environment object, which must have been obtained from the `
  &environment` paramater in a Vari macro.
- an optional name

Varjo will then add a uniform (as it would be using uniform list in
`make-stage`). You can then use the symbol provided as the 'name' argument
in your Vari code.

When the compilation completes you can query the `compiled-stage` object using
the `implicit-uniforms` function which will return a list of
`implicit-uniform-variable` objects which can be queried using the following
functions:

- `name` - the vari name of the uniform
- `glsl-name` - returns a string with the glsl name of the uniform variable
- `v-type-of` - Returns the Varjo type object
- `cpu-side-transform` - this is how you obtain the Common Lisp code that
                         was passed to `add-lisp-form-as-uniform`
")
  (defun all-bound-symbols
"
When given an `environment` object from a Vari macro `all-bound-symbols`
returns a list of all the symbols currently bound.
")
  (defun argument-is-uniform-p
"
When given an `environment` object from a Vari macro and a symbol that names
one of the arguments to the compiler-macro, `argument-is-uniform-p` returns T
if the value of the argument comes from a uniform variable.
")
  (defun argument-type
"
When given an `environment` object from a Vari macro and a symbol that names
a variable bound in the current scope, `argument-type` returns the Varjo
type object that represents the Vari type of that variable.
")
  (defun argument-uniform-name
"
When given an `environment` object from a Vari macro and a symbol that names
an argument to the current compiler-macro, `argument-uniform-name` returns
the the symbol which names the uniform in the stage if the value passed to
the named paramater has come from a uniform.
")
  (defun metadata-for-argument
"
When given an `environment` object from a Vari compiler-macro and a symbol
that names one of the arguments to the macro[0], `metadata-for-argument`
returns an object that holds the compile time metadata for the value

[0] the argument cannot be the &whole or &environment argument.
")
  (defun metadata-for-variable
"
When given an `environment` object from a Vari compiler-macro and a symbol
that names a variable currently in scope to the macro[0],
`metadata-for-variable` returns an object that holds the compile time metadata
for the value bound to that variable.

[0] the argument cannot be the &whole or &environment argument.
")
  (defun variable-in-scope-p
"
When given an `environment` object from a Vari compiler-macro and a symbol,
`variable-in-scope-p` returns T if that symbol names a variable that is
currently in scope.
")
  (defun variable-is-uniform-p
"
When given an `environment` object from a Vari macro and a symbol,
`variable-is-uniform-p` returns T if that symbol names a uniform variable
in the current scope, or it contains the value from a uniform variable.
")
  (defun variable-type
"
When given an `environment` object from a Vari macro and a symbol that names
a variable in the current scope, `variable-type` returns a Varjo type object
that represents the Vari type of the value bound to the variable.
")
  (defun variable-uniform-name
"
When given an `environment` object from a Vari macro and a symbol that names
a variable whos value is taken from a uniform, `argument-uniform-name` returns
the the symbol which names the uniform in the stage.
")
  (defun variables-in-scope
"
When given an `environment` object from a Vari macro, `variables-in-scope`
returns a list contains the names of all the variables currently in scope.
"))
