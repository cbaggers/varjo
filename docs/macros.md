# Macros in Vari

Macros works much like they do in Common Lisp.

## Local Macro Definitions

The following locally scoped macro kinds have the same names as in CL

- `macrolet`
- `symbol-macrolet`

The syntax and behavior is the also the same

## Top Level Macro Definitions

In order not to clash with Common Lisp, Vari's variants of top level macros have different names:

- `define-vari-macro` is Vari's version of `defmacro`
- `define-vari-compiler-macro` is Vari's version of `define-compiler-macro`

Other than the naming, Vari's variants of the macros behave in the same way as their Common Lisp cousins.

Vari does not yet have an equivalent of `define-symbol-macro`

## Reader Macros

Vari naturally shares the reader with Common Lisp and as such has no additions here.

## A Note on Expansion

Varjo is at leave to expand a given macro for the same form any number of times. As such it is recommended to avoid use of global state or non-deterministic functions in the creation of your expansion as you will not be able to control what the output will be.
