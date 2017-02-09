### Varjo

Varjo is a Lisp to GLSL compiler.

Varjo has no OpenGL dependency as is designed to be integrated into other projects, see CEPL for an example.

#### Features

Subset of common lisp. Including:

- Macros
 - `defmacro`, `define-compiler-macro`, `macrolet`, `symbol-macrolet` all supported
 - `&environment` supported (currently custom API for introspection but [cltl2](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html) API planned)
- Multiple-value return via `values` & `multiple-value-bind`
- local functions via `labels`
- `defstruct` which can be used across multiple shaders (Less repeated code)
- limited first-class function support (Functions have to be statically resolved to avoid runtime conditionals)
- inline GLSL expressions
- WIP for `declare` and extensible declaration identifiers
- Type checking in and across shader stages
- Rolling translate, where out vars from one stage can be automatically fed into the next
- Easy to extend (API still in progress)
