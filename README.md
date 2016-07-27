### Varjo

Varjo is a Lisp to GLSL Language compiler.

Varjo has no OpenGL dependency as is designed to be itegrated into other projects, see CEPL for an example.

#### Features

* Subset of common lisp. Including:
- Macros (Regular and Compiler - Reader macros work anyway)
- Multiple-value return
- local functions via "labels"
- defstruct which can be used across multiple shaders (Less repeated code)

* Type Checking - Works across shader stages

* Rolling translate, where out vars from one stage can be automatically fed into the next

* Easy to extend
