### Umbra
Umbra is a Lisp to GLSL Language translator. It will allow you to write vertex, fragment & geometry shaders for any version of glsl greater than or equal to 1.20 (opengl version 2.1).

This is currently WIP and is not ready for use.

#### Features
* Support for GLSL Version 1.30 (opengl 3.0). This will be expanded on as development continues.
* Type Checking
* GLSL Macros
* Lisp Function Substitution: You can define how existing lisp functions map onto glsl functions. This means you can blur the line between lisp code and umbra code without having to shadow symbols. For example the cl:expt function maps to the umbra:pow function.
* -More to come-