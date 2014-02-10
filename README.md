### Varjo

> NOTE: As of 10/02/2013 I am away for a month and a bit. 
> This project is not dead and I will in fact I have a 
> lot in the works for this and Cepl. 
> Hope this find you well. Ciao
>
> Baggers

Varjo is a Lisp to GLSL Language translator. It will allow you to write vertex, fragment & geometry shaders for any version of glsl greater than or equal to 1.20 (opengl version 2.1).

This is currently WIP and is not ready for use.

#### Features
* Support for GLSL Version 1.30 (opengl 3.0). This will be expanded on as development continues.
* Type Checking
* GLSL Macros
* Lisp Function Substitution: You can define how existing lisp functions map onto glsl functions. This means you can blur the line between lisp code and varjo code without having to shadow symbols. For example the cl:expt function maps to the varjo:pow function.
* -More to come-
