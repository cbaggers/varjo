### Varjo [BETA]

Varjo is a Lisp to GLSL compiler.

Varjo has no OpenGL dependency as is designed to be integrated into other projects, see CEPL for an example.

Vari is the dialect of lisp Varjo compiles. It aims to be as close to Common Lisp as possible, but naturally it is statically typed so there are differences.

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

### Documentation

The user guide can be found in [docs/user-guide.md](docs/user-guide.md)

The reference documentation for the compiler can be found [here](http://techsnuffle.com/varjo/varjo-reference.html)

The reference documentation for the Vari language can be found [here](http://techsnuffle.com/varjo/vari-reference.html)

### Installing

Run `(ql:quickload :varjo)` at your REPL.

### Cloning

Whist it is recommended to get Varjo from quicklisp, if you clone please note that `master` is not the stable branch. Please use `release-quicklisp` for the stable code that will be in the next Varjo release.

#### Name Origins

- Varjo is 'shadow' in Finnish
- Vari is 'shadow' in Estonian

### Updates

#### 2018-04-05

- Tonnes of improvements to docs (see the links in the 'Documentation' section above)
- `vari-describe`, which is function that given a symbol that names a gpu func/var gives you documentation (check out vari-describe.el for emacs integration)
- Lots more glsl operators are supported. WARNING we now have pre & post vector/matrix multiplication, please remember that the order matters (just like in regular glsl). To use the glsl specific operators (e.g. ~ ! & etc) please `:use` the `:glsl-symbols.operators` package in your project.

#### 2018-03-14

Lots landing this month, whilst my current test suite passes I'm still expecting some issues or regressions, if these hit you PLEASE let me know so I can avoid merging this to quicklisp before the regressions have been handled. Quick summary of changes are:

- Documentation! (see the Documentation section above for the links)
- GLSL 460 fixes
- compute bug fixes
- tonnes of functions from the CL spec added (see below)
- swizzle of rgba & stpq
- more closely mimick the CL type tree (Still lots to be done here)
- fix long standing bug which stopped you passing ubo/ssbo structs to functions (due to them being represented by interface blocks)
- fix max array length bug
- dont emit spurious function & struct glsl in various cases. Wasnt hurting anything except glsl compile time, but was still ugly.
- internal environment object now uses the uniform-variable objects rather than lists
- qualifier refactor to reduce duplication of information
- some more validation checks around ubos & ssbos (though the validation code is scattered and needs a cleanup)
- internal macro renaming
- def-metadata-infer deprecated, use define-metadata-infer instead
- def-metadata-kind deprecated, use define-metadata-kind instead
- def-shadow-type-constructor deprecated, use define-shadow-type-constructor instead
- :int32 & :uint32 type names now work as expected
- improvements to slot-value
- fix bug letting you assign to a read-only place
- perf improvements
- [BREAKING CHANGE] Removed ast (non trivial ammount of cons and not used for compile)
- [BREAKING CHANGE] Removed the unused *draw-modes* variable

CL functions added listed below. Some of the predicates are basically pointless, but for completeness it's nice to have them

- symbolp
- keywordp
- row-major-aref
- svref
- array-rank
- array-row-major-index
- array-total-size
- adjustable-array-p
- array-has-fill-pointer-p
- arrayp
- bit-vector-p
- simple-bit-vector-p
- simple-vector-p
- vectorp
- compiled-function-p
- functionp
- 1+
- 1-
- cis
- float (add optional args)
- integer-length
- signum
- conjugate
- random-state-p (though just false for now)
- complexp
- evenp
- floatp
- integerp
- minusp
- numberp
- oddp
- plusp
- rationalp
- realp
- zerop
- copy-structure
- prog2
- dotimes
- typecase
- phase
- isqrt
- logand
- logandc1
- logandc2
- logcount
- logeqv
- logior
- lognand
- lognor
- lognot
- logorc1
- logorc2
- logtest
- logxor
- with-accessors
- multiple-value-setq
- nth-value

Also added the complex & ratio types. Complex support is limited right now and ratio basically useless. But we have a place to work from.
Complex numbers are always single-float in vari.

#### 2017-06-04

**PACKAGES**

There has been a huge refactoring of packages.

This is annoying but a neccessary step before I could start documenting the project. The original structure just appeared out of neccessity so I'm hoping this will last a little longer. The most important changes are:

- Our lisp dialect has a name. The naming around Varjo and it's lisp dialect were confusing people so I have named the dialect `Vari`, `Varjo` remains as the name of the compiler.

- `varjo-lang` renamed to `vari`. You will need to update your package `:use`ings to pull in `vari` and not `varjo-lang`.

- No more dependency on rtg-math! If you want to use rtg-math's functions in your shaders please quickload `rtg-math.vari`

- The `varjo` package now only exports symbols for interacting with the compiler.

- There are places where GLSL & Common Lisp's function names overlap. This is fine when the behaviour is also the same but this is not always the case. For this reason we have a `vari.cl` package and a `vari.glsl` package. When there are conflicts (that people care about) we will put the different versions in the respective packages and `vari` will use the most common case.

**Bugfixes & Enhancements**

- `if` will now generate a ternary expression (e.g. `x<0 ? -1 : 1`) if the followiung is true
  - there is an 'else' form
  - both the 'then' & 'else' forms return a value of the same type
  - both the 'then' & 'else' forms are pure
  - neither the 'then' & 'else' forms return multiple values
  - there is also a 'style' restriction that means if the glsl code for the expression goes over 100 characters then it will emit a regular `if` block. This is just to keep code readable.

- If you use `->` in a symbol it will become `_to_` in glsl. So `int->float` becomes `int_to_float`

- Fix bug where, if the 'then' clause of an `if` contained a progn, that code would be missing from the glsl.

- Redamentary checks for purity of an expression (whether it has side effects). This feature is currently young and over conservative

- Better error messages when assigning the wrong type to a place

- Better error message for invalid argument format in function definition

- Add the `dynamic` draw mode. Used when you dont know (or don't want to specify) the primitive for the compile. This will only work for vertex & fragment stages but will throw a sensible error if passed to anything else.

- `vec2`, `vec3`, `vec4`, `mat3` & `mat4` constructors

- Fix bug where `#'` would get confused in the presence of local & external functions

#### 2017-05-16

- Tesselation & Geometry stages are now fully supported (for real this time :D)

- Very basic mutual recursion checks.

- Fix bug where inlining and indirect recursion could cause compiler to infinite loop

- Make use of `{}` braces consistent in emitted glsl

- Add `continue`

- Add `/=`

- Add `with-slots` & `with-accessors`

- Add `multiple-value-call`

- strings containing floats will be spliced into the emitted glsl unchanged (very useful when float used for bits & you dont want the risk of lisp rounding something)

- strings containing integers will be turned integer literals (only here for completeness with above code) doesnt not yet detect signed'ness of int.

- make the value argument to `incf`, `decf` etc optional (defaulting to 1)

- Better errors when can't find function match

- Don't allow `funcall` on special operators. Give better error message

- Fix bad glsl emitted for tail-calls to functions returning multiple values

- Fix `funcall` so it handles multiple values correctly (dumb mistake :p)

- Add the stage name (as a comment) to the generated glsl.

- Large amount of refactoring how types are stored/propegated

- `v-defun` renamed to `v-def-glsl-template-fun`, `v-defun` will be used for functions with lisp code.
