## Language Features

- Static typing
- Basic type propagation (minimizes the need to write the types)
- Statically resolved first class functions (this means that as long and the compiler can work out the exact function at all call sites you can pass the function)
- Macros
  - `defmacro`, `define-compiler-macro`, `macrolet`, `symbol-macrolet` all supported
  - `&environment` supported (currently custom API for introspection but [cltl2](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html) API planned)
- Multiple-value return via `values`, `multiple-value-bind`, `multiple-value-call`, etc
- local functions via `labels`
- Functions & Structs defined outside of the shader which allows increase reuse and composability
- inline GLSL expressions
- Type & Primitive propagation & checking between shader stages.
