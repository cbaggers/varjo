;;;; varjo.core.asd

(asdf:defsystem #:varjo
  :description "Common Lisp -> GLSL Compiler"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:documentation-utils
               #:vas-string-metrics
               #:named-readtables
               #:split-sequence
               #:glsl-symbols
               #:parse-float
               #:alexandria
               #:glsl-spec
               #:glsl-docs
               #:cl-ppcre
               #:uiop
               #:fn)
  :components ((:file "package")
               (:file "src/utils-v")
               (:file "src/defcondition")
               (:file "src/varjo.internals/generics")
               (:file "src/varjo.internals/globals")
               (:file "src/varjo.internals/names")
               (:file "src/varjo.internals/internal-types")
               (:file "src/varjo.internals/glsl-generation/line-and-chunk")
               (:file "src/varjo.internals/qualifiers")
               (:file "src/varjo.internals/types/def-v-core-type")
               (:file "src/varjo.internals/types/type-spec")
               (:file "src/varjo.internals/types/early-types")
               (:file "src/varjo.internals/flow")
               (:file "src/varjo.internals/types/types")
               (:file "src/varjo.internals/return-set")
               (:file "src/varjo.internals/emit-set")
               (:file "src/varjo.internals/value-metadata")
               (:file "src/varjo.internals/errors")
               (:file "src/varjo.internals/function-obj")
               (:file "src/varjo.internals/deftype")
               (:file "src/varjo.internals/variables")
               (:file "src/varjo.internals/code-object")
               (:file "src/varjo.internals/stemcells")
               (:file "src/vari.types/types")
               (:file "src/vari.types/other-types")
               (:file "src/varjo.internals/environment")
               (:file "src/varjo.internals/external-functions")
               (:file "src/varjo.internals/structs")
               (:file "src/varjo.internals/glsl-generation/string-generation")
               (:file "src/varjo.internals/casting")
               (:file "src/varjo.internals/stages")
               (:file "src/varjo.internals/compile-result")
               (:file "src/varjo.internals/compile-literal")
               (:file "src/varjo.internals/compile-vars")
               (:file "src/varjo.internals/compile-special")
               (:file "src/varjo.internals/make-function")
               (:file "src/varjo.internals/functions")
               (:file "src/varjo.internals/traits")
               (:file "src/varjo.internals/macros")
               (:file "src/varjo.internals/compile-funcall")
               (:file "src/varjo.internals/compile-form")
               (:file "src/varjo.internals/declaim")
               ;;
               (:file "src/vari.glsl/equality")
               (:file "src/vari.glsl/matrix-constructors")
               (:file "src/vari.glsl/scalar-constructors")
               (:file "src/vari.glsl/vector-constructors")
               (:file "src/vari.glsl/parse-from-spec")
               (:file "src/vari.glsl/variables-from-spec")
               (:file "src/vari.glsl/functions-from-spec")
               (:file "src/vari.glsl/built-in-types")
               (:file "src/vari.glsl/special-operators/iteration")
               (:file "src/vari.glsl/special-operators/switch")
               (:file "src/vari.glsl/special-operators/discard")
               ;;
               (:file "src/vari.cl/docs")
               (:file "src/vari.cl/macros")
               (:file "src/vari.cl/functions")
               (:file "src/vari.cl/nary-operators")
               (:file "src/vari.cl/bitwise-operators")
               (:file "src/vari.cl/special-operators/uint")
               (:file "src/vari.cl/special-operators/progn")
               (:file "src/vari.cl/special-operators/let")
               (:file "src/vari.cl/special-operators/assignment")
               (:file "src/vari.cl/special-operators/conditionals")
               (:file "src/vari.cl/special-operators/case")
               (:file "src/vari.cl/special-operators/and-or")
               (:file "src/vari.cl/special-operators/declarations")
               (:file "src/vari.cl/special-operators/values")
               (:file "src/vari.cl/special-operators/return")
               (:file "src/vari.cl/special-operators/multiple-value-x")
               (:file "src/vari.cl/special-operators/local-functions")
               (:file "src/vari.cl/special-operators/function")
               (:file "src/vari.cl/special-operators/macros")
               (:file "src/vari.cl/special-operators/swizzle")
               (:file "src/vari.cl/special-operators/inline-code")
               (:file "src/vari.cl/special-operators/compiler-debugging")
               (:file "src/vari.cl/special-operators/make-array")
               (:file "src/vari.cl/special-operators/aref")
               (:file "src/vari.cl/special-operators/emit")
               (:file "src/vari.cl/special-operators/slots")
               (:file "src/vari.cl/special-operators/typecase")
               (:file "src/vari.cl/special-operators/coerce")
               (:file "src/vari.cl/complex")
               (:file "src/vari.cl/ratio")
               (:file "src/vari.cl/misc")
               ;;
               (:file "src/varjo.internals/translate")
               (:file "src/varjo.internals/glsl-stage")
               (:file "src/varjo.internals/rolling-translate")
               (:file "src/varjo.internals/test-compile")
               (:file "src/varjo.internals/front-end")

               (:file "src/varjo.api/environment")

               (:file "src/varjo.api/docs")
               (:file "src/docs")))
