;;;; varjo.core.asd

(asdf:defsystem #:varjo
  :description "Common Lisp -> GLSL Compiler"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:vas-string-metrics
               #:named-readtables
               #:split-sequence
               #:glsl-symbols
               #:parse-float
               #:alexandria
               #:glsl-spec
               #:cl-ppcre
               #:uiop
               #:fn)
  :components ((:file "src/package")
               (:file "src/utils-v")
               (:file "src/defcondition")
               (:file "src/varjo.internals/generics")
               (:file "src/varjo.internals/globals")
               (:file "src/varjo.internals/names")
               (:file "src/varjo.internals/internal-types")
               (:file "src/varjo.internals/types/def-v-core-type")
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
               (:file "src/varjo.internals/ast-node")
               (:file "src/varjo.internals/stemcells")
               (:file "src/varjo.types/types")
               (:file "src/varjo.internals/environment")
               (:file "src/varjo.internals/external-functions")
               (:file "src/varjo.internals/structs")
               (:file "src/varjo.internals/string-generation")
               (:file "src/varjo.internals/casting")
               (:file "src/varjo.internals/stages")
               (:file "src/varjo.internals/compile-result")
               (:file "src/varjo.internals/compile-literal")
               (:file "src/varjo.internals/compile-vars")
               (:file "src/varjo.internals/compile-special")
               (:file "src/varjo.internals/make-function")
               (:file "src/varjo.internals/functions")
               (:file "src/varjo.internals/macros")
               (:file "src/varjo.internals/compile-funcall")
               (:file "src/varjo.internals/compile-form")
               ;;
               (:file "src/varjo.glsl/macros")
               (:file "src/varjo.glsl/functions")
               (:file "src/varjo.glsl/parse-from-spec")
               (:file "src/varjo.glsl/variables-from-spec")
               (:file "src/varjo.glsl/functions-from-spec")
               (:file "src/varjo.glsl/built-in-types")
               (:file "src/varjo.glsl/bitwise-operators")
               ;;
               (:file "src/varjo.cl/macros")
               (:file "src/varjo.cl/functions")
               (:file "src/varjo.cl/bitwise-operators")
               (:file "src/varjo.cl/special-operators/uint")
               (:file "src/varjo.cl/special-operators/progn")
               (:file "src/varjo.cl/special-operators/let")
               (:file "src/varjo.cl/special-operators/assignment")
               (:file "src/varjo.cl/special-operators/conditionals")
               (:file "src/varjo.cl/special-operators/and-or")
               (:file "src/varjo.cl/special-operators/declarations")
               (:file "src/varjo.cl/special-operators/values")
               (:file "src/varjo.cl/special-operators/return")
               (:file "src/varjo.cl/special-operators/multiple-value-x")
               (:file "src/varjo.cl/special-operators/local-functions")
               (:file "src/varjo.cl/special-operators/function")
               (:file "src/varjo.cl/special-operators/iteration")
               (:file "src/varjo.cl/special-operators/macros")
               (:file "src/varjo.cl/special-operators/swizzle")
               (:file "src/varjo.cl/special-operators/inline-code")
               (:file "src/varjo.cl/special-operators/compiler-debugging")
               (:file "src/varjo.cl/special-operators/make-array")
               (:file "src/varjo.cl/special-operators/aref")
               (:file "src/varjo.cl/special-operators/emit")
               (:file "src/varjo.cl/special-operators/slots")
               ;;
               (:file "src/varjo.internals/translate")
               (:file "src/varjo.internals/glsl-stage")
               (:file "src/varjo.internals/rolling-translate")
               (:file "src/varjo.internals/test-compile")
               (:file "src/varjo.internals/front-end")
               (:file "src/varjo.api/environment/public-api")))
