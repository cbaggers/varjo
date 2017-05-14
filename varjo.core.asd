;;;; varjo.core.asd

(asdf:defsystem #:varjo.core
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
  :components ((:file "varjo.core/package")
               (:file "varjo.core/utils-v")
               (:file "varjo.core/defcondition")
               (:file "varjo.core/varjo.internals/generics")
               (:file "varjo.core/varjo.internals/globals")
               (:file "varjo.core/varjo.internals/names")
               (:file "varjo.core/varjo.internals/internal-types")
               (:file "varjo.core/varjo.internals/types/def-v-core-type")
               (:file "varjo.core/varjo.internals/types/early-types")
               (:file "varjo.core/varjo.internals/flow")
               (:file "varjo.core/varjo.internals/types/types")
               (:file "varjo.core/varjo.internals/return-set")
               (:file "varjo.core/varjo.internals/emit-set")
               (:file "varjo.core/varjo.internals/value-metadata")
               (:file "varjo.core/varjo.internals/errors")
               (:file "varjo.core/varjo.internals/function-obj")
               (:file "varjo.core/varjo.internals/deftype")
               (:file "varjo.core/varjo.internals/variables")
               (:file "varjo.core/varjo.internals/code-object")
               (:file "varjo.core/varjo.internals/ast-node")
               (:file "varjo.core/varjo.internals/stemcells")
               (:file "varjo.core/varjo.types/types")
               (:file "varjo.core/varjo.internals/environment")
               (:file "varjo.core/varjo.internals/external-functions")
               (:file "varjo.core/varjo.internals/structs")
               (:file "varjo.core/varjo.internals/string-generation")
               (:file "varjo.core/varjo.internals/casting")
               (:file "varjo.core/varjo.internals/stages")
               (:file "varjo.core/varjo.internals/compile-result")
               (:file "varjo.core/varjo.internals/compile-literal")
               (:file "varjo.core/varjo.internals/compile-vars")
               (:file "varjo.core/varjo.internals/compile-special")
               (:file "varjo.core/varjo.internals/make-function")
               (:file "varjo.core/varjo.internals/functions")
               (:file "varjo.core/varjo.internals/macros")
               (:file "varjo.core/varjo.internals/compile-funcall")
               (:file "varjo.core/varjo.internals/compile-form")
               ;;
               (:file "varjo.core/varjo.glsl/macros")
               (:file "varjo.core/varjo.glsl/functions")
               (:file "varjo.core/varjo.glsl/parse-from-spec")
               (:file "varjo.core/varjo.glsl/variables-from-spec")
               (:file "varjo.core/varjo.glsl/functions-from-spec")
               (:file "varjo.core/varjo.glsl/built-in-types")
               (:file "varjo.core/varjo.glsl/bitwise-operators")
               ;;
               (:file "varjo.core/varjo.cl/macros")
               (:file "varjo.core/varjo.cl/functions")
               (:file "varjo.core/varjo.cl/bitwise-operators")
               (:file "varjo.core/varjo.cl/special-operators/uint")
               (:file "varjo.core/varjo.cl/special-operators/progn")
               (:file "varjo.core/varjo.cl/special-operators/let")
               (:file "varjo.core/varjo.cl/special-operators/assignment")
               (:file "varjo.core/varjo.cl/special-operators/conditionals")
               (:file "varjo.core/varjo.cl/special-operators/and-or")
               (:file "varjo.core/varjo.cl/special-operators/declarations")
               (:file "varjo.core/varjo.cl/special-operators/values")
               (:file "varjo.core/varjo.cl/special-operators/return")
               (:file "varjo.core/varjo.cl/special-operators/multiple-value-x")
               (:file "varjo.core/varjo.cl/special-operators/local-functions")
               (:file "varjo.core/varjo.cl/special-operators/function")
               (:file "varjo.core/varjo.cl/special-operators/iteration")
               (:file "varjo.core/varjo.cl/special-operators/macros")
               (:file "varjo.core/varjo.cl/special-operators/swizzle")
               (:file "varjo.core/varjo.cl/special-operators/inline-code")
               (:file "varjo.core/varjo.cl/special-operators/compiler-debugging")
               (:file "varjo.core/varjo.cl/special-operators/make-array")
               (:file "varjo.core/varjo.cl/special-operators/aref")
               (:file "varjo.core/varjo.cl/special-operators/emit")
               (:file "varjo.core/varjo.cl/special-operators/slots")
               ;;
               (:file "varjo.core/varjo.internals/translate")
               (:file "varjo.core/varjo.internals/glsl-stage")
               (:file "varjo.core/varjo.internals/rolling-translate")
               (:file "varjo.core/varjo.internals/test-compile")
               (:file "varjo.core/varjo.internals/front-end")
               (:file "varjo.core/varjo.api/environment/public-api")))
