;;;; varjo.asd

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(asdf:defsystem #:varjo
  :description "Common Lisp -> GLSL Compiler"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence #:alexandria #:named-readtables
                          #:vas-string-metrics #:fn #:uiop
                          #:rtg-math #:glsl-spec)
  :components ((:file "package")
               (:file "utils-v")
               (:file "compiler/generics")
               (:file "compiler/globals")
               (:file "compiler/log")
               (:file "compiler/errors")
               (:file "compiler/names")
               (:file "compiler/internal-types")
               (:file "compiler/types")
               (:file "compiler/function-obj")
               (:file "compiler/code-object")
               (:file "compiler/stemcells")
               (:file "language/types")
               (:file "compiler/flow")
               (:file "compiler/variables")
               (:file "compiler/external-functions")
               (:file "compiler/environment")
               (:file "compiler/value-metadata")
               (:file "compiler/structs")
               (:file "compiler/macros")
               (:file "language/macros")
               (:file "compiler/functions")
               (:file "language/struct-types")
               (:file "compiler/string-generation")
               (:file "compiler/compile-result")
               (:file "compiler/ast-node")
               (:file "compiler/compile-literal")
               (:file "compiler/compile-vars")
               (:file "compiler/compile-special")
               (:file "compiler/compile-funcall")
               (:file "compiler/compile-form")
               (:file "compiler/make-function")
               (:file "language/parse-from-spec")
               (:file "language/variables-from-spec")
               (:file "language/functions-from-spec")
               (:file "language/special")
               (:file "language/functions")
               (:file "language/bitwise-operators")
               (:file "compiler/translate")
               (:file "compiler/front-end")
               (:file "compiler/environment/public-api")
               (:file "language/rtg-math-equivalents")))
