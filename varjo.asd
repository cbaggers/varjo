;;;; varjo.asd

(asdf:defsystem #:varjo
  :description "Common Lisp -> GLSL Compiler"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "LLGPL"
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence #:alexandria #:named-readtables
                          #:vas-string-metrics #:stefil #:fn #:structy-defclass
			  #:cl-game-math)
  :components ((:file "package")
               (:file "utils-v")
	       (:file "compiler/log")
               (:file "compiler/errors")
	       (:file "compiler/names")
               (:file "compiler/types")
               (:file "compiler/code-object")
               (:file "compiler/stemcells")
               (:file "language/types")
               (:file "compiler/flow")
               (:file "compiler/variables")
               (:file "compiler/environment")
               (:file "compiler/structs")
               (:file "compiler/functions")
               (:file "language/variables")
               (:file "compiler/macros")
               (:file "language/macros")
               (:file "compiler/string-generation")
	       (:file "compiler/ast-node")
	       (:file "compiler/compile-literal")
	       (:file "compiler/compile-vars")
	       (:file "compiler/compile-special")
	       (:file "compiler/compile-funcall")
	       (:file "compiler/compile-form")
	       (:file "compiler/compile-result")
               (:file "language/special")
               (:file "language/functions")
               (:file "language/textures")
               (:file "compiler/front-end")
               (:file "language/object-printers")
	       (:file "language/cl-game-math-equivalents")
	       (:file "tests/package")
	       (:file "tests/tests")))
