;;;; varjo.asd

(asdf:defsystem #:varjo.import
  :description "Common Lisp -> GLSL Compiler"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:varjo #:glsl-toolkit #:optima #:fare-quasiquote-extras)
  :components ((:file "import/package")
			   (:file "import/readtables")
               (:file "import/import")))
