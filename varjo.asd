;;;; varjo.asd

(asdf:defsystem #:varjo
  :description "Common Lisp -> GLSL Compiler"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:varjo.core #:rtg-math)
  :components ((:file "varjo/package")
               (:file "varjo/varjo.rtg-math/functions")
               (:file "varjo/varjo.rtg-math/rtg-math-equivalents")))
