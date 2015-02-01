;;;; varjo.asd

(asdf:defsystem #:varjo
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence #:alexandria #:fn_ #:named-readtables)
  :components ((:file "package")
               (:file "utils-v")
               (:file "compiler/errors")
               (:file "language/types")
               (:file "compiler/types")
               (:file "compiler/variables")
               (:file "compiler/environment")
               (:file "compiler/structs")
               (:file "compiler/code-object")
               (:file "compiler/functions")
               (:file "language/variables")
               (:file "compiler/macros")
               (:file "language/macros")
               (:file "compiler/string-generation")
               (:file "compiler/compiler")
               (:file "language/special")
               (:file "language/functions")
               (:file "language/textures")
               (:file "compiler/front-end")))


