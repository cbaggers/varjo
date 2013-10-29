;;;; varjo.asd

(asdf:defsystem #:varjo
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence)
  :components ((:file "package")
               (:file "utils-v")
               (:file "compiler/generics")
               (:file "compiler/homeless-things")
               (:file "compiler/backend-data")
               (:file "compiler/types")
               (:file "compiler/environment")
               (:file "compiler/code-object")
               (:file "compiler/functions")
               (:file "compiler/variables")
               (:file "compiler/macros")
               (:file "compiler/structs")
               ;; (:file "compiler/errors")
               ;; (:file "compiler/compiler")
               ;; (:file "compiler/string-generation")
               ;; (:file "language/types")
               ;; (:file "language/special")
               ;; (:file "language/functions")
               ;; (:file "language/macros")
               ;; (:file "language/textures")
               ;; (:file "front-end/arguments")
               ;; (:file "front-end/front-end")
               ))


