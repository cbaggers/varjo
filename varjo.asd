;;;; varjo.asd

(asdf:defsystem #:varjo
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence)
  :components ((:file "package")
               (:file "utils-v")
               (:file "compiler/homeless-things")
               (:file "compiler/backend-data")
               (:file "compiler/code-object")
               (:file "compiler/types")
               (:file "compiler/functions")
               (:file "compiler/variables")
               (:file "compiler/structs")
               (:file "compiler/errors")
               (:file "compiler/compiler")
               (:file "compiler/string-generation")
               ;;-v-v-v----old----v-v-v-;
               (:file "language")
               (:file "varjo")))


