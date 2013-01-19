;;;; varjo.asd

(asdf:defsystem #:varjo
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "base")
	       (:file "language")
               (:file "varjo")))

