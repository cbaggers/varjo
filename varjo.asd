;;;; varjo.asd

(asdf:defsystem #:varjo
  :serial t
  :components ((:file "package")
               (:file "base")
	       (:file "language")
               (:file "varjo")))

