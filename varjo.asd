;;;; varjo.asd

(asdf:defsystem #:varjo
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence)
  :components ((:file "package")
               (:file "base")
	       (:file "language")
               (:file "varjo")))

