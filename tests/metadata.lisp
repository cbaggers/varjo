(in-package :varjo.tests)
(5am:in-suite metadata-tests)

;;------------------------------------------------------------
;; Helper data

;;------------------------------------------------------------
;; Tests


(5am:def-test metadatas-0 (:suite metadata-tests)
  (finishes-p
   (compile-vert () :450 t
     ;; this is to text #'inject-implicit-uniform which is used
     ;; by the metadata api
     (varjo::lisp-code-as-uniform foo :int 10)
     (v! 0 0 0 1))))
