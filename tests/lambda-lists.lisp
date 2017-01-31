(in-package :varjo.tests)
(5am:in-suite lambda-list-tests)

;;------------------------------------------------------------
;; Helper data

(add-external-function 'test-ll-0 '((x :int)) nil
                       '((* x x)))

;;------------------------------------------------------------
;; Tests

(5am:def-test lambda-lists-0 (:suite lambda-list-tests)
  (finishes-p
   (compile-vert () :450 nil
     (test-ll 10)
     (v! 0 0 0 0))))
