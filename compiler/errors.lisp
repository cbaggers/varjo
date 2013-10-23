(in-package :varjo)

(define-condition missing-function-error (error)
  ((text :initarg :text :reader text)))
