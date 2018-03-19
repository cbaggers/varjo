(in-package :varjo.tests)

(defmacro define-vari-trait (name &body func-signatures))

(define-vari-trait iterable
  (length _)
  (make-sequence-like _)
  (make-sequence-like _ :int)
  (limit _)
  (limit _ :bool)
  (create-iterator-state _)
  (create-iterator-state _ :bool)
  (next-iterator-state iter-state)
  (next-iterator-state iter-state :bool)
  (iterator-limit-check iter-state iter-state)
  (iterator-limit-check iter-state iter-state :bool)
  (element-for-state _ iter-state)
  (index-for-state iter-state))
