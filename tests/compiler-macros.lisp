(in-package :varjo.tests)
(5am:in-suite compiler-macro-tests)

;;------------------------------------------------------------
;; Helper data

(add-external-function 'test-cm '((x :int) (y :float)) nil
                       '((* x y)))

(v-define-compiler-macro test-cm (&whole w (x :int) (y :float))
  (if (and (numberp x) (numberp y))
      (* x y)
      w))

(v-define-compiler-macro test-cm (&whole w (x :int) (y :int))
  (if (and (numberp x) (numberp y))
      (* x y)
      w))

(v-define-compiler-macro test-cm (&whole w (x v-type) (y v-type))
  (if (and (numberp x) (numberp y))
      (* x y)
      w))

;;------------------------------------------------------------
;; Tests

(defun test-expand (func-sig env arg-code)
  (let* ((func (find-form-binding-by-literal func-sig env))
         (func (first (functions func)))
         (dummy-objs (loop :for nil :in arg-code :collect nil)))
    (find-and-expand-compiler-macro func dummy-objs arg-code env)))

(5am:def-test compiler-macros-0 (:suite compiler-macro-tests)
  (finishes-p
   (compile-vert () :410 nil
     (test-cm 10 2s0)
     (v! 0 0 0 0))))

(5am:def-test compiler-macros-1 (:suite compiler-macro-tests)
  (flow-id-scope
    (let* ((env (make-env :vertex)))
      (vbind (expansion use-expansion)
          (test-expand 'test-cm env '(1 2f0))
        (is (equal expansion
                   2f0))))))

(5am:def-test compiler-macros-2 (:suite compiler-macro-tests)
  (flow-id-scope
    (let* ((env (make-env :vertex)))
      (vbind (expansion use-expansion)
          (test-expand 'test-cm env '(x 2f0))
        (is (equal expansion
                   '(test-cm x 2f0)))))))
