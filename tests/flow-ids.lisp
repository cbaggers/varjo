(in-package :varjo.tests)
(5am:in-suite flow-id-tests)

(define-is-true-test flow-id-0 (:suite flow-id-tests)
  (flow-id-scope
    (let ((env (make-env :vertex)))
      (vbind (c0 e0) (compile-form `(+ 1 2 3) env)
        (vbind (c1 e1) (compile-form `(let ((x ,c0)) x) e0)
          (id= (flow-ids c0) (flow-ids c1)))))))

(define-is-true-test flow-id-1 (:suite flow-id-tests)
  (flow-id-scope
    (let ((env (make-env :vertex)))
      (vbind (c0 e0) (compile-form `(+ 1 2 3) env)
        (vbind (c1 e1) (compile-form `(+ ,c0 4) e0)
          (not (id= (flow-ids c0) (flow-ids c1))))))))

(define-is-true-test flow-id-2 (:suite flow-id-tests)
  (flow-id-scope
    (let ((env (make-env :vertex)))
      (vbind (c0 e0) (compile-form `(+ 1 2 3) env)
        (vbind (c1 e1) (compile-form `(labels ((foo ((x :int)) x))
                                        ,c0)
                                     e0)
          (id= (flow-ids c0) (flow-ids c1)))))))

(define-dbind-test flow-id-3 (:suite flow-id-tests) (c0 c1)
    (is-true (id= (flow-ids c0) (flow-ids c1)))
  (flow-id-scope
    (let ((env (make-env :vertex)))
      (vbind (c0 e0) (compile-form `(+ 1 2 3) env)
        (list c0 (compile-form `(labels ((foo ((x :int)) x))
                                  (foo ,c0))
                               e0))))))

(define-dbind-test flow-id-4 (:suite flow-id-tests) (c0 c1)
    (is-true (id= (flow-ids c0) (flow-ids c1)))
  (flow-id-scope
    (let ((env (make-env :vertex nil '((x :mat4)))))
      (list (v-type-of (find 'x (v-uniforms env) :key #'name))
            (compile-form 'x env)))))

(define-dbind-test flow-id-5 (:suite flow-id-tests) (c0 c1)
    (is-true (id= (flow-ids c0) (flow-ids c1)))
  (flow-id-scope
    (let ((env (make-env :vertex nil '((x :mat4)))))
      (list (v-type-of (find 'x (v-uniforms env) :key #'name))
            (compile-form '(let ((y x))
                            y)
                          env)))))

(define-dbind-test flow-id-6 (:suite flow-id-tests) (c0 c1)
    (is-true (id= (flow-ids c0) (flow-ids c1)))
  (flow-id-scope
    (let ((env (make-env :vertex nil '((x :mat4)))))
      (list (v-type-of (find 'x (v-uniforms env) :key #'name))
            (compile-form '(labels ((foo ((a :mat4)) a))
                            (let ((y (foo x)))
                              y))
                          env)))))
