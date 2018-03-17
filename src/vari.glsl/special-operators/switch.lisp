(in-package :vari.glsl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Switch

;; {TODO} check keys
(v-defspecial switch (test-form &rest clauses)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (let* ((keys (mapcar #'first clauses))
           (clause-pairs (mapcar λ(multiple-value-list
                                   (compile-form `(progn ,(second _)) env))
                                 clauses))
           (clause-objs (mapcar #'first clause-pairs))
           (final-env
            (let ((envs (apply #'env-prune* (env-depth test-env)
                               (mapcar #'second clause-pairs))))
              (reduce #'env-merge-history
                      (rest envs) :initial-value (first envs)))))
      (if (and (or (v-typep (primary-type test-obj) 'v-uint)
                   (v-typep (primary-type test-obj) 'v-int))
               (loop :for key :in keys :always
                  (or (eq key 'default) (integerp key))))
          (let* ((type-set (make-type-set)))
            (values (merge-compiled
                     clause-objs
                     :type-set type-set
                     :current-line nil
                     :to-block (gen-switch-chunk test-obj keys
                                                 clause-objs)
                     :node-tree (ast-node!
                                 'switch
                                 (cons (node-tree test-obj)
                                       (mapcar λ`(,(first _)
                                                   ,(node-tree _1))
                                               clauses
                                               clause-objs))
                                 type-set env final-env))
                    final-env))
          (error 'switch-type-error :test-obj test-obj :keys keys)))))

;;------------------------------------------------------------
