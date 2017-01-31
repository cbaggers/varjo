(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Multiple Values

(v-defspecial multiple-value-bind (vars value-form &rest body)
  :args-valid t
  :return
  (let* ((base (lisp-name->glsl-name 'mvb env))
         (new-env (fresh-environment env :multi-val-base base)))
    (let ((value-obj (compile-form value-form new-env)))
      (unless (= (length vars) (+ 1 (length (multi-vals value-obj))))
        (error 'multi-val-bind-mismatch :val-form value-form :bindings vars))
      (let* ((mvals (multi-vals value-obj))
             (v-vals (mapcar #'multi-val-value mvals))
             (types (cons (code-type value-obj) (mapcar #'v-type-of v-vals))))
        (vbind ((m-objs s-obj b-objs) final-env)
            (with-fresh-env-scope (fresh-env env)
              (env-> (p-env fresh-env)
                (%mapcar-multi-env-progn
                 (lambda (env type name i)
                   (compile-let name (type->type-spec type) nil env
                                (format nil "~a~a" base i)))
                 p-env types vars (iota (length types)))
                (compile-form `(setq ,(first vars) ,value-obj) p-env)
                (compile-progn body p-env)))
          (let* ((m-obj (%merge-multi-env-progn m-objs))
                 (merged (merge-progn `(,m-obj ,s-obj ,@b-objs)
                                      env final-env)))
            (values
             (copy-code
              merged
              :node-tree (ast-node! 'multiple-value-bind
                                    `(,vars ,(node-tree value-obj)
                                            ,@(mapcar #'node-tree b-objs))
                                    (code-type merged)
                                    env final-env))
             final-env)))))))
