(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Labels, flet & labels-no-implicit

(v-defspecial labels (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) e)
      (with-fresh-env-scope (fresh-env env)
        (env-> (p-env fresh-env)
          (compile-forms-propagating-env-returning-list-of-compiled
           (lambda (env d)
             (dbind (name args &rest body) d
               (vbind (fn code) (build-function nil name args body t env)
                 (values code (add-form-binding fn env)))))
           p-env definitions)
          (compile-form `(progn ,@body) p-env)))
    (assert body-obj)
    ;; can be nil in case of cvt funcs--↓↓↓
    (let* ((merged (merge-progn (remove nil (cons-end body-obj func-def-objs))
                                env e))
           (ast (ast-node!
                 'labels
                 (list (remove nil (mapcar λ(when _1
                                              (cons-end
                                               (node-tree _1)
                                               (subseq _ 0 2)))
                                           definitions
                                           func-def-objs))
                       (node-tree body-obj))
                 (type-set body-obj)
                 env env)))
      (values (copy-compiled merged :node-tree ast)
              e))))

(v-defspecial flet (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) e)
      (with-fresh-env-scope (fresh-env env)
        (env-> (p-env fresh-env)
          (compile-forms-not-propagating-env-returning-list-of-compiled
           (lambda (env d)
             (dbind (name args &rest body) d
               (vbind (fn code) (build-function nil name args body t env)
                 (values code (add-form-binding fn env)))))
           p-env definitions)
          (compile-form `(progn ,@body) p-env)))
    (let* ((merged (merge-progn (remove nil (cons-end body-obj func-def-objs))
                                env e))
           (ast (ast-node!
                 'flet
                 (list (remove nil (mapcar λ(when _1
                                              (cons-end
                                               (node-tree _1)
                                               (subseq _ 0 2)))
                                           definitions
                                           func-def-objs))
                       (node-tree body-obj))
                 (type-set body-obj) env env)))
      (values (copy-compiled merged :node-tree ast)
              e))))

(v-defspecial labels-no-implicit (definitions exceptions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) pruned-starting-env) ;;ending-env
      (with-fresh-env-scope (fresh-env env)
        ;;
        (env-> (p-env fresh-env)
          (compile-forms-propagating-env-returning-list-of-compiled
           (lambda (env d)
             (dbind (name args &rest body) d
               (vbind (fn code)
                   (build-function nil name args body exceptions env)
                 (values code (add-form-binding fn env)))))
           p-env
           definitions)
          (compile-form `(progn ,@body) p-env)))
    ;;
    (let* ((merged (merge-progn
                    (remove nil (cons-end body-obj func-def-objs))
                    env
                    pruned-starting-env))
           (ast (ast-node!
                 'labels-no-implicit
                 (list (remove nil (mapcar λ(if _1
                                                (cons-end
                                                 (node-tree _1)
                                                 (subseq _ 0 2))
                                                _)
                                           definitions
                                           func-def-objs))
                       exceptions
                       (node-tree body-obj))
                 (type-set body-obj) env env)))
      (values (copy-compiled merged :node-tree ast)
              pruned-starting-env))))
