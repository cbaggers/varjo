(in-package :vari.cl)
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
               (vbind (fn code) (build-function name args body t env)
                 (values code (add-form-binding fn env)))))
           p-env definitions)
          (compile-form `(progn ,@body) p-env)))
    (assert body-obj)
    ;; can be nil in case of cvt funcs--↓↓↓
    (let* ((merged (merge-progn (remove nil (cons-end body-obj func-def-objs))
                                env e)))
      (values merged e))))

(v-defspecial flet (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) e)
      (with-fresh-env-scope (fresh-env env)
        (env-> (p-env fresh-env)
          (let (form-bindings)
            (vbind (tobj tenv)
                (compile-forms-not-propagating-env-returning-list-of-compiled
                 (lambda (env d)
                   (dbind (name args &rest body) d
                     (vbind (fn code) (build-function name args body t env)
                       (push fn form-bindings)
                       (values code env))))
                 p-env definitions)
              (values tobj (add-form-bindings form-bindings tenv))))
          (compile-form `(progn ,@body) p-env)))
    (let* ((objs (remove nil (cons-end body-obj func-def-objs)))
           (merged (merge-progn objs env e)))
      (values merged
              e))))

;; TODO: should only take 1 definition
(v-defspecial labels-no-implicit (definitions derived-from exceptions
                                   &rest body)
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
                   (build-function name args body exceptions env
                                   :derived-from derived-from)
                 (values code (add-form-binding fn env)))))
           p-env
           definitions)
          (compile-form `(progn ,@body) p-env)))
    ;;
    (let* ((merged (merge-progn
                    (remove nil (cons-end body-obj func-def-objs))
                    env
                    pruned-starting-env)))
      (values merged
              pruned-starting-env))))
