(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Bind

(v-defspecial multiple-value-bind (vars value-form &rest body)
  :args-valid t
  :return
  (let* ((base (lisp-name->glsl-name 'mvb env))
         (new-env (fresh-environment env :multi-val-base base)))
    (let ((value-obj (compile-form value-form new-env)))
      (unless (= (length vars) (length (type-set value-obj)))
        (error 'multi-val-bind-mismatch
               :val-form value-form
               :bindings vars
               :return-set (type-set value-obj)))
      (let ((types (type-set-to-type-list (type-set value-obj))))
        (vbind ((m-objs s-obj b-objs) final-env)
            (with-fresh-env-scope (fresh-env env)
              (env-> (p-env fresh-env)
                (compile-forms-not-propagating-env-returning-list-of-compiled
                 (lambda (env type name i)
                   (compile-let name (type->type-spec type) nil env
                                (postfix-glsl-index base i)))
                 p-env types vars (iota (length types)))
                (compile-form `(setq ,(first vars) ,value-obj) p-env)
                (compile-progn body p-env)))
          (let* ((m-obj (%merge-multi-env-progn m-objs))
                 (merged (merge-progn `(,m-obj ,s-obj ,@b-objs)
                                      env final-env)))
            (values
             (copy-compiled
              merged
              :node-tree (ast-node! 'multiple-value-bind
                                    `(,vars ,(node-tree value-obj)
                                            ,@(mapcar #'node-tree b-objs))
                                    (make-type-set (primary-type merged))
                                    env final-env))
             final-env)))))))

;;------------------------------------------------------------
;; Call

(v-defspecial multiple-value-call (function &rest value-forms)
  ;; {TODO} extend to multiple forms
  :args-valid t
  :return
  (vbind (func-obj fenv) (compile-form function env)
    ;; We want a seperate base for each form to avoid var naming clashes
    (let ((bases (loop :for nil :in value-forms :collect
                    (lisp-name->glsl-name 'mvb fenv))))
      ;; we compile the forms in a way that passes along the env (as in progn)
      ;; but returns all the compiled objs as a list
      (vbind (value-objs v-env)
          (compile-forms-propagating-env-returning-list-of-compiled
           (lambda (env form base)
             (compile-form form (fresh-environment
                                 env :multi-val-base base)))
           fenv
           value-forms
           bases)
        ;; We then want to make let forms for the multiple returns and then
        ;; assignments which store the primary returns of the value-objs
        (let* ((types-grouped (mapcar #'type-set-to-type-list
                                      (mapcar #'type-set value-objs)))
               (names-grouped (loop :for g :in types-grouped :collect
                                 (loop :for nil :in g :collect (gensym))))
               (assign-names (mapcar #'first names-grouped))
               (types (flatten types-grouped))
               (names (flatten names-grouped))
               (glsl-names (loop :for b :in bases
                              :for tg :in types-grouped :append
                              (loop :for i :below (length tg)
                                 :collect (postfix-glsl-index b i)))))
          ;; here we compile the lets and..
          (vbind ((m-objs s-obj b-obj) final-env)
              (with-fresh-env-scope (fresh-env fenv)
                (env-> (p-env fresh-env)
                  (compile-forms-not-propagating-env-returning-list-of-compiled
                   (lambda (env type name glsl-name)
                     (compile-let name (type->type-spec type) nil env
                                  glsl-name))
                   p-env types names glsl-names)
                  ;; ..the assignments..
                  (compile-form `(progn ,@(loop :for n :in assign-names
                                             :for v :in value-objs
                                             :collect `(setq ,n ,v)))
                                p-env)
                  ;; ..and finally the call the the function passed into
                  ;; multiple-value-call in the first place
                  (compile-form `(funcall ,function ,@names) p-env)))
            ;; The rest is fairly standard
            (let* ((m-obj (%merge-multi-env-progn m-objs))
                   (merged (merge-progn `(,m-obj ,s-obj ,b-obj)
                                        env final-env)))
              (values
               (copy-compiled
                merged
                :node-tree (ast-node! 'multiple-value-bind
                                      (cons (node-tree func-obj)
                                            (mapcar #'node-tree value-objs))
                                      (make-type-set (primary-type merged))
                                      env final-env))
               final-env))))))))

;;------------------------------------------------------------
;; Prog1

(v-defspecial multiple-value-prog1 (values-form &rest body)
  ;; {TODO} extend to multiple forms
  :args-valid t
  :return
  (let* ((base (lisp-name->glsl-name 'mvb env))
         (new-env (fresh-environment env :multi-val-base base)))
    (vbind (value-obj v-env) (compile-form values-form new-env)
      ;; We then want to make let forms for the multiple returns and then
      ;; assignments which store the primary returns of the value-objs
      (let* ((types (type-set-to-type-list (type-set value-obj)))
             (names (loop :for nil :in types :collect (gensym)))
             (glsl-names (loop :for i :below (length types)
                            :collect (postfix-glsl-index base i))))
        ;; here we compile the lets and..
        (vbind ((m-objs s-obj b-objs r-obj) final-env)
            (with-fresh-env-scope (fresh-env env)
              (env-> (p-env fresh-env)
                (compile-forms-not-propagating-env-returning-list-of-compiled
                 (lambda (env type name glsl-name)
                   (compile-let name (type->type-spec type) nil env
                                glsl-name))
                 p-env types names glsl-names)
                ;; ..the assignments..
                (compile-form `(setq ,(first names) ,value-obj) p-env)
                ;; .. the body..
                (compile-progn body p-env)
                ;; and the multi-return
                (compile-form `(values ,@names) p-env)))
          ;; The rest is fairly standard
          (let* ((m-obj (%merge-multi-env-progn m-objs))
                 (merged (merge-progn `(,m-obj ,s-obj ,@b-objs ,r-obj)
                                      env final-env)))
            (values
             (copy-compiled
              merged
              :node-tree (ast-node! 'multiple-value-bind
                                    (cons (node-tree value-obj)
                                          (mapcar #'node-tree b-objs))
                                    (make-type-set (primary-type merged))
                                    env final-env))
             final-env)))))))
