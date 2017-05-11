(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Let

(v-defspecial let (bindings &rest body)
  :args-valid t
  :return
  (let* ((binding-names (mapcar λ(first (listify (first _)))
                                bindings))
         (dup-names (find-duplicates binding-names)))
    (assert (not dup-names) () 'dup-names-in-let :names dup-names)
    (unless body (error 'body-block-empty :form-name 'let))
    (vbind (body declarations) (extract-declares body)
      (vbind ((new-var-objs nil body-obj) final-env)
          (with-fresh-env-scope (fresh-env env)
            (env-> (p-env fresh-env)
              (compile-forms-not-propagating-env-returning-list-of-compiled
               (lambda (p-env binding)
                 (with-v-let-spec binding
                   (compile-let name type-spec value-form p-env nil nil)))
               p-env bindings)
              (compile-declares declarations p-env)
              (compile-form `(progn ,@body) p-env)))
        (let* ((merged (merge-progn (list (merge-multi-env-progn new-var-objs)
                                          body-obj)
                                    env final-env))
               (val-ast-nodes (mapcar λ(unless (eq (node-tree _) :ignored)
                                         (list (node-tree _)))
                                      new-var-objs))
               (ast-args
                (list (mapcar λ(with-v-let-spec _
                                 (if type-spec
                                     `((,name ,type-spec) ,@_1)
                                     `(,name ,@_1)))
                              bindings
                              val-ast-nodes)
                      (node-tree body-obj)))
               (ast (ast-node! 'let ast-args
                               (type-set merged)
                               env final-env)))
          (values
           (copy-compiled merged :node-tree ast)
           final-env))))))

(v-defmacro let* (bindings &rest body)
  (unless body (error 'body-block-empty :form-name 'let))
  (let* ((bindings (reverse bindings))
         (result `(let (,(first bindings)) ,@body)))
    (loop :for binding :in (rest bindings) :do
       (setf result `(let (,binding) ,result)))
    result))

;;------------------------------------------------------------
