(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Let

(v-defspecial let (bindings &rest body)
  :args-valid t
  :return
  (let* ((binding-names (mapcar λ(nth-or-self 0 _) bindings))
         (dup-names (find-duplicates binding-names))
         (base (v-multi-val-base env)))
    (assert (not dup-names) () 'dup-names-in-let :names dup-names)
    (unless body (error 'body-block-empty :form-name 'let))
    (vbind (body declarations) (extract-declares-and-doc-string body)
      (vbind ((new-var-objs nil body-obj) final-env)
          (with-fresh-env-scope (fresh-env env :multi-val-base nil)
            (env-> (p-env fresh-env)
              (compile-forms-not-propagating-env-returning-list-of-compiled
               (lambda (p-env binding)
                 (with-v-let-spec binding
                   (compile-let name type-spec value-form p-env nil nil)))
               p-env bindings)
              (compile-declares declarations p-env)
              (with-fresh-env-scope (based-env p-env :multi-val-base base)
                (compile-form `(progn ,@body) based-env))))
        (let* ((merged (merge-progn (list (merge-multi-env-progn new-var-objs)
                                          body-obj)
                                    env final-env)))
          (values
           merged
           final-env))))))

(v-defmacro let* (bindings &rest body)
  (unless body (error 'body-block-empty :form-name 'let))
  (let* ((bindings (reverse bindings))
         (result `(let (,(first bindings)) ,@body)))
    (loop :for binding :in (rest bindings) :do
       (setf result `(let (,binding) ,result)))
    result))

;;------------------------------------------------------------
