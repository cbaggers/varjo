(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Values

(v-defspecial values (&rest values)
  :args-valid t
  :return
  (if values
      (if (v-multi-val-base env)
          (%values values env)
          (compile-form `(prog1 ,@values) env))
      (%values-void env)))

(defun %values (values env)
  (let* ((new-env (fresh-environment env :multi-val-base nil))
         (qualifier-lists (mapcar #'extract-value-qualifiers values))
         (out-qualifiers (mapcar λ(remove-if-not λ(out-qualifier-p _ env) _)
                                 qualifier-lists))
         (qualifier-lists (mapcar λ(set-difference _ _1 :test #'equal)
                                  qualifier-lists
                                  out-qualifiers))
         (forms (mapcar #'extract-value-form values))

         (objs (mapcar λ(compile-form _ new-env) forms))
         (base (v-multi-val-base env))
         (glsl-names (loop :for i :below (length forms) :collect
                        (format nil "~a~a" base i)))
         (vals (loop :for o :in objs :for n :in glsl-names :collect
                  (v-make-value (code-type o) env :glsl-name n)))
         (first-name (gensym))
         (result (compile-form
                  `(let ((,first-name ,(first objs)))
                     ,@(loop :for o :in (rest objs)
                          :for v :in (rest vals) :collect
                          `(%assign ,v ,o))
                     ,first-name)
                  env))
         (ast (ast-node! 'values
                         (mapcar λ(if _1 `(,@_1 ,(node-tree _)) (node-tree _))
                                 objs
                                 qualifier-lists)
                         (code-type result) env env)))
    (values (copy-code result
                       :multi-vals (mapcar #'make-mval (rest vals)
                                           (rest qualifier-lists))
                       :return-set (make-return-set
                                    (mapcar #'cons
                                            (mapcar #'v-type-of vals)
                                            out-qualifiers))
                       :node-tree ast)
            env)))

(defun %values-void (env)
  (let ((void (type-spec->type :void (flow-id!))))
    (values (code! :type void
                   :current-line nil
                   :node-tree (ast-node! 'values nil void env env))
            env)))

(defun extract-value-qualifiers (value-form)
  (when (and (listp value-form) (keywordp (first value-form)))
    (butlast value-form)))

(defun extract-value-form (value-form)
  (if (and (listp value-form) (keywordp (first value-form)))
      (last1 value-form)
      value-form))


(v-defspecial varjo-lang:values-safe (form)
  ;; this special-form executes the form without destroying
  ;; the multi-return 'values' travalling up the stack.
  ;; Progn is implictly values-safe, but * isnt by default.
  ;;
  ;; it will take the values from whichever argument has them
  ;; if two of the arguments have them then values-safe throws
  ;; an error
  :args-valid t
  :return
  (if (listp form)
      (let ((safe-env (fresh-environment
                       env :multi-val-base (v-multi-val-base env)
                       :multi-val-safe t)))
        (vbind (c e) (compile-list-form form safe-env)
          (let* ((final-env (fresh-environment e :multi-val-safe nil))
                 (ast (ast-node! 'varjo-lang:values-safe
                                 (list (node-tree c))
                                 (code-type c)
                                 env
                                 final-env)))
            (values (copy-code c :node-tree ast)
                    final-env))))
      (compile-form form env )))
