(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Values

;; type & multi-vals are set by values.
;; 'return' turns those into a return-set.
;; 'emit' turns those into a emit-set.

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
         (forms (mapcar #'extract-value-form values))

         (objs (mapcar λ(compile-form _ new-env) forms))
         (base (v-multi-val-base env))
         (glsl-names (loop :for i :below (length forms) :collect
                        (postfix-glsl-index base i)))

         (vals (loop :for o :in objs
                  :for n :in glsl-names
                  :for q :in qualifier-lists
                  :collect (make-typed-glsl-name
                            (qualify-type (primary-type o) q)
                            n)))
         (first-name (gensym))
         (result (compile-form
                  `(let ((,first-name ,(first objs)))
                     ,@(loop :for o :in (rest objs)
                          :for v :in (rest vals) :collect
                          `(%assign ,v ,o))
                     ,first-name)
                  env))
         (type-set (apply #'make-type-set
                          (cons (primary-type result)
                                (rest vals))))
         (ast (ast-node! 'values
                         (mapcar λ(if _1 `(,@_1 ,(node-tree _)) (node-tree _))
                                 objs
                                 qualifier-lists)
                         type-set env env)))
    (values (copy-compiled
             result
             :type-set type-set
             :node-tree ast)
            env)))

(defun %values-void (env)
  (let ((void (make-type-set)))
    (values (make-compiled :type-set void
                           :current-line nil
                           :node-tree (ast-node! 'values nil void env env)
                           :pure t)
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
                                 (primary-type c)
                                 env
                                 final-env)))
            (values (copy-compiled c :node-tree ast)
                    final-env))))
      (compile-form form env )))
