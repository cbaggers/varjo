(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Values
;;
;; Values is a special form in Varjo, not a function. I don't plan to change
;; this.

;; type & multi-vals are set by values.
;; 'return' turns those into a return-set.
;; 'emit' turns those into a emit-set.

(v-defspecial values (&rest values)
  :args-valid t
  :return
  (let* ((base (v-multi-val-base env))
         (for-return (equal base *return-var-name-base*))
         (for-emit (equal base *emit-var-name-base*))
         ;;
         (new-env (fresh-environment env :multi-val-base nil))
         (qualifier-lists (mapcar #'extract-value-qualifiers values))
         (forms (mapcar #'extract-value-form values))
         (objs (mapcar λ(compile-form _ new-env) forms)))
    (if values
        (cond
          (for-return (%values-for-return objs qualifier-lists env))
          (for-emit (%values-for-emit objs qualifier-lists env))
          (base (%values forms objs qualifier-lists env))
          (t (compile-form `(prog1 ,@values) env)))
        (%values-void for-return env))))

(defun %values (forms objs qualifier-lists env)
  (let* ((base (v-multi-val-base env))
         (glsl-names (loop :for i :from 1 :below (length forms) :collect
                        (postfix-glsl-index base i)))

         (vals (loop :for o :in objs
                  :for q :in qualifier-lists
                  :collect (qualify-type (primary-type o) q)))
         (first-name (gensym))
         (result (compile-form
                  `(let ((,first-name ,(first objs)))
                     ,@(loop :for o :in (rest objs)
                          :for n :in glsl-names :collect
                          `(glsl-expr ,(format nil "~a = ~~a" n)
                                      ,(primary-type o)
                                      ,o))
                     ,first-name)
                  env))
         (type-set (make-type-set* (cons (primary-type result) (rest vals))))
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


(defun %values-for-emit (objs qualifier-lists env)
  (let* (;;
         (new-env (fresh-environment env :multi-val-base nil))

         ;;
         (assign-forms (mapcar λ(gen-assignement-form-for-return new-env _ _1)
                               (iota (length objs))
                               objs))
         ;;
         (first-name (gensym))
         (result (cond
                   ((v-voidp (first objs))
                    (compile-form `(progn ,@assign-forms) env))
                   ((> (length objs) 1)
                    (compile-form
                     `(let ((,first-name ,(first assign-forms)))
                        ,@(rest assign-forms)
                        ,first-name)
                     env))
                   (t (first objs))))
         ;;
         (qualified-types (loop :for o :in objs
                             :for q :in qualifier-lists
                             :collect (qualify-type (primary-type o) q)))
         (type-set (make-type-set* qualified-types))
         ;;
         (ast (ast-node! 'values
                         (mapcar λ(if _1 `(,@_1 ,(node-tree _)) (node-tree _))
                                 objs
                                 qualifier-lists)
                         type-set env env)))
    (values (copy-compiled
             result
             :type-set type-set
             :emit-set type-set
             :node-tree ast)
            env)))

(defun %values-for-return (objs qualifier-lists env)
  (let* (;;
         (is-main-p (not (null (member :main (v-context env)))))
         (new-env (fresh-environment env :multi-val-base nil))
         ;;
         (assign-forms (mapcar λ(gen-assignement-form-for-return new-env _ _1)
                               (iota (length objs))
                               objs))
         ;;
         (first-name (gensym))
         (result (cond
                   ((v-voidp (first objs))
                    (compile-form
                     `(progn
                        ,@assign-forms
                        (%glsl-expr "return" :void))
                     env))
                   ((> (length objs) 1)
                    (compile-form
                     (if is-main-p
                         `(progn
                            ,@assign-forms
                            (%glsl-expr "return" :void))
                         `(let ((,first-name ,(first assign-forms)))
                            ,@(rest assign-forms)
                            (%glsl-expr "return ~a"
                                        ,(primary-type (first objs))
                                        ,first-name)))
                     env))
                   (t (compile-form
                       (if is-main-p
                           `(progn
                              ,(first assign-forms)
                              (%glsl-expr "return" :void))
                           `(%glsl-expr "return ~a"
                                        ,(primary-type (first objs))
                                        ,(first assign-forms)))
                       env))))
         ;;
         (qualified-types (loop :for o :in objs
                             :for q :in qualifier-lists
                             :collect (qualify-type (primary-type o) q)))
         (type-set (make-type-set* qualified-types))
         ;;
         (ast (ast-node! 'values
                         (mapcar λ(if _1 `(,@_1 ,(node-tree _)) (node-tree _))
                                 objs
                                 qualifier-lists)
                         type-set env env)))
    (values (copy-compiled
             result
             :type-set type-set
             :return-set type-set
             :node-tree ast)
            env)))

(defun gen-assignement-form-for-return (env index code-obj)
  (let* ((is-main-p (not (null (member :main (v-context env)))))
         (stage (stage env)))
    (if is-main-p
        (if (and (stage-where-first-return-is-position-p stage)
                 (= index 0))
            (if (v-type-eq (primary-type code-obj) (type-spec->type :vec4))
                `(setq varjo-lang::gl-position ,code-obj)
                (error 'vertex-stage-primary-type-mismatch
                       :prim-type (primary-type code-obj)))
            `(glsl-expr
              ,(format nil "~a = ~~a" (nth-return-name index stage t))
              ,(primary-type code-obj) ,code-obj))
        (if (> index 0)
            `(glsl-expr
              ,(format nil "~a = ~~a"
                       (postfix-glsl-index *return-var-name-base* index))
              ,(primary-type code-obj) ,code-obj)
            code-obj))))

(defun %values-void (for-return env)
  (let ((void (make-type-set)))
    (values (make-compiled :type-set void
                           :return-set (when for-return void)
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
