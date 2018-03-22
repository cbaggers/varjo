(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Values
;;
;; Values is a special form in Varjo, not a function. I don't plan to change
;; this.

(v-defspecial values (&rest values)
  :args-valid t
  :return
  (let* ((base (v-multi-val-base env))
         (for-return (equal base *return-var-name-base*))
         (for-emit (equal base *emit-var-name-base*))
         ;;
         (new-env (fresh-environment env :multi-val-base nil))
         (qualifier-lists (mapcar #'extract-value-qualifiers values))
         (parsed-qualifier-lists (mapcar λ(mapcar #'parse-qualifier _)
                                         qualifier-lists))
         (forms (mapcar #'extract-value-form values))
         ;; {TODO} the environments are being thrown away here
         (objs (mapcar λ(compile-form _ new-env) forms)))
    (if values
        (cond
          (for-return (%values-for-return objs
                                          parsed-qualifier-lists
                                          env))
          (for-emit (%values-for-emit objs
                                      parsed-qualifier-lists
                                      env))
          (base (%values-for-multi-value-bind forms
                                              objs
                                              parsed-qualifier-lists
                                              env))
          (t (compile-form `(prog1 ,@values) env)))
        (%values-void env))))

(defun %values-void (env)
  (let ((void (make-type-set)))
    (values (make-compiled :type-set void
                           :current-line nil
                           :pure t)
            env)))

(defun %values-for-multi-value-bind (forms
                                     objs
                                     parsed-qualifier-lists
                                     env)
  (let* ((base (v-multi-val-base env))
         (glsl-names (loop :for i :from 1 :below (length forms) :collect
                        (postfix-glsl-index base i)))

         (vals (loop :for o :in objs
                  :for qlist :in parsed-qualifier-lists
                  :collect (qualify-type (primary-type o) qlist)))
         (first-name (gensym "val-for-mvb-"))
         (result (compile-form
                  `(let ((,first-name ,(first objs)))
                     ,@(loop :for o :in (rest objs)
                          :for n :in glsl-names :collect
                          `(glsl-expr ,(format nil "~a = ~~a" n)
                                      ,(primary-type o)
                                      ,o))
                     ,first-name)
                  env))
         (type-set (make-type-set* (cons (primary-type result) (rest vals)))))
    (values (copy-compiled
             result
             :type-set type-set)
            env)))


(defun %values-for-emit (objs parsed-qualifier-lists env)
  (let* (;;
         (new-env (fresh-environment env :multi-val-base nil))

         ;;
         (assign-forms (mapcar λ(gen-assignement-form-for-emit new-env _ _1)
                               (alexandria:iota (length objs))
                               objs))
         ;;
         (result (cond
                   ((v-voidp (first objs))
                    (compile-form `(progn ,@assign-forms) env))
                   (objs
                    (compile-form
                     `(progn
                        ,@assign-forms
                        (values))
                     env))
                   (t (error "Varjo: Invalid values form inside emit (values)"))))
         ;;
         (qualified-types (loop :for o :in objs
                             :for qlist :in parsed-qualifier-lists
                             :collect (qualify-type (primary-type o) qlist)))
         (type-set (make-type-set* qualified-types)))
    (values (copy-compiled
             result
             :type-set type-set
             :emit-set type-set
             :used-types (append
                          (used-types result)
                          (coerce type-set 'list)))
            env)))

(defun %values-for-return (objs parsed-qualifier-lists env)
  (assert objs)
  (let* (;;
         (needs-assign-p (not (or (v-voidp (first objs))
                                  (v-discarded-p (first objs)))))
         (new-env (fresh-environment env :multi-val-base nil))
         ;;
         (forms (if needs-assign-p
                    (mapcar λ(gen-assignement-form-for-return new-env _ _1)
                            (alexandria:iota (length objs))
                            objs)
                    objs))
         ;;
         (result (compile-form `(prog1 ,@forms) env))
         ;;
         (qualified-types (loop :for o :in objs
                             :for qlist :in parsed-qualifier-lists
                             :collect (qualify-type (primary-type o) qlist)))
         (type-set (make-type-set* qualified-types)))
    (values (copy-compiled
             result
             :type-set type-set
             :used-types (append
                          (used-types result)
                          (coerce type-set 'list)))
            env)))

(defun gen-assignement-form-for-return (env index code-obj)
  (if (= index 0)
      code-obj
      (let* ((is-main-p (not (null (member :main (v-context env)))))
             (stage (stage env)))
        (if is-main-p
            `(glsl-expr
              ,(format nil "~a = ~~a" (nth-return-name index stage t))
              ,(primary-type code-obj) ,code-obj)
            `(glsl-expr
              ,(format nil "~a = ~~a"
                       (postfix-glsl-index *return-var-name-base* index))
              ,(primary-type code-obj) ,code-obj)))))

(defun gen-assignement-form-for-emit (env index code-obj)
  (let* ((stage (stage env)))
    (assert (typep stage 'geometry-stage))
    `(glsl-expr
      ,(format nil "~a = ~~a" (nth-return-name index stage t))
      ,(primary-type code-obj) ,code-obj)))

(defun qualifier-form-p (form)
  (or (keywordp form)
      (and (listp form) (keywordp (first form)))))

(defun extract-value-qualifiers (value-form)
  (when (and (listp value-form)
             (qualifier-form-p (first value-form)))
    (butlast value-form)))

(defun extract-value-form (value-form)
  (if (and (listp value-form)
           (qualifier-form-p (first value-form)))
      (last1 value-form)
      value-form))

(v-defspecial values-safe (form)
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
          (let* ((final-env (fresh-environment e :multi-val-safe nil)))
            (values c
                    final-env))))
      (compile-form form env )))
