(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Return


(v-defspecial return (&optional (form '(values)))
  :args-valid t
  :return
  (%return form nil env))

(v-defspecial implicit-return (form)
  :args-valid t
  :return
  (%return form t env))

(defun %return (form implicit env)
  (let ((new-env (fresh-environment env :multi-val-base "return")))
    ;; we create an environment with the signal to let any 'values' forms
    ;; down the tree know they will be caught and what their name prefix should
    ;; be.
    ;; We then compile the form using the augmented environment, the values
    ;; statements will expand and flow back as 'multi-vals' and the
    ;; current-line
    ;;
    ;; now there are two styles of return:
    ;; - The first is for a regular function, in which multivals become
    ;;   out-arguments and the current-line is returned
    ;; - The second is for a shader stage in which the multi-vars become
    ;;   output-variables and the current line is handled in a 'context'
    ;;   specific way.
    ;;
    ;; If you make changes here, look at #'emit to see if it needs
    ;; similar changes
    (let* ((code-obj (compile-form form new-env))
           (is-main (not (null (member :main (v-context new-env)))))
           (result
            (if is-main
                (%main-return code-obj implicit new-env)
                (%regular-return code-obj implicit)))
           (ast (ast-node! 'return (node-tree code-obj)
                           (make-type-set)
                           env env))
           (ret-set (or (return-set result)
                        (error 'nil-return-set
                               :form (list (if implicit
                                               'implicit-return
                                               'return)
                                           form)
                               :possible-set (return-set code-obj)))))
      (values (copy-compiled result
                             :type-set (make-type-set)
                             :node-tree ast
                             :return-set ret-set)
              env))))

;; Used when this is a labels (or otherwise local) function
(defun %regular-return (code-obj implicit)
  (let* ((suppress-return (or (v-voidp (primary-type code-obj))
                              (v-typep (primary-type code-obj)
                                       :ephemeral-type)))
         (ret-set (if (and implicit (v-voidp (primary-type code-obj)))
                      (or (return-set code-obj)
                          (make-type-set))
                      (type-set code-obj))))
    ;;
    (copy-compiled
     code-obj
     :type-set (make-type-set)
     :current-line (if suppress-return
                       (current-line code-obj)
                       (format nil "return ~a" (current-line code-obj)))
     :place-tree nil
     :return-set ret-set)))


;; Used when this is the main stage function
(defun %main-return (code-obj implicit env)
  ;; If you make changes here, look at %emit to see if it needs
  ;; similar changes
  (let ((type (primary-type code-obj)))
    (cond
      ((and implicit (v-voidp type))
       (values (copy-compiled
                code-obj
                :return-set (or (return-set code-obj) (make-type-set))
                :pure nil)
               env))
      ((> (length (type-set code-obj)) 1)
       (let* ((mvals (rest (coerce (type-set code-obj) 'list)))
              (types (mapcar #'v-type-of mvals))
              (glsl-lines (mapcar #'glsl-name mvals)))
         (copy-compiled
          (merge-progn
           (with-fresh-env-scope (fresh-env env)
             (env-> (p-env fresh-env)
               (merge-multi-env-progn
                (%mapcar-multi-env-progn
                 (lambda (p-env type gname)
                   (compile-let (gensym) (type->type-spec type)
                                nil p-env gname))
                 p-env types glsl-lines))
               ;; We compile these ↓↓, however we dont include them in the ast
               (compile-form (%default-out-for-stage code-obj p-env)
                             p-env)
               (compile-form (mvals->out-form code-obj p-env)
                             p-env)
               (compile-form '(glsl-expr "return" :void) p-env)))
           env)
          :return-set (type-set code-obj))))
      (t (let ((ret-set
                (if (stage-where-first-return-is-position-p (stage env))
                    (make-type-set)
                    (make-type-set type))))
           (copy-compiled
            (with-fresh-env-scope (fresh-env env)
              (compile-form `(progn
                               ,(%default-out-for-stage code-obj fresh-env)
                               (glsl-expr "return" :void))
                            fresh-env))
            :return-set ret-set
            :pure nil))))))

;; fragment comes first as it doesnt restrict the exit type...this is a bug
;; really as fragment out-var should be vec4...We should have a case for
;; when context includes all stages, in which case any type is allowed
(defun %default-out-for-stage (code-obj env)
  (let ((stage (stage env)))
    (if (stage-where-first-return-is-position-p (stage env))
        (if (v-type-eq (primary-type code-obj) (type-spec->type :vec4))
            `(setq varjo-lang::gl-position ,code-obj)
            (error 'vertex-stage-primary-type-mismatch
                   :prim-type (primary-type code-obj)))
        `(glsl-expr ,(format nil "~a = ~~a" (nth-return-name 0 stage t))
                    :void ,code-obj))))
