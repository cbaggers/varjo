(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Geometry

;; points
;; line-strip
;; triangle-strip

(v-defspecial end-point ()
  :context :geometry
  :return
  (vbind (c e) (compile-form `(glsl-expr "EndPrimitive()" :void) env)
    (warn "defspecial context restriction not working")
    (values (copy-code c) e)))

(v-defspecial end-line ()
  :context :geometry
  :return
  (vbind (c e) (compile-form `(glsl-expr "EndPrimitive()" :void) env)
    (warn "defspecial context restriction not working")
    (values (copy-code c) e)))

(v-defspecial end-triangle ()
  :context :geometry
  :return
  (vbind (c e) (compile-form `(glsl-expr "EndPrimitive()" :void) env)
    (warn "defspecial context restriction not working")
    (values (copy-code c) e)))

;;------------------------------------------------------------

(v-defspecial emit (form)
  :args-valid t
  :return
  (let ((new-env (fresh-environment env :multi-val-base "return")))
    ;; we create an environment with the signal to let any 'values' forms
    ;; down the tree know they will be caught and what their name prefix should
    ;; be.
    ;; We then compile the form using the augmented environment, the values
    ;; statements will expand and flow back as 'multi-vals' and the
    ;; current-line
    ;;
    ;; If you make changes here, look at %return to see if it needs
    ;; similar changes
    (let* ((code-obj (compile-form form new-env))
           (result (%emit code-obj new-env))
           (ast (ast-node! 'emit (node-tree code-obj)
                           (code-type result)
                           env env))
           (ret-set (or (return-set result)
                        (error 'nil-return-set
                               :form (list 'emit form)
                               :possible-set (return-set code-obj)))))
      (values (copy-code result
                         :node-tree ast
                         :return-set ret-set
                         :pure nil)
              env))))

(defun %emit (code-obj env)
  ;; If you make changes here, look at %main-return to see if it needs
  ;; similar changes
  (let ((type (v-type-of code-obj)))
    (cond
      ((multi-vals code-obj)
       (let* ((mvals (multi-vals code-obj))
              (v-vals (mapcar #'multi-val-value mvals))
              (types (mapcar #'v-type-of v-vals))
              (glsl-lines (mapcar #'v-glsl-name v-vals)))
         (copy-code
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
               (compile-form '(glsl-expr "EmitVertex()" :void) p-env)))
           env)
          :return-set (make-return-set-from-code-obj code-obj))))
      (t (let ((ret-set (if (stage-is env :vertex)
                            (make-return-set)
                            (make-return-set (make-return-val type)))))
           (copy-code
            (with-fresh-env-scope (fresh-env env)
              (compile-form `(progn
                               ,(%default-out-for-stage code-obj fresh-env)
                               (glsl-expr "EmitVertex()" :void))
                            fresh-env))
            :return-set ret-set))))))
