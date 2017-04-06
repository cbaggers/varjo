(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Return

;; - √ Rename %return to return
;;  - √ update places where this is used
;; - √ comment out %out
;; - √ add ability to specify 'out' metadata in values form
;;  - √ how should be present this?
;;   - [x] 'declaring' form special form? isnt that just 'the'?
;;   - [x] Gotta use something other than values as we could legitimately want
;;         to return only one value.
;;   - [x] using metadata sounds inevitable
;;    - √ true, but currently metadata can only be assigned to a singular
;;        flow-id this is no good for this. So maybe values will have to do for
;;        now.
;;    - √ Actually values will be fine, if you want to qualify then just have a
;;        single values form. Will work well enough for now
;;  - √ reuse the out-vars slot from code-obj
;;   - √ change it's name to return-set to force breakage
;;   - √ out-vars should be a vector, not a list (it is position sensitive and
;;       so shouldnt be trivially extensible)
;;   - √ whole set of out-vars need to be set at once
;; - Add merging with validation of return-set
;; - √ remove %out
;; - update all dependent code, testing heavily
;; - Update AST generation to add (values) so as not to end up with nesting
;;   returns.. wait..how do we handle that now?
;; - Make ret-val type for the elements of the return set

(v-defspecial return (&optional (form '(values)))
  :args-valid t
  :return
  (let ((new-env (fresh-environment
                  env
                  :multi-val-base "return")))
    ;; we create an environment with the signal to let any 'values' forms
    ;; down the tree know they will be caught and what their name prefix should
    ;; be.
    ;; We then compile the form using the augmented environment, the values
    ;; statements will expand and flow back as 'multi-vals' and the current-line
    ;; now there are two styles of return:
    ;; - The first is for a regular function, in which multivals become
    ;;   out-arguments and the current-line is returned
    ;; - The second is for a shader stage in which the multi-vars become
    ;;   output-variables and the current line is handled in a 'context'
    ;;   specific way.
    (warn "AHHH horse, we need to handle the return qualifiers")
    (let* ((code-obj (compile-form form new-env))
           (result
            (if (member :main (v-context env))
                (%main-return code-obj env)
                (%regular-value-return code-obj))))
      (values (copy-code result
                         :node-tree (ast-node! 'return (node-tree code-obj)
                                               (code-type result)
                                               env env)
                         :return-set (or (return-set result)
                                         (error 'nil-return-set)))
              env))))

;; Used when this is a labels (or otherwise local) function
(defun %regular-value-return (code-obj)
  (let* ((flow-result
          (if (multi-vals code-obj)
              (m-flow-id! (cons (flow-ids code-obj)
                                (mapcar (lambda (c)
                                          (flow-ids (multi-val-value c)))
                                        (multi-vals code-obj))))
              (flow-ids code-obj)))
         (suppress-glsl (or (v-typep (code-type code-obj) 'v-void)
                            (v-typep (code-type code-obj)
                                     'v-ephemeral-type)))
         (ret-set (make-return-set-from-code-obj code-obj)))
    ;;
    (copy-code
     code-obj :type (type-spec->type 'v-void flow-result)
     :current-line (unless suppress-glsl
                     (format nil "return ~a" (current-line code-obj)))
     :returns (cons (code-type code-obj) (multi-vals code-obj))
     :multi-vals nil
     :place-tree nil
     :return-set ret-set)))


;; Used when this is the main stage function
(defun %main-return (code-obj env)
  (if (multi-vals code-obj)
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
              ;; We must compile these ↓↓, however we dont want them in the ast
              (compile-form (%default-out-for-stage code-obj p-env) p-env)
              (compile-form (mvals->out-form code-obj p-env) p-env)))
          env)
         :return-set (make-return-set-from-code-obj code-obj)))
      (copy-code
       (with-fresh-env-scope (fresh-env env)
         (compile-form (%default-out-for-stage code-obj fresh-env)
                       fresh-env))
       :return-set (make-single-val-return-set env (v-type-of code-obj)))))


;; fragment comes first as it doesnt restrict the exit type...this is a bug
;; really as fragment out-var should be vec4...We should have a case for
;; when context includes all stages, in which case any type is allowed
(defun %default-out-for-stage (code-obj env)
  (let ((context (v-context env)))
    (if (member :vertex context)
        `(setq varjo-lang::gl-position ,code-obj)
        `(glsl-expr ,(format nil "~a = ~~a" (nth-return-name 0))
                    :void ,code-obj))))
