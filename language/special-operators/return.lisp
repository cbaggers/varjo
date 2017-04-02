(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Return

(v-defspecial %return (form)
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
    (let* ((code-obj (compile-form form new-env))
           (result
            (if (member :main (v-context env))
                (%main-return code-obj env)
                (%regular-value-return code-obj))))
      (values (copy-code result
                         :node-tree (ast-node! '%return (node-tree code-obj)
                                               (code-type result)
                                               env env))
              env))))

;; Used when this is a labels (or otherwise local) function
(defun %regular-value-return (code-obj)
  (let ((flow-result
         (if (multi-vals code-obj)
             (m-flow-id! (cons (flow-ids code-obj)
                               (mapcar (lambda (c)
                                         (flow-ids (multi-val-value c)))
                                       (multi-vals code-obj))))
             (flow-ids code-obj))))
    ;; {TODO} why not 'end-line' here? who is doing that?
    (let ((suppress-glsl (or (v-typep (code-type code-obj) 'v-void)
                             (v-typep (code-type code-obj)
                                      'v-ephemeral-type))))
      (copy-code
       code-obj :type (type-spec->type 'v-void flow-result)
       :current-line (unless suppress-glsl
                       (format nil "return ~a" (current-line code-obj)))
       :returns (cons (code-type code-obj) (multi-vals code-obj))
       :multi-vals nil
       :place-tree nil))))


;; Used when this is the main stage function
;; this
(defun %main-return (code-obj env)
  (if (multi-vals code-obj)
      (let* ((mvals (multi-vals code-obj))
             (v-vals (mapcar #'multi-val-value mvals))
             (types (mapcar #'v-type-of v-vals))
             (glsl-lines (mapcar #'v-glsl-name v-vals)))

        (merge-progn
         (with-fresh-env-scope (fresh-env env)
           (env-> (p-env fresh-env)
             (merge-multi-env-progn
              (%mapcar-multi-env-progn
               (lambda (p-env type gname)
                 (compile-let (gensym) (type->type-spec type)
                              nil p-env gname))
               p-env types glsl-lines))
             (compile-form (%default-out-for-stage code-obj p-env) p-env)
             (compile-form `(progn ,@(mapcar λ(mval->out-form _ env)
                                             (multi-vals code-obj)))
                           p-env)))
         env))
      (with-fresh-env-scope (fresh-env env)
        (compile-form (%default-out-for-stage code-obj env)
                      fresh-env))))

;; fragment comes first as it doesnt restrict the exit type...this is a bug
;; really as fragment out-var should be vec4...We should have a case for
;; when context includes all stages, in which case any type is allowed
(defun %default-out-for-stage (form env)
  (let ((context (v-context env)))
    (cond ((member :fragment context) `(%out (,(gensym "OUTPUT-COLOR"))
                                             ,form))
          ((member :vertex context) `(setq varjo-lang::gl-position ,form))
          (t `(%out (,(gensym "OUTPUT-VAR"))
                    ,form)))))

;; {TODO} what if type of form is not value
(v-defspecial %out (name-and-qualifiers form)
  :args-valid t
  :return
  (let* ((form-obj (compile-form form env))
         (out-var-name (if (consp name-and-qualifiers)
                           (first name-and-qualifiers)
                           name-and-qualifiers))
         (qualifiers (when (consp name-and-qualifiers)
                       (rest name-and-qualifiers)))
         (glsl-name (safe-glsl-name-string out-var-name))
         (type (type-spec->type :void (flow-id!))))
    (values
     (end-line
      (copy-code
       form-obj :type type
       :current-line (gen-out-var-assignment-string glsl-name form-obj)
       :to-block (to-block form-obj)
       :out-vars (cons `(,out-var-name
                         ,qualifiers
                         ,(v-make-value (code-type form-obj) env
                                        :glsl-name glsl-name))
                       (out-vars form-obj))
       :node-tree (ast-node! '%out (list name-and-qualifiers
                                         (node-tree form-obj))
                             type env env)
       :multi-vals nil
       :place-tree nil) t)
     env)))
