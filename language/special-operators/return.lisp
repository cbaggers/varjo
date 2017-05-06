(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Return

(v-defspecial return (&optional (form '(values)))
  :args-valid t
  :return
  (%return-impl env form nil))

(v-defspecial implicit-return (&optional (form '(values)))
  :args-valid t
  :return
  (%return-impl env form t))

(defun %return-impl (env form implicit-p)
  (let ((new-env (fresh-environment
                  env :multi-val-base *return-var-name-base*)))
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
    (vbind (code-obj final-env) (compile-form form new-env)
      (if (return-set code-obj)
          (let ((ast (ast-node! 'return (node-tree code-obj)
                                (make-type-set)
                                env env)))
            (unless implicit-p
              (assert (type-sets-equal (type-set code-obj)
                                       (return-set code-obj))
                      () 'return-set-mismatch :form form ))
            (values (copy-compiled code-obj
                                   :type-set (make-type-set)
                                   :node-tree ast)
                    final-env))
          (%values-for-return (list code-obj)
                              (list (extract-value-qualifiers code-obj))
                              final-env)))))

;; (error 'nil-return-set
;;        :form `(return ,form)
;;        :possible-set (return-set code-obj))
