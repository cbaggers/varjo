(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Declarations
;;
;; The main logic for this is in value-metadata.lisp


;; {TODO} The declarations in 'locally' are meant to be lexically scoped.
;;        however so far our metadata always flows with the values.
;;        Resolve this later.
;;
(v-defspecial locally (&rest body)
  :args-valid t
  :return
  (vbind (body declarations) (extract-declares body)
    (compile-declares declarations env)
    (compile-form `(progn ,@body) env)))

;;------------------------------------------------------------
;; The

(v-defspecial the (type-name form)
  :args-valid t
  :return
  (let* ((compiled (compile-form form env))
         (obj (if (stemcellp (code-type compiled))
                  (add-type-to-stemcell-code compiled type-name)
                  (if (v-typep (code-type compiled)
                               (type-spec->type type-name))
                      compiled ;{TODO} proper error here
                      (error "Incorrect declaration that ~a was of type ~a"
                             compiled type-name)))))
    (values
     (copy-code
      obj
      :node-tree (ast-node! 'the (list type-name (node-tree compiled))
                            (code-type compiled) env env))
     env)))
