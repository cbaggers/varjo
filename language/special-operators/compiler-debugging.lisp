(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Debugging Compilation

(v-defspecial %break (&optional datum &rest args)
  :args-valid t
  :return
  (progn
    (break (format nil "Varjo compiler breakpoint:~%~s" (or datum ""))
           (mapcar λ(compile-form _ env) args))
    (let* ((none-type (gen-none-type))
           (node (make-code-obj
                  none-type nil
                  :node-tree (ast-node! :break (cons datum args)
                                        none-type nil nil))))
      (values node env))))

(v-defspecial %peek (form)
  :args-valid t
  :return
  (vbind (o e) (compile-form form env)
    (break "Varjo Peek:~%:code-obj ~s~%:env ~s" o e)
    (values o e)))

;;------------------------------------------------------------
