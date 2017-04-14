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
    (compile-form '(values) env)))

(v-defspecial %peek (form)
  :args-valid t
  :return
  (vbind (o e) (compile-form form env)
    (break "Varjo Peek:~%:code-obj ~s~%:env ~s" o e)
    (values o e)))

;;------------------------------------------------------------
