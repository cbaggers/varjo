(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Debugging Compilation

(v-defspecial %break (&optional datum &rest args)
  :args-valid t
  :return
  (progn
    (assert (or (null datum) (stringp datum)) (datum)
            "Varjo: first argument to %break must be a format string or nil")
    (break (format nil "Varjo compiler breakpoint (~~a):~%~a~%~%~~a"
                   (apply #'format nil (or datum "~{~s~}") args))
           env
           (mapcar λ(compile-form _ env) args))
    (compile-form '(values) env)))

(v-defspecial %peek (form)
  :args-valid t
  :return
  (vbind (o e) (compile-form form env)
    (break "Varjo Peek:~%:code-obj ~s~%:env ~s" o e)
    (values o e)))

;;------------------------------------------------------------
