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
