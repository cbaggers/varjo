(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Regular Macros

(v-defmacro macrolet (definitons &rest body)
  (unless body (error 'body-block-empty :form-name 'macrolet))
  (reduce (lambda (accum definition)
            `(macrolet-1 ,definition ,accum))
          definitons
          :initial-value `(progn ,@body)))

(v-defspecial macrolet-1 (definition &rest body)
  :args-valid t
  :return
  (let ((macro
         (dbind (name lambda-list &body body) definition
           (vbind (func-code context)
               (gen-macro-function-code name lambda-list body)
             (make-regular-macro name (compile nil func-code) context env)))))
    (with-fresh-env-scope (fresh-env env)
      (let ((new-env (add-form-binding macro fresh-env)))
        (compile-form `(progn ,@body) new-env)))))

;;------------------------------------------------------------
;; Symbol Macros

(v-defmacro symbol-macrolet (macrobindings &rest body)
  (unless body (error 'body-block-empty :form-name 'symbol-macrolet))
  (reduce (lambda (accum binding)
            (dbind (name expansion) binding
              `(symbol-macrolet-1 ,name ,expansion ,accum)))
          macrobindings
          :initial-value `(progn ,@body)))


(v-defspecial symbol-macrolet-1 (name expansion &rest body)
  :args-valid t
  :return
  (let* ((scope (v-function-scope env))
         (macro (make-symbol-macro expansion scope env)))
    (with-fresh-env-scope (fresh-env env)
      (let ((new-env (add-symbol-binding name macro fresh-env)))
        (compile-form `(progn ,@body) new-env)))))
