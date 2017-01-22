(in-package :varjo)

(defmacro v-defmacro (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (subst '&rest '&body
                      (if context-pos (subseq args 0 context-pos) args)
                      :test #'symbol-name-equal)))
    `(progn (add-macro ',name (lambda ,args ,@body) ,context *global-env*)
            ',name)))


(defmacro v-define-compiler-macro (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (subst '&rest '&body
                      (if context-pos (subseq args 0 context-pos) args)
                      :test #'symbol-name-equal)))
    `(progn (add-compiler-macro ',name (lambda ,args ,@body) ,context *global-env*)
            ',name)))

;;------------------------------------------------------------
;; Symbol Macros

(defmethod make-symbol-macro (expansion-form function-scope env)
  (make-instance 'v-symbol-macro
                 :expansion expansion-form
                 :function-scope (or function-scope (v-function-scope env))))
