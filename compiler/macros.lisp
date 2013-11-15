(in-package :varjo)

(defmacro v-defmacro (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (subst '&rest '&body 
                      (if context-pos (subseq args 0 context-pos) args)
                      :test #'symbol-name-equal)))
    `(progn (add-macro ',name (lambda ,args ,@body) ,context *global-env*)
            ',name)))
