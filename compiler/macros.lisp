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

;;-------------------------------------------------------------------------

(defmacro v-define-var-macro (varjo-type (arg-name environment-var-name)
                              &body body)
  (alexandria:with-gensyms (type)
    `(defmethod expand-typed-var-macro
         ((,type ,varjo-type) ,arg-name ,environment-var-name)
       (declare (ignore ,type) (ignorable ,arg-name ,environment-var-name))
       ,@body)))

(defgeneric expand-typed-var-macro (type var-name env))

(defmethod expand-typed-var-macro (type var-name env)
  (declare (ignore type env))
  var-name)

;; (v-define-var-macro v-space (var-name env)
;;   ..)

;;-------------------------------------------------------------------------
