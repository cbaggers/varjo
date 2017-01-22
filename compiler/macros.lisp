(in-package :varjo)

;;------------------------------------------------------------
;; Regular Macros

(defmacro v-defmacro (name lambda-list &body body)
  (alexandria:with-gensyms (form-var environment)
    (vbind (context lambda-list)
        (extract-arg-pair lambda-list :&context)
      (vbind (maybe-env lambda-list)
          (extract-arg-pair lambda-list :&environment)
        (let* ((env-var (or maybe-env environment)))
          `(progn
             (add-form-binding
              (make-regular-macro ',name
                                  (lambda (,form-var ,env-var)
                                    (declare (ignorable ,env-var))
                                    (destructuring-bind ,lambda-list ,form-var
                                      ,@body))
                                  ,context
                                  *global-env*)
              *global-env*)
             ',name))))))

(defmethod make-regular-macro (name macro-function context env)
  (make-instance 'v-regular-macro
                 :name name
                 :macro-function macro-function
                 :context context
                 :function-scope (if (eq env *global-env*)
                                     0
                                     (v-function-scope env))))

;;------------------------------------------------------------
;; Symbol Macros

(defmethod make-symbol-macro (expansion-form function-scope env)
  (make-instance 'v-symbol-macro
                 :expansion expansion-form
                 :function-scope (or function-scope (v-function-scope env))))

;;------------------------------------------------------------
;; Compile Macros

(defmacro v-define-compiler-macro (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (subst '&rest '&body
                      (if context-pos (subseq args 0 context-pos) args)
                      :test #'symbol-name-equal)))
    `(progn (add-compiler-macro ',name (lambda ,args ,@body) ,context *global-env*)
            ',name)))

;;------------------------------------------------------------
;; Helpers

(defun extract-arg-pair (lambda-list key)
  (let* ((key-pos (position key lambda-list :test #'symbol-name-equal))
         (value (when key-pos
                  (first (subseq lambda-list (1+ key-pos)))))
         (cleaned (if key-pos
                      (append (subseq lambda-list 0 key-pos)
                              (subseq lambda-list (+ 2 key-pos)))
                      lambda-list)))
    (values value cleaned)))
