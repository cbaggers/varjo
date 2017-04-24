(in-package :varjo)
(in-readtable fn:fn-reader)

(defun v-variable->code-obj (var-name v-value env)
  (let* ((var-type (v-type-of v-value))
         (from-higher-scope (binding-in-higher-scope-p v-value env)))
    (when from-higher-scope
      (assert (or (eq t (v-allowed-outer-vars env))
                  (find var-name (v-allowed-outer-vars env)))
              () 'symbol-unidentified :sym var-name))
    (assert (flow-ids var-type) (var-type)
            "Hmm, v-variable->code-obj failed as ~s has no flow-ids" var-type)
    (let* ((type-set (make-type-set var-type))
           (code-obj
            (make-compiled
             :type-set type-set
             :current-line (gen-variable-string var-name v-value)
             :place-tree `((,var-name ,v-value))
             :node-tree (ast-node! :get var-name type-set env env)
             :pure t)))
      (if from-higher-scope
          (add-higher-scope-val code-obj v-value)
          code-obj))))

(defun maybe-add-constant-or-stemcell (var-name env)
  (let ((constant-to-inject (when (constantp var-name)
                              (funcall *constant-inject-hook* var-name))))
    (cond
      (constant-to-inject (compile-form constant-to-inject env))
      ((suitable-symbol-for-stemcellp var-name env)
       (let ((scell (make-stem-cell var-name env))
             (assumed-type (funcall *stemcell-infer-hook* var-name)))
         (if assumed-type
             (add-type-to-stemcell-code scell assumed-type)
             scell)))
      (t (error 'symbol-unidentified :sym var-name)))))

(defun expand-symbol-macro (binding env)
  (compile-form (expansion binding) env))

(defun compile-symbol (symbol env &key allow-unbound)
  (let ((binding (get-symbol-binding symbol t env)))
    (etypecase binding
      (uninitialized-value (if allow-unbound
                               (v-variable->code-obj symbol binding env)
                               (error 'uninitialized-var :name symbol)))
      (v-symbol-macro (expand-symbol-macro binding env))
      (v-value (v-variable->code-obj symbol binding env))
      (null (maybe-add-constant-or-stemcell symbol env)))))

(defmacro with-constant-inject-hook (func &body body)
  (let ((func-name (gensym "hook")))
    `(let* ((,func-name ,func)
            (*constant-inject-hook* ,func-name))
       ,@body)))
