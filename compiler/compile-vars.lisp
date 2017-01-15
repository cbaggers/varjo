(in-package :varjo)
(in-readtable fn:fn-reader)

(defun v-variable->code-obj (var-name v-value from-higher-scope env)
  (let ((var-type (v-type v-value)))
    (assert (flow-ids var-type) (var-type)
            "Hmm, v-variable->code-obj failed as ~s has no flow-ids" var-type)
    (let ((code-obj
           (make-code-obj var-type
                          (unless (typep var-type 'v-compile-time-value)
                            (gen-variable-string var-name v-value))
                          :place-tree `((,var-name ,v-value))
                          :node-tree (ast-node! :get var-name
                                                var-type
                                                env env))))
      (if from-higher-scope
          (if (or (eq t (v-allowed-outer-vars env))
                  (find var-name (v-allowed-outer-vars env)))
              (add-higher-scope-val code-obj v-value)
              (error 'symbol-unidentified :sym var-name))
          code-obj))))

(defun %v-value->code (v-val env)
  (make-code-obj (v-type v-val) (v-glsl-name v-val)
                 :node-tree (ast-node! :get-v-value (list (v-glsl-name v-val))
                                       (v-type v-val)
                                       env env)))

;; [TODO] move error
(defun compile-symbol (code env)
  (let* ((var-name code)
         (v-value (get-var var-name env)))
    (if v-value
        (let* ((val-scope (v-function-scope v-value))
               (from-higher-scope (and (> val-scope 0)
                                       (< val-scope (v-function-scope env)))))
          (v-variable->code-obj var-name v-value from-higher-scope env))
        (let ((constant-to-inject (when (constantp var-name)
                                    (funcall *constant-inject-hook* var-name))))
          (cond
            (constant-to-inject (compile-form constant-to-inject env))
            ((suitable-symbol-for-stemcellp var-name env)
             (let ((scell (make-stem-cell code env))
                   (assumed-type (funcall *stemcell-infer-hook* var-name)))
               (if assumed-type
                   (add-type-to-stemcell-code scell assumed-type)
                   scell)))
            (t (error 'symbol-unidentified :sym code)))))))


(defmacro with-constant-inject-hook (func &body body)
  (let ((func-name (gensym "hook")))
    `(let* ((,func-name ,func)
            (*constant-inject-hook* ,func-name))
       ,@body)))
