(in-package :varjo)
(in-readtable fn:fn-reader)

(defun from-higher-scope-p (v-value env)
  (let ((val-scope (v-function-scope v-value)))
    (and (> val-scope 0) (< val-scope (v-function-scope env)))))

(defun valid-value-access-p (var-name env)
  (let* ((v-value (get-var var-name env)))
    (and v-value
         (if (from-higher-scope-p v-value env)
             (or (eq t (v-allowed-outer-vars env))
                 (find var-name (v-allowed-outer-vars env)))
             t))))

(defun v-variable->code-obj (var-name v-value env)
  (assert (flow-ids v-value) (v-value)
          "Varjo: Compiler Bug: v-variable->code-obj failed as ~s has no flow-ids"
          v-value)

  (let ((var-type (v-type v-value))
        (from-higher-p (from-higher-scope-p v-value env)))

    (unless (valid-value-access-p var-name env)
      (error 'symbol-unidentified :sym var-name))

    (vbind (expanded-var-form expanded-env)
        (expand-typed-var-macro (v-type v-value) var-name env)
      (if (eq expanded-var-form var-name)
          (let ((code-obj
                 (make-code-obj var-type
                                (unless (typep var-type 'v-ephemeral-type)
                                  (gen-variable-string var-name v-value))
                                :place-tree `((,var-name ,v-value))
                                :node-tree (ast-node! :get var-name
                                                      var-type
                                                      env env))))
            (if from-higher-p
                (add-higher-scope-val code-obj v-value)
                code-obj))
          (compile-form expanded-var-form env)))))

(defun %v-value->code (v-val env)
  (make-code-obj (v-type v-val) (v-glsl-name v-val)
                 :node-tree (ast-node! :get-v-value (list (v-glsl-name v-val))
                                       (v-type v-val)
                                       env env)))

(defun maybe-inject-missing-var (var-name env)
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

;; [TODO] move error
(defun compile-symbol (symbol env)
  (let ((v-value (get-var symbol env)))
    (if v-value
        (v-variable->code-obj symbol v-value env)
        (maybe-inject-missing-var symbol env))))

(defmacro with-constant-inject-hook (func &body body)
  (let ((func-name (gensym "hook")))
    `(let* ((,func-name ,func)
            (*constant-inject-hook* ,func-name))
       ,@body)))
