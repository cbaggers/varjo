(in-package :varjo)
(in-readtable fn:fn-reader)

(defun v-variable->code-obj (var-name v-value from-higher-scope env)
  (let ((code-obj (make-code-obj (v-type v-value)
				 (gen-variable-string var-name v-value)
				 :flow-ids (flow-ids v-value)
				 :place-tree `((,var-name ,v-value))
				 :node-tree (ast-node! :get var-name
						       (v-type v-value)
						       (flow-ids v-value)
						       env env))))
    (if from-higher-scope
        (add-higher-scope-val code-obj v-value)
        code-obj)))

(defun %v-value->code (v-val env)
  (make-code-obj (v-type v-val) (v-glsl-name v-val)
		 :flow-ids (flow-ids v-val)
		 :node-tree (ast-node! :get-v-value (list (v-glsl-name v-val))
				       (v-type v-val) (flow-ids v-val)
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
