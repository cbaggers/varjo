(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-bool (code env)
  (let ((flow-id (flow-id!)))
    (if code
	(make-code-obj 'v-bool "true" :flow-ids flow-id
		       :node-tree (ast-node! :literal code :bool flow-id
					     env env))
	(make-code-obj 'v-bool "false" :flow-ids flow-id
		       :node-tree (ast-node! :literal code :bool flow-id
					     env env)))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (cond ((floatp x) (type-spec->type 'v-float))
        ((integerp x) (type-spec->type 'v-int))
        (t (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (let ((num-type (get-number-type code))
	(flow-id (flow-id!)))
    (make-code-obj num-type (gen-number-string code num-type)
		   :flow-ids flow-id
		   :node-tree (ast-node! :literal code num-type flow-id
					 env env))))
