(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-bool (code env)
  (let ((flow-id (flow-id!))
        (bool-type (type-spec->type 'v-bool)))
    (if code
	(make-code-obj bool-type "true" :flow-ids flow-id
		       :node-tree (ast-node! :literal code bool-type flow-id
					     env env))
	(make-code-obj bool-type "false" :flow-ids flow-id
		       :node-tree (ast-node! :literal code bool-type flow-id
					     env env)))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (typecase x
    (single-float (type-spec->type 'v-float))
    (double-float (type-spec->type 'v-double))
    (integer (type-spec->type 'v-int))
    (otherwise (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (let ((num-type (get-number-type code))
	(flow-id (flow-id!)))
    (make-code-obj num-type (gen-number-string code num-type)
		   :flow-ids flow-id
		   :node-tree (ast-node! :literal code num-type flow-id
					 env env))))
