(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-bool (code env)
  (let* ((flow-id (flow-id!))
         (bool-type (type-spec->type 'v-bool flow-id)))
    (if code
        (make-code-obj bool-type "true" :node-tree (ast-node! :literal code
                                                              bool-type
                                                              env env))
        (make-code-obj bool-type "false" :node-tree (ast-node! :literal code
                                                               bool-type
                                                               env env)))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (typecase x
    (single-float (type-spec->type 'v-float))
    (double-float (type-spec->type 'v-double))
    (integer (type-spec->type 'v-int))
    (otherwise (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (let* ((flow-id (flow-id!))
         (num-type (set-flow-id (get-number-type code) flow-id)))
    (make-code-obj num-type (gen-number-string code num-type)
                   :node-tree (ast-node! :literal code num-type
                                         env env))))
