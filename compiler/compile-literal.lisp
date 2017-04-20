(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-bool (code env)
  (let* ((flow-id (flow-id!))
         (bool-type (type-spec->type 'v-bool flow-id)))
    (if code
        (make-code-obj bool-type "true" :node-tree (ast-node! :literal code
                                                              bool-type
                                                              env env)
                       :pure t)
        (make-code-obj bool-type "false" :node-tree (ast-node! :literal code
                                                               bool-type
                                                               env env)
                       :pure t))))

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
                                         env env)
                   :pure t)))

(defun compile-array-literal (arr env)
  (assert (= (array-rank arr) 1) (arr)
          'multi-dimensional-array
          :dimensions (array-dimensions arr))
  (let* ((len (length arr))
         (elements (map 'list λ(compile-literal _ env) arr))
         (types (mapcar #'v-type-of elements))
         (element-type (apply #'find-mutual-cast-type types))
         (array-type (v-array-type-of element-type len (flow-id!)))
         (glsl (gen-array-literal-string elements element-type env))
         (ast (ast-node! :literal arr array-type env env)))
    (code! :type array-type
           :current-line glsl
           :used-types (list element-type)
           :node-tree ast
           :pure t)))
