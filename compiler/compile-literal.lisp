(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-bool (code env)
  (let* ((flow-id (flow-id!))
         (type-set (make-type-set (type-spec->type 'v-bool flow-id))))
    (if code
        (make-compiled
         :type-set type-set
         :current-line "true"
         :node-tree (ast-node! :literal code type-set env env)
         :pure t)
        (make-compiled
         :type-set type-set
         :current-line "false"
         :node-tree (ast-node! :literal code type-set env env)
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
         (num-type (set-flow-id (get-number-type code) flow-id))
         (type-set (make-type-set num-type)))
    (make-compiled
     :type-set type-set
     :current-line (gen-number-string code num-type)
     :node-tree (ast-node! :literal code type-set env env)
     :pure t)))

(defun compile-array-literal (arr env)
  (assert (= (array-rank arr) 1) (arr)
          'multi-dimensional-array
          :dimensions (array-dimensions arr))
  (let* ((len (length arr))
         (elements (map 'list λ(compile-literal _ env) arr))
         (types (mapcar #'primary-type elements))
         (element-type (apply #'find-mutual-cast-type types))
         (array-type (v-array-type-of element-type len (flow-id!)))
         (glsl (gen-array-literal-string elements element-type env))
         (type-set (make-type-set array-type))
         (ast (ast-node! :literal arr type-set env env)))
    (make-compiled :type-set type-set
                   :current-line glsl
                   :used-types (list element-type)
                   :node-tree ast
                   :pure t)))

(defun compile-string-literal (str env)
  (let ((is (or (handler-case (parse-integer str)
                  (error () nil))
                (handler-case (parse-float:parse-float str)
                  (error () nil)))))
    (etypecase is
      (integer (compile-form is env))
      (float (compile-form `(glsl-expr ,str :float) env)))))
