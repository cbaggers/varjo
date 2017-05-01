(in-package :varjo)
(in-readtable :fn.reader)

(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-array v-int) (:element 0) :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-vector v-int) (:element 0) :v-place-index 0)

(v-defspecial aref ((arr (v-block-array "n/a" t *)) (index v-int))
  :return
  (let* ((type (primary-type arr))
         (elem-type (set-flow-id (v-element-type type) (flow-id!)))
         (type-set (make-type-set elem-type)))
    (values
     (merge-compiled (list arr index)
                     :type-set type-set
                     :current-line (format nil "~a[~a].~a"
                                           (block-name type)
                                           (current-line index)
                                           (current-line arr t))
                     :node-tree (ast-node!
                                 'aref
                                 (list (node-tree arr)
                                       (node-tree index))
                                 type-set env env))
     env)))
