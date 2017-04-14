(in-package :varjo)
(in-readtable :fn.reader)

(v-defun aref (x i) "~a[~a]" (v-array v-int) (:element 0) :v-place-index 0)
(v-defun aref (x i) "~a[~a]" (v-vector v-int) (:element 0) :v-place-index 0)

(v-defspecial aref ((arr v-block-array) (index v-int))
  :return
  (let* ((type (code-type arr))
         (elem-type (set-flow-id (v-element-type type) (flow-id!))))
    (values
     (merge-obs (list arr index)
                :type elem-type
                :current-line (format nil "~a.~a[~a]"
                                      (block-name type)
                                      (current-line arr)
                                      (current-line index))
                :node-tree (ast-node! 'aref (list arr index)
                                      elem-type env env))
     env)))
