(defun make-none-ob ()
  (let ((none-type (gen-none-type)))
    (make-compiled
     :type-set (make-type-set none-type)
     :current-line nil
     :node-tree (ast-node! :none nil (make-type-set none-type) nil nil))))
