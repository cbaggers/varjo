(defun make-none-ob ()
  (let ((none-type (gen-none-type)))
    (make-code-obj
     none-type nil
     :node-tree (ast-node! :none nil none-type nil nil))))
