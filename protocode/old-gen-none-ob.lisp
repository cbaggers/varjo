(defun make-none-ob ()
  (let ((none-type (gen-none-type)))
    (make-compiled
     :type none-type
     :current-line nil
     :node-tree (ast-node! :none nil none-type nil nil))))
