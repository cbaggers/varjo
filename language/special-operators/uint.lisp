(in-package :varjo)

(v-defun %uint (x) "uint(~a)" (v-bool) v-uint)
(v-defun %uint (x) "uint(~a)" (v-float)  v-uint)
(v-defun %uint (x) "uint(~a)" (v-double)  v-uint)
(v-defun %uint (x) "uint(~a)" (v-int) v-uint)

(v-defspecial uint (val)
  :args-valid t
  :return
  (if (typep val '(integer 0 *))
      (let* ((flow-id (flow-id!))
             (type (type-spec->type :uint flow-id)))
        (values (code! :type type
                       :current-line (format nil "~au" val)
                       :used-types (list type)
                       :node-tree (ast-node! 'uint val type env env)
                       :pure t)
                env))
      (compile-form `(%uint ,val) env)))
