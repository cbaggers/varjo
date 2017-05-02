(in-package :varjo)

(v-def-glsl-template-fun %uint (x) "uint(~a)" (v-bool) v-uint)
(v-def-glsl-template-fun %uint (x) "uint(~a)" (v-float)  v-uint)
(v-def-glsl-template-fun %uint (x) "uint(~a)" (v-double)  v-uint)
(v-def-glsl-template-fun %uint (x) "uint(~a)" (v-int) v-uint)

(v-defspecial uint (val)
  :args-valid t
  :return
  (if (typep val '(integer 0 *))
      (let* ((flow-id (flow-id!))
             (type (type-spec->type :uint flow-id))
             (type-set (make-type-set type)))
        (values (make-compiled :type-set type-set
                               :current-line (format nil "~au" val)
                               :used-types (list type)
                               :node-tree (ast-node! 'uint val type-set
                                                     env env)
                               :pure t)
                env))
      (compile-form `(%uint ,val) env)))
