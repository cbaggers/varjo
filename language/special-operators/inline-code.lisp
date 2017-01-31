(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Inline GLSL

(v-defspecial glsl-expr (glsl-string type-spec)
  :args-valid t
  :return
  (values
   (compile-glsl-expression-string glsl-string type-spec)
   env))

(defun compile-glsl-expression-string (current-line type)
  (let* ((type-obj (if (typep type 'v-type)
                       type
                       (type-spec->type type (flow-id!))))
         (flow-id (flow-ids type-obj)))
    (assert flow-id)
    (code! :type type-obj
           :current-line current-line
           :used-types (list type-obj)
           :node-tree (ast-node! 'glsl-string nil type-obj nil nil))))

(defun glsl-let (name-symbol name-string type value-form env)
  (let ((type-spec (if (typep type 'v-type) (type->type-spec type) type)))
    (compile-let name-symbol type-spec value-form env name-string)))


;;------------------------------------------------------------
;; Inline Lisp (Injecting lisp expressions as uniforms)

(v-defspecial lisp-code-as-uniform (uniform-name type-spec lisp-form)
  :args-valid t
  :return
  (values
   (inject-implicit-uniform uniform-name type-spec env lisp-form)
   env))

;;------------------------------------------------------------
