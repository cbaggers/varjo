(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defmethod v-make-value ((type v-type) env
                         &key (glsl-name (gensym)) function-scope read-only)
  (let ((flow-ids (flow-ids type)))
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :v-values
             :code-type (type->type-spec type))))
  (make-instance 'v-value :type type :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))
                 :read-only read-only))

(defmethod v-make-uninitialized
    ((type v-type) env &key (glsl-name (gensym)) function-scope read-only)
  (let ((flow-ids (flow-ids type)))
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :v-values
             :code-type (type->type-spec type))))
  (assert (not read-only))
  (make-instance 'uninitialized-value :type type :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))))

(defun v-value-equal (a b)
  (equal (v-glsl-name a) (v-glsl-name b)))

(defun add-glsl-vars (env source)
  (loop :for (restrict . vars) :in source
     :if (or (equal restrict t)
             (context-ok-given-restriction (v-context env) (listify restrict)))
     :do (loop :for (lisp-name glsl-name type-spec setable) :in vars :do
            (let ((type (type-spec->type type-spec (%gl-flow-id!))))
              (%add-symbol-binding lisp-name (v-make-value
                                              type env :glsl-name glsl-name
                                              :read-only (not setable))
                                   env)
              (add-reserved-lisp-name lisp-name env glsl-name))))
  env)

;;--------------------------------------------------

(defun make-mval (v-value &optional qualifiers)
  (make-instance 'mval :value v-value :qualifiers qualifiers))
