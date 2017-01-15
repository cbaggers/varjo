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

(defun v-value-equal (a b)
  (equal (v-glsl-name a) (v-glsl-name b)))

(defmethod v-type-of ((val v-value))
  (v-type val))

(defun add-glsl-vars (env source)
  (loop :for (restrict . vars) :in source
     :if (or (equal restrict t)
             (context-ok-given-restriction (v-context env) (listify restrict)))
     :do (loop :for (lisp-name glsl-name type-spec setable) :in vars :do
            (let ((type (type-spec->type type-spec (%gl-flow-id!))))
              (%add-var lisp-name (v-make-value
                                   type env :glsl-name glsl-name
                                   :read-only (not setable))
                        env)
              (add-reserved-lisp-name lisp-name env glsl-name))))
  env)

;;--------------------------------------------------



(defun make-mval (v-value &optional qualifiers)
  (make-instance 'mval :value v-value :qualifiers qualifiers))

(defun mval->out-form (mval env)
  (declare (ignore env))
  (with-slots (value qualifiers) mval
    `(%out (,(gensym "OUT") ,@qualifiers) ,value)))
