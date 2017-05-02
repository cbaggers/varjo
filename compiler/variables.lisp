(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defmethod v-make-value ((type v-type) env
                         &key (glsl-name (gensym)) function-scope read-only)
  (let ((flow-ids (flow-ids type)))
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :v-values
             :primary-type (type->type-spec type))))
  (make-instance 'v-value :type type :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))
                 :read-only read-only))

(defmethod v-make-uninitialized
    ((type v-type) env &key (glsl-name (gensym)) function-scope read-only)
  (let ((flow-ids (flow-ids type)))
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :v-values
             :primary-type (type->type-spec type))))
  (assert (not read-only))
  (make-instance 'uninitialized-value :type type :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))))

(defun v-value-equal (a b)
  (equal (glsl-name a) (glsl-name b)))

(defun add-glsl-vars (env)
  (labels ((add-vars (vars)
             (loop :for (lisp-name glsl-name type-spec setable) :in vars :do
                (let ((type (type-spec->type type-spec (%gl-flow-id!))))
                  (%add-symbol-binding
                   lisp-name (v-make-value
                              type env :glsl-name glsl-name
                              :read-only (not setable))
                   env)
                  (add-reserved-lisp-name lisp-name env glsl-name)))))
    (add-vars (assocr t *glsl-variables*))
    (loop :for kind :in *stage-type-names*
       :when (typep (stage env) kind)
       :do (add-vars (assocr kind *glsl-variables*)))
    env))

;;--------------------------------------------------

(defun postfix-glsl-index (base index)
  (assert (and (stringp base) (integerp index)))
  (format nil "~a_~a" base index))

;;--------------------------------------------------

(defgeneric prefix-in-block-to-glsl-name (var)
  (:method ((var input-variable))
    (make-instance 'input-variable
                   :name (name var)
                   :qualifiers (qualifiers var)
                   :glsl-name (prefix-in-block-to-glsl-name (glsl-name var))
                   :type (v-type-of var)))
  (:method ((var string))
    (format nil "~a.~a" *in-block-name* var)))
