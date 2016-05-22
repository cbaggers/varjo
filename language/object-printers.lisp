(in-package :varjo)

(defmethod print-object ((object v-function) stream)
  (with-slots (name argument-spec return-spec multi-return-vars) object
    (format stream "#<V-FUNCTION ~s ~s -> ~s>"
            name
            (if (eq t argument-spec)
                '(t*)
		(mapcar #'type-of argument-spec))
            (if multi-return-vars
                (cons return-spec multi-return-vars)
                (typecase return-spec
		  (function  t)
		  (v-t-type (type-of return-spec))
		  (otherwise return-spec))))))
