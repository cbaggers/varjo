(in-package :varjo)

(defmethod print-object ((object v-function) stream)
  (with-slots (name argument-spec return-spec) object
    (format stream "#<V-FUNCTION ~s ~s -> ~s>"
            name
            (if (eq t argument-spec)
                '(t*)
		(mapcar #'type-of argument-spec))
            (typecase (first return-spec)
              (function t)
              (v-t-type (type-of (first return-spec)))
              (otherwise return-spec)))))
