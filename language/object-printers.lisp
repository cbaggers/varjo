(in-package :varjo)

(defmethod print-object ((object v-function) stream)
  (with-slots (name argument-spec return-spec multi-return-vars) object
    (format stream "#<V-FUNCTION - ~s ~s -> ~s>"
            name
            (if (eq t argument-spec)
                '(t*))
            (if multi-return-vars
                (cons return-spec multi-return-vars)
                (if (functionp return-spec)
                    t
                    return-spec)))))
