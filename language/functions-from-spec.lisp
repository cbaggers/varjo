(in-package :varjo)
(in-readtable fn:fn-reader)

(defmacro populate-functions ()
  `(progn
     ,@(loop :for func-spec :in glsl-spec:*functions* :append
          (destructuring-bind (&key name return args versions pure
                                    &allow-other-keys)
              func-spec
            (let* ((parsed-name (parse-gl-func-name name))
                   (lisp-name (or (find-symbol parsed-name :cl)
                                  (intern parsed-name  :varjo-lang)))
                   (arg-types (mapcar #'second args))
                   (lisp-arg-types (mapcar λ(type-spec->type
                                             (parse-gl-type-name _))
                                           arg-types))
                   (spec-returns (ensure-list return))
                   (lisp-return (if (equal spec-returns '("void"))
                                    (make-type-set)
                                    (map 'vector
                                         λ(type-spec->type
                                           (parse-gl-type-name _))
                                         spec-returns)))
                   (transform (format nil "~a(~{~a~^,~})" name
                                      (loop :for i :below (length args)
                                         :collect "~a")))
                   (versions (mapcar #'kwd versions)))
              (unless (some #'third args)
                `((add-form-binding
                   (make-function-obj
                    ',lisp-name ,transform ',versions ',lisp-arg-types
                    ',lisp-return :v-place-index nil :glsl-name nil
                    :flow-ids (%gl-flow-id!)
                    :in-arg-flow-ids (n-of (%gl-flow-id!) ,(length args))
                    :pure ,pure)
                   *global-env*)
                  (export ',lisp-name :varjo-lang))))))))

(populate-functions)
