(in-package :varjo.glsl)
(in-readtable fn:fn-reader)

(defmacro populate-functions ()
  `(progn
     ,@(loop :for func-spec :in glsl-spec:*functions* :append
          (destructuring-bind (&key name return args versions pure
                                    &allow-other-keys)
              func-spec
            (let* ((parsed-name (parse-gl-func-name name))
                   (lisp-name (or (find-symbol parsed-name :cl)
                                  (intern parsed-name :varjo.glsl)))
                   (arg-types (mapcar #'second args))
                   (lisp-arg-types (mapcar λ(type-spec->type
                                             (parse-gl-type-name _))
                                           arg-types))
                   (spec-returns (alexandria:ensure-list return))
                   (lisp-return (if (equal spec-returns '("void"))
                                    (varjo.internals::make-type-set)
                                    (map 'vector
                                         λ(type-spec->type
                                           (parse-gl-type-name _))
                                         spec-returns)))
                   (transform (format nil "~a(~{~a~^,~})" name
                                      (loop :for i :below (length args)
                                         :collect "~a")))
                   (versions (mapcar #'kwd versions)))
              (unless (some #'third args)
                `((varjo.internals::add-form-binding
                   (varjo.internals::make-function-obj
                    ',lisp-name ,transform ',versions ',lisp-arg-types
                    ',lisp-return :v-place-index nil :glsl-name nil
                    :flow-ids (varjo.internals::%gl-flow-id!)
                    ;; {TODO} n-of doesnt make new instances. This is problem here
                    ;;                ↓↓↓
                    :in-arg-flow-ids (n-of (varjo.internals::%gl-flow-id!)
                                           ,(length args))
                    :pure ,pure)
                   varjo.internals::*global-env*)
                  (export ',lisp-name :varjo.glsl))))))))

(populate-functions)
