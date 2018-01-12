(in-package :vari.glsl)
(in-readtable fn:fn-reader)

(defun populate-functions ()
  (loop :for func-spec :in glsl-spec:*functions* :do
     (destructuring-bind (&key lisp-name name return args versions pure
                               &allow-other-keys)
         func-spec
       (assert lisp-name)
       (let* ((lisp-name (or (find-symbol lisp-name :cl)
                             (intern lisp-name :vari.glsl)))
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
           (varjo.internals::add-global-form-binding
            (varjo.internals::make-function-obj
             lisp-name transform versions lisp-arg-types
             lisp-return :v-place-index nil :glsl-name nil
             :flow-ids (varjo.internals::%gl-flow-id!)
             ;; {TODO} n-of doesnt make new instances. This is problem here
             ;;                ↓↓↓
             :in-arg-flow-ids (n-of (varjo.internals::%gl-flow-id!)
                                    (length args))
             :pure pure)))))))

(populate-functions)
