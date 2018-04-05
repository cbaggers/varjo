(in-package :vari.glsl)
(in-readtable fn:fn-reader)

(defun populate-functions ()
  (loop :for func-spec :in glsl-spec:*functions* :do
     (destructuring-bind (&key lisp-name name return args versions pure
                               &allow-other-keys)
         func-spec
       (assert lisp-name)
       (register-reserved-name name)
       (let* ((lisp-name (or (find-symbol lisp-name :cl)
                             (intern lisp-name :vari.glsl)))
              (lisp-arg-types
               (loop :for (nil type . qualifiers) :in args :collect
                  (let ((ltype (type-spec->type (parse-gl-type-name type))))
                    (qualify-type
                     ltype
                     (loop
                        :for qualifier :in qualifiers
                        :unless (stringp qualifier)
                        :collect (parse-qualifier (if (eq qualifier :inout)
                                                      :in/out
                                                      qualifier)))))))
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
         (varjo.internals::add-global-form-binding
          (varjo.internals::make-function-obj
           lisp-name transform versions lisp-arg-types
           lisp-return :v-place-index nil :glsl-name nil
           :flow-ids (varjo.internals::%gl-flow-id!)
           ;; {TODO} n-of doesnt make new instances. This is problem here
           ;;                ↓↓↓
           :in-arg-flow-ids (n-of (varjo.internals::%gl-flow-id!)
                                  (length args))
           :pure pure))))))

(populate-functions)
