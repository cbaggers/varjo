(in-package :vari.glsl)
(in-readtable fn:fn-reader)

(defun populate-functions (spec-funcs infix-p)
  (loop :for func-spec :in spec-funcs :do
     (destructuring-bind (&key lisp-name name return args versions pure
                               &allow-other-keys)
         func-spec
       (assert lisp-name)
       (register-reserved-name name)
       (let* ((orig-args args)
              (out-pos (position-if (lambda (x) (find :out x)) args))
              (outs (when out-pos (subseq orig-args out-pos)))
              (args (if out-pos (subseq args 0 out-pos) orig-args)))
         (assert (every (lambda (x) (find :out x)) outs) ()
                 "Bug: Spec func arg qualified as 'out' before standard params")
         (let* ((lisp-arg-types
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
                (lisp-name (or (find-symbol lisp-name :cl)
                               (intern lisp-name :vari.glsl)))
                (spec-returns (append (alexandria:ensure-list return)
                                      (mapcar #'second outs)))
                (lisp-return (if (equal spec-returns '("void"))
                                 (varjo.internals::make-type-set)
                                 (map 'vector
                                      λ(type-spec->type
                                        (parse-gl-type-name _))
                                      spec-returns)))
                (transform (if infix-p
                               (let ((template (format nil "(~~{~~a~~^ ~~^~a~~^ ~~})" name)))
                                 (format nil template
                                         (loop :for i :below (length orig-args)
                                            :collect "~a")))
                               (format nil "~a(~{~a~^,~})" name
                                       (loop :for i :below (length orig-args)
                                          :collect "~a"))))
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
             :pure pure)))))))

(populate-functions glsl-spec:*functions* nil)
(populate-functions glsl-spec:*operators* t)
