(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; slot-value

(defun struct-slot-accessor (struct-type slot-name)
  (assert (typep struct-type 'v-struct))
  (with-slots ((slots varjo.internals::slots)) struct-type
    (let ((def (find slot-name slots :key #'first)))
      (assert def ()
              "Varjo: slot-value could not find a slot named ~a in ~a"
              slot-name (type->type-spec struct-type))
      (destructuring-bind (slot-name struct-type accessor glsl-string) def
        (declare (ignore slot-name struct-type glsl-string))
        accessor))))

(v-defspecial slot-value (instance slot-name)
  :args-valid t
  :return
  (vbind (inst-obj inst-env) (compile-form instance env)
    (let* ((inst-type (primary-type inst-obj))
           (inst-type (if (v-typep inst-type 'v-block-struct)
                          (v-element-type inst-type)
                          inst-type)))
      (assert (v-typep inst-type 'v-struct)
              () 'slot-value-on-non-struct
              :type inst-type :slot-name slot-name)
      (let* ((accessor (struct-slot-accessor inst-type slot-name)))
        (compile-form
         `(,accessor ,inst-obj)
         inst-env)))))

;;------------------------------------------------------------
;; with-slots

(v-defmacro with-slots (slots form &body body)
  (let* ((name (gensym "with-slots-tmp"))
         (bindings (mapcar λ(dbind (mname &optional sname) (ensure-list _)
                              (let ((sname (or sname mname)))
                                `(,mname (slot-value ,name ,sname))))
                           slots)))
    `(let ((,name ,form))
       (symbol-macrolet ,bindings ,@body))))

;;------------------------------------------------------------
;; with-accessors

(v-defmacro with-accessors (bindings form &body body)
  (let* ((name (gensym "with-slots-tmp"))
         (bindings (mapcar λ(dbind (accessor-symbol-name accessor-name)
                                (ensure-list _)
                              `(,accessor-symbol-name (,accessor-name ,name)))
                           bindings)))
    `(let ((,name ,form))
       (symbol-macrolet ,bindings ,@body))))

;; Below is proof to myself that with-accessors is dumb in CL too
;;
;; (defclass blep () ((blip :initarg :blip :accessor blipper)))
;;
;; (flet ((blipper (x) (error "dumb ~a" x)))
;;   (with-accessors ((b blipper)) tmp0
;;     b))
