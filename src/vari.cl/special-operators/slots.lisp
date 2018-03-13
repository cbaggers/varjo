(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; slot-value

(defun struct-slot-accessor (struct-obj slot-name)
  (let* ((type (primary-type struct-obj)))
    (assert (typep type 'v-struct))
    (with-slots ((slots varjo.internals::slots)) type
      (let ((def (find slot-name slots :key #'first)))
        (assert def ()
                "Varjo: slot-value could not find a slot named ~a in ~a"
                slot-name (type->type-spec type))
        (destructuring-bind (slot-name type accessor glsl-string) def
          (declare (ignore slot-name type glsl-string))
          accessor)))))

(v-defspecial slot-value (instance slot-name)
  :args-valid t
  :return
  (vbind (inst-obj inst-env) (compile-form instance env)
    (assert (v-typep (primary-type inst-obj) 'v-struct) ()
            'slot-value-on-non-struct
            :type (v-type-of inst-obj) :slot-name slot-name)
    (let* ((accessor (struct-slot-accessor inst-obj slot-name)))
      (compile-form
       `(,accessor ,inst-obj)
       inst-env))))

;;------------------------------------------------------------
;; with-slots

(v-defmacro with-slots (slots form &body body)
  (let* ((name (gensym "with"))
         (bindings (mapcar λ(dbind (mname &optional sname) (ensure-list _)
                              (let ((sname (or sname mname)))
                                `(,mname (slot-value ,name ,sname))))
                           slots)))
    `(let ((,name ,form))
       (symbol-macrolet ,bindings ,@body))))

;;------------------------------------------------------------
;; with-accessors

(v-defmacro with-accessors (bindings form &body body)
  (let* ((name (gensym "instance"))
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
