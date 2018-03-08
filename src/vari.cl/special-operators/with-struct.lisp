(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; with-slots

(defun struct-slot-accessor (struct-obj slot-name)
  (let* ((type (primary-type struct-obj)))
    (assert (typep type 'v-struct))
    (with-slots ((slots varjo.internals::slots)) type
      (let ((def (find slot-name slots :key #'first)))
        (assert def ()
                "Varjo: with-slots could not find a slot named ~a in ~a"
                slot-name (type->type-spec type))
        (destructuring-bind (slot-name type accessor glsl-string) def
          (declare (ignore slot-name type glsl-string))
          accessor)))))

(v-defspecial with-slots (slots instance &rest body)
  :args-valid t
  :return
  (vbind (inst-obj inst-env) (compile-form instance env)
    (let* ((accessors (mapcar λ(struct-slot-accessor inst-obj _) slots))
           (ginst (gensym "instance")))
      (compile-form
       `(let ((,ginst ,inst-obj))
          (symbol-macrolet ,(loop :for slot :in slots :for acc :in accessors
                               :collect `(,slot (,acc ,ginst)))
            ,@body))
       inst-env))))

;;------------------------------------------------------------
;; with-accessors

(defun struct-accessors (struct-obj)
  (let* ((type (primary-type struct-obj)))
    (assert (typep type 'v-struct))
    (with-slots ((slots varjo.internals::slots)) type
      (mapcar #'third slots))))

(v-defspecial with-accessors (slots instance &rest body)
  :args-valid t
  :return
  (vbind (inst-obj inst-env) (compile-form instance env)
    (let* ((type (primary-type inst-obj))
           (struct-accessors (struct-accessors inst-obj))
           (ginst (gensym "instance")))
      (compile-form
       `(let ((,ginst ,inst-obj))
          (symbol-macrolet ,(loop :for (name accessor) :in slots
                               :do (assert (find accessor struct-accessors) ()
                                           "Varjo: with-accessors could not find accessor named ~a not found in ~a"
                                           accessor (type->type-spec type))
                               :collect `(,name (,accessor ,ginst)))
            ,@body))
       inst-env))))
