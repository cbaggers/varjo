(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Macro for defining vari.types

;; {TODO} proper errors
(defmacro def-v-type-class (name direct-superclass direct-slots &rest options)
  (unless (eq name 'v-type)
    (assert (and (listp direct-superclass)
                 (symbolp (first direct-superclass))
                 (or (= (length direct-superclass) 1)
                     (and (= (length direct-superclass) 2)
                          (find 'v-ephemeral-type direct-superclass))))
            ()
            "Varjo: All types must specify one superclass, this will usually be v-type"))
  ;;
  (let ((direct-superclass (mapcar #'v-type-name direct-superclass))
        (v-name (v-type-name name)))
    (unless (find 'type->type-spec direct-slots :key #'first)
      (push `(type->type-spec :initform ',name :accessor type->type-spec)
            direct-slots))

    ;; Register the type name and possibly the kwd equivalent
    (pushnew name *registered-types*)
    (when (and (equal (package-name (symbol-package name)) "VARJO")
               (equal (subseq (symbol-name name) 2) "V-")
               (not (member name '(v-type))))
      (pushnew (kwd (subseq (symbol-name name) 2))  *registered-types*))

    `(progn
       (defclass ,v-name ,direct-superclass
         ,(if (eq name 'v-type)
              direct-slots
              (cons `(superclass :initform ',(first direct-superclass))
                    direct-slots))
         ,@options)
       ',name)))

;;------------------------------------------------------------
