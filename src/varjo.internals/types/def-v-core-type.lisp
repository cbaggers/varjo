(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Macro for defining vari.types

;; {TODO} proper errors
(defmacro define-v-type-class (name direct-superclass direct-slots &rest options)
  (unless (eq name 'v-type)
    (assert (and (listp direct-superclass)
                 (symbolp (first direct-superclass))
                 (or (= (length direct-superclass) 1)
                     (and (= (length direct-superclass) 2)
                          (find 'v-ephemeral-type direct-superclass))))
            ()
            "Varjo: All types must specify one superclass, this will usually be v-type"))
  ;;
  `(progn
     (defclass ,name ,direct-superclass
       ((type-name :initform ',name)
        ,@(if (eq name 'v-type)
              direct-slots
              (cons `(superclass :initform ',(first direct-superclass))
                    direct-slots)))
       ,@options)
     (register-type-name ',name)
     ',name))

;;------------------------------------------------------------
