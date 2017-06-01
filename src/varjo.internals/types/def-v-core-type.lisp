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
  (let ((new-names (if (and (equal (package-name (symbol-package name)) "VARJO")
                            (equal (subseq (symbol-name name) 2) "V-")
                            (not (member name '(v-type))))
                       `(append (list ,(kwd (subseq (symbol-name name) 2))
                                      ',name)
                                *registered-types*)
                       `(cons ',name *registered-types*))))
    `(progn
       (defclass ,name ,direct-superclass
         ,(if (eq name 'v-type)
              direct-slots
              (cons `(superclass :initform ',(first direct-superclass))
                    direct-slots))
         ,@options)
       (setf *registered-types* (remove-duplicates ,new-names))
       ',name)))

;;------------------------------------------------------------
