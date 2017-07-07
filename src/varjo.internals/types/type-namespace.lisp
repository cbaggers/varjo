(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(declaim (type hash-table *v-names*))
(defvar *v-names* (make-hash-table))

(defun v-type-name (symb)
  (or (gethash symb *v-names*)
      (setf (gethash symb *v-names*)
            (intern (format nil "~a.~a"
                            (package-name (symbol-package symb))
                            (symbol-name symb))
                    :vari.types.namespace))))

(defmacro declare-internal-type-name (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *v-names*) ',name)))
