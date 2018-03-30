(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun register-type-name (name)
  (assert (symbolp name))
  (setf (gethash name *registered-types*) t))

(defun type-name-known (name)
  (gethash name *registered-types*))

(defun vtype-existsp (type-name)
  (let ((type-name (resolve-name-from-alternative type-name)))
    (etypecase type-name
      (symbol (type-name-known type-name))
      (list (type-name-known (first type-name))))))

;;------------------------------------------------------------
