(in-package :varjo.internals)

;;----------------------------------------------------------------------

(defmacro v-declaim (&body declaration-specifiers)
  `(progn
     (apply-declaration-specifiers ',declaration-specifiers)))

(defun apply-declaration-specifiers (specifiers)
  (let* ((errors-0 (mapcar #'apply-declaration-specifier specifiers))
         (errors-1 (remove nil errors-0)))
    (declare (ignore errors-1))
    nil))

(defun apply-declaration-specifier (specifier)
  (case (first specifier)
    (inline (let ((funcs (rest specifier)))
              (mapcar #'declaim-func-inline funcs)))
    (otherwise (error "Varjo: v-declaim currently only supports 'inline', and only on functions~%defined with v-defun~%Recieved: ~s"
                      specifier))))

(defun declaim-func-inline (func-sig)
  (assert (listp func-sig) ()
          "Varjo: function signatures must be fully specified when attempting to declaim inline")
  (let* ((func (handler-case
                   (get-external-function-by-literal func-sig)
                 (error ()
                   (error "Varjo: Could not declaim ~s inline. It has either not been defined or was not defined using v-defun"
                          func-sig)))))
    (setf (declaimed-inline func) t)))

;;----------------------------------------------------------------------

#+nil
(v-declaim (inline (scale-float :float :int)))
