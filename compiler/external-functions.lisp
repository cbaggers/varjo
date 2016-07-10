(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defparameter *external-functions*
  (make-hash-table))

(defun get-external-function-by-name (name)
  (gethash name *external-functions*))

(defclass external-function ()
  ((name :initarg :name :reader name)
   (in-args :initarg :in-args :reader in-args)
   (uniforms :initarg :uniforms :reader uniforms)
   (code :initarg :code :reader code)))

(defmethod add-external-function (name in-args uniforms code)
  (labels ((get-types (x) (mapcar #'second (in-args x))))
    (let* ((func (make-instance 'external-function
				:name name
				:in-args in-args
				:uniforms uniforms
				:code code))
	   (funcs (cons func (get-external-function-by-name name))))
      (setf (gethash name *external-functions*)
	    (remove-duplicates funcs :key #'get-types :test #'equal
			       :from-end t))
      func)))

(defmethod delete-external-function (name in-args-types)
  (labels ((get-types (x) (mapcar #'second (in-args x))))
    (let* ((funcs (get-external-function-by-name name)))
      (setf (gethash name *external-functions*)
	    (remove in-args-types funcs :key #'get-types :test #'equal))
      t)))

(defmethod func-need-arguments-compiledp ((func external-function))
  t)

(defmethod v-special-functionp ((func external-function))
  nil)

(defmethod v-argument-spec ((func external-function))
  (mapcar λ(type-spec->type (second _)) (in-args func)))
