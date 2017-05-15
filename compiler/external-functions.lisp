(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defparameter *external-functions*
  (make-hash-table))

(defun get-external-function-by-name (name env)
  (let ((funcs (gethash name *external-functions*)))
    (if env
        (let ((v (get-version-from-context env)))
          (remove-if-not λ(or (null (glsl-versions _))
                              (member v (glsl-versions _)))
                         funcs))
        funcs)))

(defmethod add-external-function (name in-args uniforms code
                                  &optional valid-glsl-versions)
  (quick-check-of-arg-type-validity name (append in-args uniforms))
  (labels ((get-types (x) (mapcar #'second (in-args x))))
    (let* ((func (make-instance 'external-function
                                :name name
                                :in-args in-args
                                :uniforms uniforms
                                :code code
                                :glsl-versions valid-glsl-versions))
           (funcs (cons func (get-external-function-by-name name nil))))
      (setf (gethash name *external-functions*)
            (remove-duplicates funcs :key #'get-types :test #'equal
                               :from-end t))
      func)))

(defun quick-check-of-arg-type-validity (name args)
  (labels ((key (_)
             (handler-case
                 (progn (type-spec->type (second _)) nil)
               (unknown-type-spec (e)
                 (slot-value e 'type-spec)))))
    (let ((unknown (remove nil args :key #'key)))
      (when unknown
        (error 'external-function-invalid-in-arg-types
               :name name
               :args unknown)))
    args))

(defmethod delete-external-function (name in-args-types)
  (labels ((get-types (x) (mapcar #'second (in-args x))))
    (let* ((funcs (get-external-function-by-name name nil)))
      (setf (gethash name *external-functions*)
            (remove in-args-types funcs :key #'get-types :test #'equal))
      t)))

(defmethod func-need-arguments-compiledp ((func external-function))
  t)

(defmethod v-special-functionp ((func external-function))
  nil)

(defmethod v-argument-spec ((func external-function))
  (mapcar λ(type-spec->type (second _)) (in-args func)))

(defun format-external-func-for-error (func)
  `(,(name func) ,@(in-args func)
     ,@(when (uniforms func) (cons '&uniforms (uniforms func)))))
