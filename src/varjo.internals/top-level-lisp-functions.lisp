(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defvar *top-level-lisp-function-decls*
  (make-hash-table))

(defmethod get-top-level-lisp-function-decl-by-name (name env)
  (let ((funcs (gethash name *top-level-lisp-function-decls*)))
    (if env
        (let ((v (get-version-from-context env)))
          (remove-if-not λ(or (null (glsl-versions _))
                              (member v (glsl-versions _)))
                         funcs))
        funcs)))

(defmethod add-top-level-lisp-function-decl (name in-args uniforms code
                                  &optional valid-glsl-versions)
  (quick-check-of-arg-type-validity name (append in-args uniforms))
  (labels ((get-types (x) (mapcar #'second (in-args x))))
    (let* ((func (make-instance 'top-level-lisp-function-decl
                                :name name
                                :in-args in-args
                                :uniforms uniforms
                                :code code
                                :glsl-versions valid-glsl-versions))
           (funcs (cons func (get-top-level-lisp-function-decl-by-name name nil))))
      (setf (gethash name *top-level-lisp-function-decls*)
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
        (error 'top-level-lisp-function-decl-invalid-in-arg-types
               :name name
               :args unknown)))
    args))

(defmethod delete-top-level-lisp-function-decl (name in-args-types)
  (labels ((get-types (x) (mapcar #'second (in-args x))))
    (let* ((funcs (get-top-level-lisp-function-decl-by-name name nil)))
      (setf (gethash name *top-level-lisp-function-decls*)
            (remove in-args-types funcs :key #'get-types :test #'equal))
      t)))

(defmethod func-need-arguments-compiledp ((func top-level-lisp-function-decl))
  t)

(defmethod v-special-functionp ((func top-level-lisp-function-decl))
  nil)

(defmethod v-argument-spec ((func top-level-lisp-function-decl))
  (mapcar λ(type-spec->type (second _)) (in-args func)))

(defun format-top-level-lisp-function-decl-for-error (func)
  `(,(name func) ,@(mapcar #'second (in-args func))
     ,@(when (uniforms func)
             (mapcar #'second (cons '&uniforms (uniforms func))))))

(defun top-level-lisp-function-decl-p (f)
  (typep f 'top-level-lisp-function-decl))
