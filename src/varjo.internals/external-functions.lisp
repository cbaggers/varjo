(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defvar *external-functions*
  (make-hash-table :test #'eq))

(defmethod get-external-function-by-name (name env)
  (let ((funcs (gethash name *external-functions*)))
    (if env
        (let ((v (get-version-from-context env)))
          (remove-if-not λ(or (null (glsl-versions _))
                              (member v (glsl-versions _)))
                         funcs))
        funcs)))

(defmethod get-external-function-by-literal ((func-name list))
  ;;
  (destructuring-bind (name &rest arg-types) func-name
    (assert (not (eq name 'declare)) ()
            'treating-declare-as-func :decl func-name)
    (assert (not (find-if #'&rest-p arg-types))
            () 'cannot-take-reference-to-&rest-func :func-name func-name)
    (let ((arg-types (mapcar (lambda (x) (type-spec->type x))
                             arg-types))
          (binding (make-function-set
                    (get-external-function-by-name name nil))))
      ;;
      (etypecase binding
        ;; When we have types we should try to match exactly
        (v-function-set
         (if arg-types
             (or (%post-process-found-literal-func
                  (find-if λ(exact-match-function-to-types arg-types _)
                           (functions binding))
                  arg-types)
                 (error 'could-not-find-function :name func-name))
             binding))
        ;;
        (null (error 'could-not-find-function :name func-name))))))

(defmethod add-external-function (name in-args uniforms code
                                  &optional valid-glsl-versions)
  (quick-check-of-arg-type-validity name (append in-args uniforms))
  (multiple-value-bind (body decls doc-string)
      (extract-declares-and-doc-string code code)
    (labels ((get-types (x) (mapcar #'second (in-args x))))
      (let* ((func (make-instance 'external-function
                                  :name name
                                  :in-args in-args
                                  :&rest-pos nil
                                  :uniforms uniforms
                                  :code (append decls body)
                                  :doc-string doc-string
                                  :glsl-versions valid-glsl-versions))
             (funcs (cons func (get-external-function-by-name name nil))))
        (setf (gethash name *external-functions*)
              (remove-duplicates funcs :key #'get-types :test #'equal
                                 :from-end t))
        func))))

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
  (if (eq in-args-types :all)
      (setf (gethash name *external-functions*) nil)
      (labels ((get-types (x) (mapcar #'second (in-args x))))
        (let* ((funcs (get-external-function-by-name name nil)))
          (setf (gethash name *external-functions*)
                (remove in-args-types funcs :key #'get-types :test #'equal))
          t))))

(defmethod func-need-arguments-compiledp ((func external-function))
  t)

(defmethod v-special-functionp ((func external-function))
  nil)

(defmethod v-argument-spec ((func external-function))
  (mapcar λ(type-spec->type (second _)) (in-args func)))

(defun format-external-func-for-error (func)
  `(,(name func) ,@(mapcar #'second (in-args func))
     ,@(when (uniforms func)
             (mapcar #'second (cons '&uniforms (uniforms func))))))

(defun external-function-p (f)
  (typep f 'external-function))
