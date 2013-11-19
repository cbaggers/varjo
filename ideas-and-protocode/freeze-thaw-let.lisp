;;[TODO] does this put blocks in the correct place? e.g. let in assign
(v-defun %add-var (var-spec &optional (include-declarations t))
  :special
  :args-valid t
  :return
  (let* ((form-specified (> (length var-spec) 2))
         (name (first var-spec))
         (type (when (second name-&-type) (type-spec->type (second var-spec))))
         (form (when form-specified (varjo->glsl (third var-spec) env))))
    (cond
      ((and (null type) (null form)) (error unknown-variable-type name))
      ((and form type (not (v-type-eq (code-type form) type)))
       (error 'var-type-mismatch type form))
      (t (let* ((type (or type (code-type form)))
                (assign-obj (varjo->glsl
                             (if include-declarations
                                 `(%typify (setf (%make-var ,name ,type) ,form))
                                 `(%typify (%make-var ,name ,type)))
                             env)))
           (add-var (free-name name env) (v-make-value type) env t)
           (values (if include-type-declarations
                       (merge-obs assign-obj :type (make-instance 'v-none)
                                  :current-line nil
                                  :to-block (append (to-block assign-obj)
                                                    (end-line assign-obj))
                                  :to-top (mapcan #'to-top decl-objs))
                       (make-instance 'code :type 'v-none))
                   env))))))

;; check for duplicates
(v-defmacro let (bindings &body body)
  `(%new-env-block
    (%freeze-environment)
    ,@(loop for b in bindings collect `(%add-var ,b))
    (%thaw-environment)
    ,@body))
