(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun compile-special-function (func args env)
  (multiple-value-bind (code-obj new-env)
      (apply (v-return-spec func) env func args)
    (values code-obj new-env)))

;;----------------------------------------------------------------------

(defun compile-progn (body env)
  (if (= 1 (length body))
      (vbind (obj new-env) (compile-form (first body) env)
        (values (list obj) new-env))
      (let* ((mvb (v-multi-val-base env))
             (env (fresh-environment env :multi-val-base nil))
             (body-objs
              (append
               (loop :for code :in (butlast body)
                  :collect (vbind (code-obj new-env) (compile-form code env)
                             (when new-env (setf env new-env))
                             code-obj))
               (vbind (code-obj new-env)
                   (compile-form (last1 body)
                                 (fresh-environment env :multi-val-base mvb))
                 (when new-env (setf env new-env))
                 (list code-obj)))))
        (values body-objs env))))

(defun compile-forms-propagating-env-returning-list-of-compiled
    (func env list &rest more-lists)
  "Compile each form passing the env from the previous into the next.
   return the compiled code objects as a list along with the final env"
  (values (apply #'mapcar
                 (lambda (&rest args)
                   (vbind (code-obj new-env) (apply func (cons env args))
                     (when new-env (setf env new-env))
                     code-obj))
                 (cons list more-lists))
          env))

(defun %merge-progn (code-objs starting-env final-env)
  (if (= 1 (length code-objs))
      (first code-objs)
      (let* ((last-obj (last1 (remove nil code-objs)))
             (type-set (type-set (last1 code-objs))))
        (merge-compiled code-objs
                        :type-set type-set
                        :current-line (current-line last-obj)
                        :to-block (merge-lines-into-block-list code-objs)
                        :node-tree (ast-node! 'progn
                                              (mapcar #'node-tree code-objs)
                                              type-set
                                              starting-env final-env)))))

(defmacro merge-progn (code-objs starting-env &optional final-env)
  (let ((co (gensym "code-objs"))
        (pe (gensym "potential-env"))
        (se (gensym "starting-env"))
        (fe (gensym "final-env")))
    `(vbind (,co ,pe) ,code-objs
       (let* ((,se ,starting-env)
              (,fe ,(if final-env
                        `(or ,final-env ,pe ,se)
                        `(or ,pe ,se))))
         (values (%merge-progn ,co ,se ,fe)
                 ,fe)))))


;;----------------------------------------------------------------------

(defun compile-forms-not-propagating-env-returning-list-of-compiled
    (func env list &rest more-lists)
  "This compiled each form one after the other (just like progn) however,
   unlike progn, each form is evaluated with the original environment this
   means that bindings in one won't be visable in another.
   Finally the resulting environement is merged.

   This gives us the behaviour from the binding expressions portion of
   let forms"
  (when list
    (let* ((e (apply #'mapcar
                     (lambda (&rest args)
                       (vlist (apply func (cons env args))))
                     (cons list more-lists)))
           (code-objs (mapcar #'first e))
           (env-objs (mapcar #'second e))
           (merged-env (reduce #'merge-env env-objs)))
      (values code-objs merged-env))))

(defun %merge-multi-env-progn (code-objs)
  (let ((type-set (make-type-set)))
    (merge-compiled
     code-objs
     :type-set type-set
     :current-line nil
     :to-block (append (mappend #'to-block code-objs)
                       (mapcar (lambda (_) (current-line (end-line _)))
                               code-objs))
     :node-tree :ignored)))

(defmacro merge-multi-env-progn (code-objs)
  (let ((co (gensym "code-objs"))
        (fe (gensym "final-env")))
    `(vbind (,co ,fe) ,code-objs
       (values (%merge-multi-env-progn ,co)
               ,fe))))

;;----------------------------------------------------------------------

(defun typify-code (code-obj &optional new-value)
  (let* ((prefixed-line (prefix-type-declaration code-obj))
         (current-line
          (if new-value
              (%gen-assignment-string prefixed-line (current-line new-value))
              prefixed-line))
         (to-block (when new-value
                     (to-block new-value)))
         (type (if new-value
                   (progn
                     (assert (flow-ids new-value))
                     (replace-flow-id (primary-type code-obj)
                                      (flow-ids new-value)))
                   (primary-type code-obj)))
         (type-set (make-type-set type)))
    (copy-compiled code-obj
                   :type-set type-set
                   :current-line current-line
                   :to-block to-block
                   :node-tree :ignored
                   :place-tree nil)))

;;----------------------------------------------------------------------

;; {TODO} use boundp to error on shadowing specials. Explain that varjo
;;        cannot provide dynamic scope so this should be avoided
(defun compile-let (name type-spec value-form env
                    &optional glsl-name (assume-bound t))
  ;;
  (let* ((value-obj (when value-form (compile-form value-form env)))
         (glsl-name (or glsl-name (lisp-name->glsl-name name env)))
         (type-obj (when type-spec
                     (type-spec->type type-spec
                                      (if value-form
                                          (flow-ids value-obj)
                                          (flow-id!))))))
    (%validate-var-types name type-obj value-obj)
    (let* ((let-obj
            (cond
              ;; handle unrepresentable values
              ((and value-obj (ephemeral-p (primary-type value-obj)))
               value-obj)
              ;;
              (value-obj
               (typify-code
                (make-compiled
                 :type-set (make-type-set
                            (or type-obj (primary-type value-obj)))
                 :current-line glsl-name
                 :node-tree :ignored)
                value-obj))
              ;;
              (t (typify-code
                  (make-compiled :type-set (make-type-set type-obj)
                                 :current-line glsl-name
                                 :node-tree :ignored)))))
           (to-block
            (cons-end (current-line (end-line let-obj))
                      (to-block let-obj))))
      (values
       (copy-compiled let-obj
                      :type-set (make-type-set)
                      :current-line nil
                      :to-block to-block
                      :place-tree nil
                      :node-tree (if value-form
                                     (node-tree value-obj)
                                     :ignored)
                      :stemcells (append (and let-obj (stemcells let-obj))
                                         (and value-obj (stemcells value-obj)))
                      :pure (if value-obj (pure-p value-obj) t))
       (add-symbol-binding
        name
        (if (and (not value-obj) (not assume-bound))
            (v-make-uninitialized (or type-obj (primary-type value-obj)) env
                                  :glsl-name glsl-name)
            (v-make-value (or type-obj (primary-type value-obj))
                          env
                          :glsl-name glsl-name))
        env)))))

(defun %validate-var-types (var-name type code-obj)
  (when (and code-obj (typep (primary-type code-obj) 'v-stemcell))
    (error "Code not ascertain the type of the stemcell used in the let form:~%(~a ~a)"
           (string-downcase var-name) (current-line code-obj)))
  (when (and (null type) (null code-obj))
    (error "Could not establish the type of the variable: ~s" var-name))
  (when (and code-obj type (not (v-type-eq (primary-type code-obj) type)))
    (error "Type specified does not match the type of the form~%~s~%~s"
           (primary-type code-obj) type))
  t)

;;----------------------------------------------------------------------

(defmacro env-> ((env-var env) &body compiling-forms)
  "Kinda like varjo progn in that it accumulates the env and
   returns the results of all the forms and the final env.
   However it DOES NOT make a fresh environment to compile the forms in.
   It expects that each form returns a result and optionally an env"
  (let ((objs (gensym "results"))
        (obj (gensym "result"))
        (new-env (gensym "new-env")))
    `(let ((,env-var ,env)
           (,objs nil))
       (declare (ignorable ,env-var))
       ,(reduce (lambda (_ _1)
                  `(vbind (,obj ,new-env) ,_1
                     (let ((,env-var (or ,new-env ,env-var)))
                       (declare (ignorable ,env-var))
                       (unless ,obj ;; {TODO} proper error
                         (assert (null ,new-env) ()
                                 "env-> error: if the code-object returned by a step is null then the env must also be null"))
                       (push ,obj ,objs)
                       ,_)))
                (cons `(values (reverse ,objs) ,env-var)
                      (reverse compiling-forms))))))



(defmacro with-v-let-spec (form &body body)
  (let ((var-spec (gensym "var-spec"))
        (qual (gensym "qualifiers"))
        (full-spec (gensym "form")))
    `(let* ((,full-spec ,form)
            (,var-spec (listify (first ,full-spec)))
            (value-form (second ,full-spec))
            (value-form (if (and (> (length ,full-spec) 1) (null value-form))
                            `(the :bool nil)
                            value-form)))
       (declare (ignorable value-form))
       (destructuring-bind (name &optional type-spec ,qual) ,var-spec
         (declare (ignore ,qual))
         ,@body))))

;;----------------------------------------------------------------------
