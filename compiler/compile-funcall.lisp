(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-list-form (code env)
  (if (eq (first code) 'funcall)
      (compile-funcall-form code env)
      (compile-call-form code env)))

(defun compile-call-form (code env)
  (dbind (name . args-code) code
    (assert (not (keywordp name)) ()
            'keyword-in-function-position :form code)
    (assert (not (eq name 'declare)) ()
            'calling-declare-as-func :decl code)
    (let ((binding (get-form-binding name env)))
      (etypecase binding
        (v-regular-macro (expand-macro binding args-code env))
        (v-function-set (compile-call-with-set-of-functions
                         binding args-code env name code))
        (null (error 'could-not-find-function :name name))))))

(defun expand-macro (macro args-code env)
  (let ((public-env (make-instance 'macro-expansion-environment
                                   :macro-obj macro
                                   :env env)))
    (compile-form (funcall (v-macro-function macro)
                           args-code
                           public-env)
                  env)))

(defun compile-funcall-form (code env)
  (dbind (fc func-form . arg-forms) code
    (assert (eq fc 'funcall))
    (vbind (func-code-obj f-env) (compile-form func-form env)
      (declare (ignore f-env)) ;;{TODO} is this ok?
      (let ((func (get-actual-function func-code-obj code)))
        (vbind (obj final-env)
            (compile-call-with-set-of-functions func arg-forms env nil code)
          (let* ((ast (node-tree obj))
                 (funcall-ast (ast-node! :funcall
                                         (cons (node-tree func-code-obj)
                                               (ast-args ast))
                                         (ast-return-type ast)
                                         env
                                         (ast-ending-env ast)))
                 (to-block (append
                            (to-block func-code-obj)
                            (list (current-line (end-line func-code-obj)))
                            (to-block obj))))
            (assert (eq final-env (ast-ending-env ast)))
            (merge-compiled (list func-code-obj obj)
                            :type-set (make-type-set (primary-type obj))
                            :current-line (current-line obj)
                            :to-block (remove nil to-block)
                            :node-tree funcall-ast)))))))

(defun compile-call-with-set-of-functions (func-set args-code env
                                           &optional name code)
  (let ((func-name (or name (%func-name-from-set func-set))))
    (dbind (func args) (find-function-in-set-for-args
                        func-set args-code env func-name)
      (typecase func
        (v-function (compile-function-call func-name func args env))
        (external-function (compile-external-function-call func args env))
        (v-error (if (v-payload func)
                     (error (v-payload func))
                     (error 'cannot-compile
                            :code (or code
                                      `(funcall ,func-set ,@args-code)))))
        (t (error 'problem-with-the-compiler :target func))))))

(defun get-actual-function (func-code-obj code)
  (let ((primary-type (primary-type func-code-obj)))
    (if (typep primary-type 'v-any-one-of)
        (make-function-set (mapcar #'ctv (v-types primary-type)))
        (let* ((func (ctv primary-type)))
          (restart-case (typecase func
                          (v-function func)
                          (v-function-set func)
                          (t (error 'cannot-establish-exact-function
                                    :funcall-form code)))
            ;;
            (allow-call-function-signature ()
              (values
               (make-dummy-function-from-type (primary-type func-code-obj))
               t)))))))

(defun find-and-expand-compiler-macro (func args env)
  (unless (v-special-functionp func)
    (let ((macro (find-compiler-macro-for-func func env)))
      (when macro
        (let ((public-env (make-instance 'compiler-macro-expansion-environment
                                         :macro-obj macro
                                         :args args
                                         :env env)))
          (funcall (v-macro-function macro)
                   (mapcar #'ast->code args)
                   public-env))))))

(defun compile-function-call (func-name func args env)
  (vbind (expansion use-expansion)
      (find-and-expand-compiler-macro func args env)
    (if use-expansion
        (compile-form expansion env)
        (vbind (code-obj new-env)
            (cond
              ;; special funcs
              ((v-special-functionp func) (compile-special-function func args
                                                                    env))
              ;; funcs with multiple return values
              ((and (vectorp (v-return-spec func))
                    (> (length (v-return-spec func)) 1))
               (compile-multi-return-function-call func-name func args env))

              ;; funcs taking unrepresentable values as arguments
              ((and (typep func 'v-user-function)
                    (some λ(typep _ 'v-unrepresentable-value)
                          (v-argument-spec func)))
               (compile-function-taking-unreps func-name func args env))

              ;; all the other funcs :)
              (t (compile-regular-function-call func-name func args env)))
          (assert new-env)
          (values code-obj new-env)))))

(defun compile-function-taking-unreps (func-name func args env)
  (assert (v-code func))
  (labels ((unrep-p (x) (typep (primary-type x) 'v-unrepresentable-value)))
    (dbind (args-code body-code) (v-code func)
      (dbind (trimmed-args hard-coded)
          (loop :for arg-code :in args-code :for arg :in args
             :if (unrep-p arg) :collect (list (first arg-code) arg) :into h
             :else :collect arg-code :into a
             :finally (return (list a h)))
        ;; this is a hack but it'll do for now. It just lets our func use
        ;; the vars that were captured, if we are still in scope
        (let* ((captured (captured-vars func))
               (captured (remove-if-not λ(descendant-env-p env (origin-env _))
                                        captured))
               (allowed (append (mapcar #'first hard-coded)
                                (mapcar #'name captured))))
          (compile-form
           `(let ,hard-coded
              (labels-no-implicit ((,func-name ,trimmed-args ,@body-code))
                                  ,allowed
                                  (,func-name ,@(remove-if #'unrep-p args))))
           env))))))

(defun compile-external-func-returning-ref (func func-name-form env)
  ;; Here we are going to make use of the fact that a external function
  ;; is not allowed to mutate the environment it was called from.
  ;; We are going to grab the base environment and compile the labels form
  ;; there. We can then extract the signatures we need from this and add them
  ;; to the final source. The deduplication will be achieved by the fact that
  ;; we will use the external-function object as a key to a hashtable in the
  ;; base-env which will cache the results of these compiled external functions.
  ;;
  (let* ((base-env (get-base-env env))
         (compiled-func (or (compiled-functions base-env func)
                            (build-external-function func base-env))))
    (setf (compiled-functions base-env func) compiled-func)
    ;;
    (let* ((func (function-obj compiled-func))
           (flow-id (flow-id!))
           (type (set-flow-id (v-type-of func) flow-id))
           (type-set (make-type-set type)))
      (when (implicit-args func)
        (error 'closures-not-supported :func func-name-form))
      (values
       (make-compiled :type-set type-set
                      :current-line nil
                      :used-types (list type)
                      :node-tree (ast-node! 'function (list func-name-form)
                                            type-set nil nil))
       env))))

(defun compile-external-function-call (func args env)
  ;; Here we are going to make use of the fact that a external function
  ;; is not allowed to mutate the environment it was called from.
  ;; We are going to grab the base environment and compile the labels form
  ;; there. We can then extract the signatures we need from this and add them
  ;; to the final source. The deduplication will be achieved by the fact that
  ;; we will use the external-function object as a key to a hashtable in the
  ;; base-env which will cache the results of these compiled external
  ;; functions.
  (let* ((base-env (get-base-env env))
         (compiled-func (or (compiled-functions base-env func)
                            (build-external-function func base-env))))
    (setf (compiled-functions base-env func) compiled-func)
    (compile-function-call (name func)
                           (function-obj compiled-func)
                           args
                           env)))

(defun calc-place-tree (func args)
  (when (v-place-function-p func)
    (let ((i (v-place-index func)))
      (cons (list func (elt args i)) (place-tree (elt args i))))))

(defun compile-regular-function-call (func-name func args env)
  (let* ((c-line (gen-function-string func args))
         (flow-ids (calc-function-return-ids-given-args func args))
         ;; This is one of the few cases where we want to set a flow id
         ;; regardless of the current state
         (type (or (resolve-func-type func args)
                   (error 'unable-to-resolve-func-type
                          :func-name func-name :args args)))
         (type-set (if (v-typep type :void)
                       (make-type-set)
                       (apply #'make-type-set
                              (cons (if (flow-ids type)
                                        (replace-flow-id type flow-ids)
                                        (set-flow-id type flow-ids))
                                    (handle-regular-function-mvals args))))))
    (values (merge-compiled
             args
             :type-set type-set
             :current-line c-line
             :pure (pure-p func)
             :emit-set (emit-set func)
             :stemcells (mapcat #'stemcells args)
             :place-tree (calc-place-tree func args)
             :node-tree (ast-node! func-name
                                   (mapcar #'node-tree args)
                                   type-set
                                   env
                                   env))
            env)))

(defun handle-regular-function-mvals (args)
  ;; ok so by getting here a few things are true:
  ;; - we were compiling a regular function which usually stops 'values
  ;;   from propagating up the stack
  ;; - someone is subverting this by using the 'value-safe special-form.
  ;;   let's hope they know what they are doing :p
  ;; - we now have to work out the mvals for a regular function.
  (labels ((multi-vals-p (x)
             (> (length (type-set x)) 1)))
    (let ((count (count-if #'multi-vals-p args))
          (n (position-if #'multi-vals-p args)))
      (cond ((> count 1) (error 'values-safe-wasnt-safe :args args))
            ((= count 1) (rest (coerce (type-set (nth n args)) 'list)))
            (t nil)))))

;;----------------------------------------------------------------------

(defun %calc-flow-id-given-args (in-arg-flow-ids return-flow-id arg-code-objs
                                 &optional (multi-return-position 0))
  (let ((p (positions-if (lambda (x) (id~= return-flow-id x))
                         in-arg-flow-ids)))
    (if p
        (reduce #'flow-id!
                (mapcar (lambda (i) (flow-ids (elt arg-code-objs i)))
                        p))
        (flow-id+meta! :return-pos multi-return-position))))

(defun calc-function-return-ids-given-args (func arg-code-objs)
  ;; {TODO} (warn "calc-function-return-ids-given-args should be merged with resolve-func-type")
  (let ((rspec (v-return-spec func)))
    (assert (or (functionp rspec)
                (typep rspec 'return-type-generator)
                (<= (length rspec) 1))))
  (unless (function-return-spec-doesnt-need-flow-ids (v-return-spec func))
    (%calc-flow-id-given-args (in-arg-flow-ids func)
                              (flow-ids func)
                              arg-code-objs)))

(defun calc-mfunction-return-ids-given-args (func func-name arg-code-objs)
  (let ((all-return-types (type-set-to-type-list (v-return-spec func))))
    (let ((flow-ids (map 'list #'flow-ids all-return-types)))
      (if (some #'type-doesnt-need-flow-id all-return-types)
          (error 'invalid-flow-id-multi-return :func-name func-name
                 :return-type all-return-types)
          (mapcar #'(lambda (x i)
                      (%calc-flow-id-given-args
                       (in-arg-flow-ids func) x arg-code-objs i))
                  flow-ids
                  (iota (length flow-ids)))))))

(defun compile-multi-return-function-call (func-name func args env)
  (let* ((flow-ids (calc-mfunction-return-ids-given-args func func-name args))
         (type (replace-flow-id (resolve-func-type func args)
                                (first flow-ids))))
    (assert (not (v-typep type :void)))
    (unless type (error 'unable-to-resolve-func-type :func-name func-name
                        :args args))
    (let* ((has-base (not (null (v-multi-val-base env))))
           (m-r-base (or (v-multi-val-base env)
                         (lisp-name->glsl-name 'nc env)))
           (return-set (v-return-spec func))
           (mvals (rest (coerce return-set 'list)))
           (mval-count (length mvals))
           (start-index 1)
           (m-r-names (loop :for i :from start-index
                         :below (+ start-index mval-count) :collect
                         (postfix-glsl-index m-r-base i))))
      (let* ((bindings (loop :for mval :in mvals :collect
                          `((,(gensym "NC")
                              ,(type->type-spec (v-type-of mval))))))

             (o (merge-compiled
                 args
                 :type-set (apply #'make-type-set
                                  (cons type
                                        (mapcar (lambda (mval glsl-name fid)
                                                  (make-typed-glsl-name
                                                   (replace-flow-id (v-type-of mval) fid)
                                                   glsl-name))
                                                mvals
                                                m-r-names
                                                (rest flow-ids))))
                 :current-line (gen-function-string func args m-r-names)
                 :stemcells (mapcat #'stemcells args)
                 :pure (pure-p func)
                 :emit-set (emit-set func)
                 :place-tree (calc-place-tree func args)
                 :node-tree :ignored))
             (final
              ;; when has-base is true then a return or mvbind has already
              ;; written the lets for the vars
              (if has-base
                  o
                  (merge-progn
                   (with-fresh-env-scope (fresh-env env)
                     (env-> (p-env fresh-env)
                       (merge-multi-env-progn
                        (%mapcar-multi-env-progn
                         (lambda (env binding gname)
                           (with-v-let-spec binding
                             (compile-let name type-spec nil env gname)))
                         p-env bindings m-r-names))
                       (compile-form o p-env)))
                   env env))))
        (values (copy-compiled final
                               :node-tree (ast-node! func-name
                                                     (mapcar #'node-tree args)
                                                     (make-type-set type)
                                                     env env))
                env)))))

;;----------------------------------------------------------------------

(defun end-line (obj &optional force)
  (assert (not force))
  (when obj
    (if (null (current-line obj))
        obj
        (copy-compiled obj :current-line (end-line-str (current-line obj))
                       :place-tree nil))))

(defun end-line-str (str)
  (format nil "~a;" str))
