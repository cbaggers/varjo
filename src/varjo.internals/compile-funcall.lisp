(in-package :varjo.internals)
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
        (assert (not (v-special-functionp func)) ()
                'funcall-of-special-operator :code func-form)
        (vbind (obj final-env)
            (compile-call-with-set-of-functions func arg-forms env nil code)
          (let* ((to-block (join-glsl-chunks
                             (list (glsl-chunk-from-compiled func-code-obj)
                                   (to-block obj)))))
            (merge-compiled (list func-code-obj obj)
                            :type-set (type-set obj)
                            :current-line (current-line obj)
                            :to-block to-block)))))))

(defun compile-call-with-set-of-functions (func-set args-code env
                                           &optional name code)
  (let ((func-name name)) ;; ← Its ok if null, other code can search for the
    ;;                         name from the func-set if needed
    (dbind (func args) (find-function-in-set-for-args
                        func-set args-code env func-name code)
      ;; We take the safe assumption that no non-user-defined function
      ;; will ever take a user defined struct as an argument. This is
      ;; important due to how ephemerals work
      (typecase func
        (v-function (compile-call-with-single-function func args args-code env))
        (external-function (compile-call-with-single-function func args args-code env))
        (v-error (if (v-payload func)
                     (error (v-payload func))
                     (error 'cannot-compile
                            :code (or code
                                      `(funcall ,func-set ,@args-code)))))
        (t (error 'problem-with-the-compiler :target func))))))

(defun compile-call-with-single-function (func compiled-args args-code env)
  (check-type func (or v-function external-function))
  ;;
  ;; We take the safe assumption that no 'NON user-defined' function
  ;; will ever take a user defined struct as an argument. This is
  ;; important due to how ephemerals work.
  ;;
  (typecase func
    (trait-function
     (with-slots (trait) func
       (let* ((arg-type (primary-type (first compiled-args)))
              (impl (get-trait-implementation trait arg-type))
              (impl-func (second
                          (find (name func)
                                (slot-value impl 'function-signatures)
                                :key #'first
                                :test #'string=))))
         (compile-call-with-single-function impl-func compiled-args args-code
                                            env))))
    (v-function
     (vbind (new-obj new-env expanded-into-new-form)
         (compile-function-call func compiled-args args-code env)
       (declare (ignore expanded-into-new-form))
       (values new-obj new-env)))
    (external-function (compile-external-function-call func compiled-args args-code env))
    (t (error 'problem-with-the-compiler :target func))))

(defvar *allow-call-function-signature* nil)

(defmacro with-unknown-first-class-functions-allowed (&body body)
  `(let ((*allow-call-function-signature* t))
     ,@body))

(defun get-actual-function (func-code-obj code)
  (let ((primary-type (primary-type func-code-obj)))
    (if (typep primary-type 'v-any-one-of)
        (make-function-set (mapcar #'ctv (v-types primary-type)))
        (let* ((func (ctv primary-type)))
          (typecase func
            (v-function func)
            (v-function-set func)
            (t (if *allow-call-function-signature*
                   (values
                    (make-dummy-function-from-type
                     (primary-type func-code-obj)
                     code)
                    t)
                   (error 'cannot-establish-exact-function
                          :funcall-form code))))))))

(defun find-and-expand-compiler-macro (func args args-code env)
  (unless (v-special-functionp func)
    (let ((macro (find-compiler-macro-for-func func)))
      (when macro
        (let ((public-env (make-instance 'compiler-macro-expansion-environment
                                         :macro-obj macro
                                         :args args
                                         :env env)))
          (funcall (v-macro-function macro)
                   args-code
                   public-env))))))

(defun compile-function-call (func args args-code env)
  "Returns 3 values: the new compiled object, the new environment & a boolean
   this shows whether the function was use or a compiler macro expansion
   (t means the compiler-macro was used)"
  (let ((call-form (cons (name func) args-code)))
    (vbind (expansion use-expansion)
        (find-and-expand-compiler-macro func args args-code env)
      (if use-expansion
          (vbind (new-obj new-env) (compile-form expansion env)
            (values new-obj new-env t))
          (vbind (code-obj new-env expanded-into-new-form)
              (cond
                ;; special funcs
                ((v-special-functionp func)
                 (compile-special-function func args env))

                ;; funcs taking traits as arguments
                ;; traits being before unreps matters here.
                ;; want to hit 'final' types before unrep transforms
                ((and (typep func 'v-user-function)
                      (some λ(typep _ 'v-trait) (v-argument-spec func)))
                 (compile-function-taking-traits func args env))

                ;; funcs taking unrepresentable values as arguments
                ((and (typep func 'v-user-function)
                      (or (some λ(typep _ 'v-unrepresentable-value)
                                (v-argument-spec func))
                          (some λ(typep (primary-type _) 'v-block-struct) args)))
                 (compile-function-taking-unreps func args args-code env))

                ;; funcs with multiple return values
                ((multi-return-function-p func)
                 (compile-multi-return-function-call func args env call-form))

                ;; all the other funcs :)
                (t (compile-regular-function-call func
                                                  args
                                                  env
                                                  call-form)))
            (assert new-env)
            (values code-obj new-env expanded-into-new-form))))))


;; TODO handle traits & unreps in one function

(defun compile-function-taking-traits (func args env)
  (assert (v-code func))
  ;; if we knew that the function was an external function and that the types
  ;; that we were passing for the traits were the same, we could dedup.
  (labels ((trait-p (x) (typep x 'v-trait)))
    (dbind (params body-code) (v-code func)
      (let ((new-params
             (loop :for param :in params
                :for arg :in args
                :for param-type := (type-spec->type (second param))
                :if (trait-p param-type)
                :collect `(,(first param)
                            ,(type->type-spec (primary-type arg))
                            ,@(cddr param))
                :else
                :collect param)))
        ;; this is a hack but it'll do for now. It just lets our func use
        ;; the vars that were captured, if we are still in scope
        (let* ((captured (captured-vars func))
               (captured (remove-if-not λ(descendant-env-p env (origin-env _))
                                        captured))
               (allowed (mapcar #'name captured))
               (func-name (name func)))
          (compile-form
           `(vari.cl:labels-no-implicit
             ((,func-name ,new-params ,@body-code))
             ,(derived-from func)
             ,allowed
             (,func-name ,@args))
           env))))))

(defun find-derived-call (func args env)
  (with-slots (derived-from) func
    (when derived-from
      (let ((compiled-result (compiled-functions env derived-from)))
        (when compiled-result
          (loop
             :for (func . types) :in (calls compiled-result)
             :when (and (= (length args) (length types))
                        (every #'v-type-eq
                               (mapcar #'primary-type args)
                               types))
             :return (identity func)))))))

(defun compile-function-taking-unreps (func args args-code env)
  (assert (v-code func))
  (labels ((unrep-type-p (x)
             (or (typep x 'v-unrepresentable-value)
                 (typep x 'v-block-struct)))
           (unrep-arg-p (x)
             (unrep-type-p (primary-type x))))
    (dbind (params body-code) (v-code func)
      (dbind (trimmed-args hard-coded final-args)
          (loop :for param :in params
             :for arg :in args
             :for param-type := (type-spec->type (second param))
             :if (or (unrep-type-p param-type) (unrep-arg-p arg))
             :collect (list (first param) arg) :into h
             :else
             :collect param :into a :and :collect arg :into f
             :finally (return (list a h f)))
        (assert hard-coded () 'no-args-remove-in-unrep-inlining
                :func func
                :args args
                :args-code args-code)
        (let ((derived-call (find-derived-call func final-args env)))
          (if derived-call
              (compile-call-with-single-function derived-call
                                                 final-args
                                                 args-code
                                                 env)
              ;; this is a hack but it'll do for now. It just lets our func use
              ;; the vars that were captured, if we are still in scope
              (let* ((captured (captured-vars func))
                     (captured (remove-if-not λ(descendant-env-p env (origin-env _))
                                              captured))
                     (allowed (append (mapcar #'first hard-coded)
                                      (mapcar #'name captured)))
                     (func-name (name func)))
                (vbind (new-obj new-env expanded-into-new-form)
                    (compile-form
                     `(let ,hard-coded
                        (vari.cl:labels-no-implicit
                         ((,func-name ,trimmed-args ,@body-code))
                         ,(derived-from func)
                         ,allowed
                         (,func-name ,@final-args)))
                     env)
                  (declare (ignore expanded-into-new-form))
                  (values new-obj new-env t)))))))))

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
                            (build-external-function func env base-env))))
    (setf (compiled-functions base-env func) compiled-func)
    ;;
    (let* ((func (function-obj compiled-func))
           (flow-id (flow-id!))
           (type (set-flow-id (v-type-of func) flow-id))
           (type-set (make-type-set type)))
      (when (implicit-args func)
        (let ((details (extract-details-from-problematic-closures (list func))))
          (error 'closures-not-supported :func func-name-form
                 :details details)))
      (values
       (make-compiled :type-set type-set
                      :current-line nil
                      :used-types (list type))
       env))))

(defun extract-details-from-problematic-closures (closures)
  (let ((closures (remove-duplicates closures)))
    (loop :for c :in closures :collect
       (let ((captured (captured-vars c))
             (implicit (implicit-args c)))
         (dbind (args body) (v-code c)
           (list `(,(name c) ,args ,@body)
                 (mapcar #'name captured)
                 (mapcar #'glsl-name implicit)))))))

(defun compile-external-function-call (func args args-code env)
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
                            (setf (compiled-functions base-env func)
                                  (build-external-function func env base-env)))))
    (if (and (or (declaimed-inline func)
                 ;; {TODO} declared inline locally. Cant do this properly until
                 ;;        metadata can take non &key args
                 )
             (inline-candidate compiled-func))
        (inline-external-function-call compiled-func args-code env)
        (vbind (new-obj new-env expanded-into-new-form)
            (compile-function-call (function-obj compiled-func) args args-code env)
          (unless expanded-into-new-form
            ;; track the number of times the function was used
            (incf (call-count compiled-func)))
          (values new-obj new-env)))))


(defun inline-external-function-call (compiled-func args-code env)
  (dbind (args body) (v-code (function-obj compiled-func))
    (compile-form
     `(let ,(mapcar #'list
                    (mapcar #'first args)
                    args-code)
        ,@body)
     env)))

(defun calc-place-tree (func args)
  (when (v-place-function-p func)
    (let ((i (v-place-index func)))
      (cons (list func (elt args i)) (place-tree (elt args i))))))

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

(defun compile-regular-function-call (func args env call-form)
  (let ((type-set (make-type-set (resolve-func-set func args)
                                 (when (v-multi-val-safe env)
                                   (handle-regular-function-mvals args))))
        (emit-set (emit-set func)))
    (with-slots (derived-from) func
      (when derived-from
        (let ((compiled-result (compiled-functions env derived-from)))
          (when compiled-result
            (push (cons func (mapcar #'primary-type args))
                  (calls compiled-result))))))
    (when (user-function-p func)
      (incf (call-count (compiled-result func))))
    (values (merge-compiled
             args
             :type-set type-set
             :current-line (gen-function-call-string func args)
             :pure (pure-p func)
             :emit-set emit-set
             :stemcells (mappend #'stemcells args)
             :place-tree (calc-place-tree func args)
             :used-types (append
                          (mappend #'used-types args)
                          (coerce type-set 'list)
                          (coerce emit-set 'list)))
            (make-env-with-place-modification-for-funcall func
                                                          args
                                                          env
                                                          call-form))))

(defun make-env-with-place-modification-for-funcall (func
                                                     arg-objs
                                                     env
                                                     call-form)
  (let ((modified-env env)
        (arg-types (remove-if #'symbolp (v-argument-spec func))))
    (loop
       :for arg-type :in arg-types
       :for obj :in arg-objs
       :do (when (find :in/out (qualifiers arg-type) :test #'qualifier=)
             (setf modified-env
                   (make-env-with-place-modification obj
                                                     (flow-id!)
                                                     modified-env
                                                     call-form))))
    modified-env))

;;----------------------------------------------------------------------

(defun compile-multi-return-function-call (func args env call-form)
  (let* ((type-set (resolve-func-set func args))
         (for-return (equal (v-multi-val-base env) *return-var-name-base*))
         (for-main (not (null (member :main (v-context env)))))
         (m-r-base (or (v-multi-val-base env)
                       (lisp-name->glsl-name 'nc env)))
         (m-r-names (loop :for i :from 1 :below (length type-set) :collect
                       (if (and for-return for-main)
                           (nth-return-name i (stage env) t)
                           (postfix-glsl-index m-r-base i))))
         (type-set type-set)
         (emit-set (emit-set func))
         (o (merge-compiled
             args
             :type-set type-set
             :current-line (gen-function-call-string func args m-r-names)
             :stemcells (mappend #'stemcells args)
             :pure (pure-p func)
             :emit-set emit-set
             :used-types (append
                          (mappend #'used-types args)
                          (coerce type-set 'list)
                          (coerce emit-set 'list))
             :place-tree (calc-place-tree func args)))
         (bindings (loop :for i :from 1 :below (length type-set) :collect
                      (let ((mval (aref type-set i)))
                        `((,(gensym "NC")
                            ,(type->type-spec mval))))))
         (call-obj
          ;; when (v-multi-val-base env) is not null then a return or mvbind
          ;; has already written the lets for the vars.
          (if (v-multi-val-base env)
              o
              (merge-progn
               (with-fresh-env-scope (fresh-env env)
                 (env-> (p-env fresh-env)
                   (merge-multi-env-progn
                    (compile-forms-not-propagating-env-returning-list-of-compiled
                     (lambda (env binding gname)
                       (with-v-let-spec binding
                         (compile-let name type-spec nil env gname)))
                     p-env bindings m-r-names))
                   (compile-form o p-env)))
               env env))))
    (when (user-function-p func)
      (incf (call-count (compiled-result func))))
    (values call-obj
            (make-env-with-place-modification-for-funcall func
                                                          args
                                                          env
                                                          call-form))))
