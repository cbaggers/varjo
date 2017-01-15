(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-list-form (code env)
  (if (eq (first code) 'funcall)
      (compile-funcall-form code env)
      (compile-func-form code env)))

(defun compile-func-form (code env)
  (let* ((func-name (first code))
         (args-code (rest code)))
    (when (keywordp func-name)
      (error 'keyword-in-function-position :form code))
    (let ((f-set (get-func-set-by-name func-name env)))
      (unless (functions f-set)
        (error 'could-not-find-function :name func-name))
      (compile-func-set-call f-set args-code env func-name code))))

(defun compile-func-set-call (func-set args-code env &optional name code)
  (let ((func-name (or name (%func-name-from-set func-set))))
    (dbind (func args) (find-function-in-set-for-args
                        func-set args-code env func-name)
      (typecase func
        (v-function (compile-function-call
                     func-name func args env))
        (external-function (compile-external-function-call func args env))
        (v-error (if (v-payload func)
                     (error (v-payload func))
                     (error 'cannot-compile
                            :code (or code
                                      `(funcall ,func-set ,@args-code)))))
        (t (error 'problem-with-the-compiler :target func))))))

(defun get-actual-function (func-code-obj code)
  (let* ((func (ctv (code-type func-code-obj))))
    (restart-case (typecase func
                    (v-function func)
                    (v-function-set func)
                    (t (error 'cannot-establish-exact-function
                              :funcall-form code)))
      ;;
      (allow-call-function-signature ()
        (values (make-dummy-function-from-type (code-type func-code-obj))
                t)))))

(defun compile-funcall-form (code env)
  (dbind (fc func-form . arg-forms) code
    (assert (eq fc 'funcall))
    (vbind (func-code-obj f-env) (compile-form func-form env)
      (let ((func (get-actual-function func-code-obj code)))
        (let ((functions (functions func)))
          (if (= 1 (length functions))
              (compile-funcall-1-func-form
               (first (functions func)) arg-forms f-env func-code-obj env)
              (compile-func-set-call func arg-forms env nil code)))))))

(defun compile-funcall-1-func-form (func arg-forms f-env func-code-obj env)
  (labels ((compile-and-assert-args-for-funcall (func args-code env)
             ;; {TODO} handle and return env
             (assert (= (length args-code) (length (v-argument-spec func))))
             (let ((args (try-compile-args args-code env))
                   (func-arg-types (v-argument-spec func)))
               (assert (and
                        (= (length func-arg-types) (length args))
                        (every (lambda (s a) (v-casts-to-p a s env))
                               func-arg-types
                               (mapcar #'code-type args))))
               (mapcar #'cast-code args func-arg-types))))
    ;;
    (let ((args (compile-and-assert-args-for-funcall func arg-forms f-env)))
      (vbind (o e) (compile-function-call (name func) func args f-env)
        (let ((ast (ast-node! :funcall (cons (node-tree func-code-obj)
                                             (mapcar #'node-tree args))
                              (code-type o) (flow-ids o) env env)))
          (values (merge-obs (list func-code-obj o)
                             :type (code-type o)
                             :node-tree ast
                             :current-line (current-line o))
                  e))))))

(defun compile-function-call (func-name func args env)
  (vbind (code-obj new-env)
      (cond
        ;; special funcs
        ((v-special-functionp func) (compile-special-function func args env))

        ;; funcs with multiple return values
        ((> (length (v-return-spec func)) 1)
         (compile-multi-return-function-call func-name func args env))

        ;; funcs taking compile time values as arguments
        ((and (typep func 'v-user-function)
              (some λ(typep _ 'v-compile-time-value)
                    (v-argument-spec func)))
         (compile-function-taking-ctvs func-name func args env))

        ;; all the other funcs :)
        (t (compile-regular-function-call func-name func args env)))
    (assert new-env)
    (values code-obj new-env)))

(defun compile-function-taking-ctvs (func-name func args env)
  (assert (v-code func))
  (labels ((ctv-p (x) (typep (v-type-of x) 'v-compile-time-value)))
    (dbind (args-code body-code) (v-code func)
      (dbind (trimmed-args hard-coded)
          (loop :for arg-code :in args-code :for arg :in args
             :if (ctv-p arg) :collect (list (first arg-code) arg) :into h
             :else :collect arg-code :into a
             :finally (return (list a h)))
        (expand-and-compile-form
         `(let ,hard-coded
            (labels-no-implicit ((,func-name ,trimmed-args ,@body-code))
                                ,(mapcar #'first hard-coded)
                                (,func-name ,@(remove-if #'ctv-p args))))
         env)))))

(defun compile-with-external-func-in-scope (func body-form env)
  ;; Here we are going to make use of the fact that a external function
  ;; is not allowed to mutate the environment it was called from.
  ;; We are going to grab the base environment and compile the labels form
  ;; there. We can then extract the signatures we need from this and add them
  ;; to the final source. The deduplication will be achieved by the fact that
  ;; we will use the external-function object as a key to a hashtable in the
  ;; base-env which will cache the results of these compiled external functions.
  ;; We will however, expand the macros in the current environment so that
  ;; macrolet (and it's kin) will work when we get around to adding them.
  ;;
  (let* ((base-env (get-base-env env))
         (compiled-func (or (compiled-functions base-env func)
                            (build-external-function func base-env))))
    (setf (compiled-functions base-env func) compiled-func)
    (vbind (code-obj new-env)
        (compile-function-call (name func)
                               (function-obj compiled-func)
                               (rest body-form)
                               env)
      (values code-obj new-env))))

(defun compile-external-function-call (func args env)
  (compile-with-external-func-in-scope func `(,(name func) ,@args) env))

(defun calc-place-tree (func args)
  (when (v-place-function-p func)
    (let ((i (v-place-index func)))
      (cons (list func (elt args i)) (place-tree (elt args i))))))

(defun compile-regular-function-call (func-name func args env)
  (let* ((c-line (gen-function-string func args))
         (flow-ids (calc-function-return-ids-given-args func func-name args))
         ;; This is one of the few cases where we want to set a flow id
         ;; regardless of the current state
         (type (resolve-func-type func args env))
         (type (if (flow-ids type)
                   (replace-flow-id type flow-ids)
                   (set-flow-id type flow-ids))))
    (unless type (error 'unable-to-resolve-func-type
                        :func-name func-name :args args))
    (values (merge-obs args
		       :type type
		       :current-line c-line
		       :signatures (mapcat #'signatures args)
		       :stemcells (mapcat #'stemcells args)
		       :multi-vals (when (v-multi-val-safe env)
				     (handle-regular-function-mvals args))
		       :place-tree (calc-place-tree func args)
		       :node-tree (ast-node! func (mapcar #'node-tree args)
					     type flow-ids env env))
	    env)))

(defun handle-regular-function-mvals (args)
  ;; ok so by getting here a few things are true:
  ;; - we were compiling a regular function which usually stops 'values
  ;;   from propagating up the stack
  ;; - someone is subverting this by using the 'value-safe special-form.
  ;;   let's hope they know what they are doing :p
  ;; - we now have to work out the mvals for a regular function.
  (let ((count (count-if #'multi-vals args)))
    (cond ((> count 1) (error 'values-safe-wasnt-safe :args args))
          ((= count 1) (multi-vals (find-if #'multi-vals args)))
          (t nil))))

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

(defun calc-function-return-ids-given-args (func func-name arg-code-objs)
  ;; {TODO} (warn "calc-function-return-ids-given-args should be merged with resolve-func-type")
  (when (typep (flow-ids func) 'multi-return-flow-id)
    (error 'multiple-flow-ids-regular-func func-name func))
  (unless (type-doesnt-need-flow-id (first (v-return-spec func)))
    (%calc-flow-id-given-args (in-arg-flow-ids func)
                              (flow-ids func)
                              arg-code-objs)))

(defun calc-mfunction-return-ids-given-args (func func-name arg-code-objs)
  (let ((all-return-types (cons (first (v-return-spec func))
                                (mapcar #'v-type
                                        (mapcar #'multi-val-value
                                                (rest (v-return-spec func))))))
        (m-flow-id (flow-ids func)))
    (assert (typep m-flow-id 'multi-return-flow-id))
    (let ((flow-ids (m-value-ids m-flow-id)))
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
         (type (replace-flow-id (resolve-func-type func args env)
                                (first flow-ids))))
    (unless type (error 'unable-to-resolve-func-type :func-name func-name
                        :args args))
    (let* ((has-base (not (null (v-multi-val-base env))))
           (m-r-base (or (v-multi-val-base env)
                         (lisp-name->glsl-name 'nc env)))
           (mvals (rest (v-return-spec func)))
           (start-index 1)
           (m-r-names (loop :for i :from start-index
                         :below (+ start-index (length mvals)) :collect
                         (fmt "~a~a" m-r-base i))))
      (let* ((bindings (loop :for mval :in mvals :collect
                          `((,(gensym "NC")
                              ,(type->type-spec
                                (v-type (multi-val-value mval)))))))

             (o (merge-obs
                 args :type type
                 :current-line (gen-function-string func args m-r-names)
                 :signatures (mapcat #'signatures args)
                 :stemcells (mapcat #'stemcells args)
                 :multi-vals (mapcar (lambda (_ _1 fid)
                                       (make-mval
                                        (v-make-value
                                         (replace-flow-id
                                          (v-type (multi-val-value _))
                                          fid)
                                         env :glsl-name _1
                                         :function-scope 0)))
                                     mvals
                                     m-r-names
                                     (rest flow-ids))
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
        (values (copy-code final :node-tree (ast-node! func
                                                       (mapcar #'node-tree args)
                                                       type
                                                       (flow-ids final)
                                                       env env))
                env)))))

;;----------------------------------------------------------------------

(defun end-line (obj &optional force)
  (when obj
    (if (and (typep (code-type obj) 'v-none) (not force))
        obj
        (if (null (current-line obj))
            obj
            (copy-code obj :current-line (format nil "~a;" (current-line obj))
                       :multi-vals nil
                       :place-tree nil)))))
