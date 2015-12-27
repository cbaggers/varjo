(in-package :varjo)
(in-readtable fn:fn-reader)

(defun compile-form (code env)
  (multiple-value-bind (code-obj new-env)
      (cond ((or (null code) (eq t code)) (compile-bool code env))
            ((numberp code) (compile-number code env))
            ((symbolp code) (compile-symbol code env))
            ((and (listp code) (listp (first code)))
             (error 'invalid-form-list :code code))
            ((listp code) (compile-list-form code env))
            ((typep code 'code) code)
            ((typep code 'v-value) (%v-value->code code env))
            (t (error 'cannot-compile :code code)))
    (values code-obj (or new-env env))))

(defun expand-and-compile-form (code env)
  "Special case generally used by special functions that need to expand
   any macros in the form before compiling"
  (pipe-> (code env)
    (equal #'symbol-macroexpand-pass
           #'macroexpand-pass
           #'compiler-macroexpand-pass)
    #'compile-form))

(defun compile-bool (code env)
  (let ((flow-id (flow-id!)))
    (if code
	(make-code-obj 'v-bool "true" :flow-ids flow-id
		       :node-tree (ast-node! :literal code :bool flow-id
					     env env))
	(make-code-obj 'v-bool "false" :flow-ids flow-id
		       :node-tree (ast-node! :literal code :bool flow-id
					     env env)))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (cond ((floatp x) (type-spec->type 'v-float))
        ((integerp x) (type-spec->type 'v-int))
        (t (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (let ((num-type (get-number-type code))
	(flow-id (flow-id!)))
    (make-code-obj num-type (gen-number-string code num-type)
		   :flow-ids flow-id
		   :node-tree (ast-node! :literal code num-type flow-id
					 env env))))

(defun v-variable->code-obj (var-name v-value from-higher-scope env)
  (let ((code-obj (make-code-obj (v-type v-value)
				 (gen-variable-string var-name v-value)
				 :flow-ids (flow-ids v-value)
				 :place-tree `((,var-name ,v-value))
				 :node-tree (ast-node! :get var-name
						       (v-type v-value)
						       (flow-ids v-value)
						       env env))))
    (if from-higher-scope
        (add-higher-scope-val code-obj v-value)
        code-obj)))

(defun %v-value->code (v-val env)
  (make-code-obj (v-type v-val) (v-glsl-name v-val)
		 :flow-ids (flow-ids v-val)
		 :node-tree (ast-node! :get-v-value (list (v-glsl-name v-val))
				       (v-type v-val) (flow-ids v-val)
				       env env)))

;; [TODO] move error
(defun compile-symbol (code env)
  (let* ((var-name code)
         (v-value (get-var var-name env)))
    (if v-value
        (let* ((val-scope (v-function-scope v-value))
               (from-higher-scope (and (> val-scope 0)
                                       (< val-scope (v-function-scope env)))))
          (v-variable->code-obj var-name v-value from-higher-scope env))
        (if (suitable-symbol-for-stemcellp var-name env)
	    (let ((scell (make-stem-cell code env))
		   (assumed-type (funcall *stemcell-infer-hook* var-name)))
	      (if assumed-type
		  (add-type-to-stemcell-code scell assumed-type)
		  scell))
            (error 'symbol-unidentified :sym code)))))

(defun compile-list-form (code env)
  (let* ((func-name (first code))
         (args-code (rest code)))
    (when (keywordp func-name)
      (error 'keyword-in-function-position :form code))
    (dbind (func args) (find-function-for-args func-name args-code env)
      (cond
        ((typep func 'v-function) (compile-function-call func-name func args env))
        ((typep func 'v-error) (if (v-payload func)
                                   (error (v-payload func))
                                   (error 'cannot-compile :code code)))
        (t (error 'problem-with-the-compiler :target func))))))


(defun compile-function-call (func-name func args env)
  (vbind (code-obj new-env)
      (cond
        ((v-special-functionp func) (compile-special-function func args env))

        ((multi-return-vars func) (compile-multi-return-function-call
                                   func-name func args env))

        (t (compile-regular-function-call func-name func args env)))
    (assert new-env)
    (values code-obj new-env)))


(defun calc-place-tree (func args)
  (when (v-place-function-p func)
    (let ((i (v-place-index func)))
      (cons (list func (elt args i)) (place-tree (elt args i))))))

(defun compile-regular-function-call (func-name func args env)
  (let* ((c-line (gen-function-string func args))
         (type (resolve-func-type func args env))
	 (flow-ids (calc-function-return-ids-given-args func func-name args)))
    (unless type (error 'unable-to-resolve-func-type
                        :func-name func-name :args args))
    (values (merge-obs args
		       :type type
		       :current-line c-line
		       :to-top (mapcan #'to-top args)
		       :signatures (mapcan #'signatures args)
		       :stemcells (mapcan #'stemcells args)
		       :flow-ids flow-ids
		       :place-tree (calc-place-tree func args)
		       :node-tree (ast-node! func (mapcar #'node-tree args)
					     type flow-ids env env))
	    env)))

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
	    (value-form (second ,full-spec)))
       (declare (ignorable value-form))
       (destructuring-bind (name &optional type-spec ,qual) ,var-spec
	 (declare (ignore ,qual))
	 ,@body))))

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
  (when (> (length (flow-ids func)) 1)
    (error 'multiple-flow-ids-regular-func func-name func))
  (unless (type-doesnt-need-flow-id (v-return-spec func))
    (%calc-flow-id-given-args (in-arg-flow-ids func)
			      (first (flow-ids func))
			      arg-code-objs)))

(defun calc-mfunction-return-ids-given-args (func func-name arg-code-objs)
  (let ((all-return-types (cons (v-return-spec func)
				(mapcar #'v-type
					(mapcar #'multi-val-value
						(multi-return-vars func)))))
	(flow-ids (flow-ids func)))
    (if (some #'type-doesnt-need-flow-id all-return-types)
	(error 'invalid-flow-id-multi-return :func-name func-name
	       :return-type all-return-types)
	(mapcar #'(lambda (x i)
		    (%calc-flow-id-given-args
		     (in-arg-flow-ids func) x arg-code-objs i))
		flow-ids
		(iota (length flow-ids))))))

(defun compile-multi-return-function-call (func-name func args env)
  (let* ((type (resolve-func-type func args env)))
    (unless type (error 'unable-to-resolve-func-type :func-name func-name
                        :args args))
    (let* ((has-base (not (null (v-multi-val-base env))))
           (m-r-base (or (v-multi-val-base env)
                         (safe-glsl-name-string (free-name 'nc))))
           (mvals (multi-return-vars func))
           (start-index 1)
           (m-r-names (loop :for i :from start-index
                         :below (+ start-index (length mvals)) :collect
                         (fmt "~a~a" m-r-base i))))
      (let* ((bindings (loop :for mval :in mvals :collect
                          `((,(free-name 'nc)
                              ,(type->type-spec
                                (v-type (multi-val-value mval)))))))
	     (flow-ids (calc-mfunction-return-ids-given-args
			func func-name args))
             (o (merge-obs
                 args :type type
                 :current-line (gen-function-string func args m-r-names)
                 :to-top (mapcan #'to-top args)
                 :signatures (mapcan #'signatures args)
                 :stemcells (mapcan #'stemcells args)
                 :multi-vals (mapcar (lambda (_ _1 fid)
                                       (make-mval
					(v-make-value
					 (v-type (multi-val-value _))
					 env :glsl-name _1 :flow-ids fid
					 :function-scope 0)))
                                     mvals
                                     m-r-names
				     (rest flow-ids))
		 :flow-ids (list (first flow-ids))
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



;;[TODO] Maybe the error should be caught and returned,
;;       in case this is a bad walk
;;{TODO} expand on this please. 'Future-you' couldnt work out what this meant
;; {TODO} you from both of your futures here. I think he was saying that
;;        the errors coming out of a special function could have been the result
;;        of the special func using #'compile-form which tried compiling a
;;        function call but while testing for the right function it threw and
;;        error. I think that is wrong as the handler-case in compiler/functions
;;        should catch those. We need to review all this stuff anyway.
;;        In the case of special funcs there should never be any ambiguity, it
;;        HAS to be the correct impl
(defun compile-special-function (func args env)
  (multiple-value-bind (code-obj new-env)
      (handler-case (apply (v-return-spec func) (cons env args))
	(varjo-error (e) (invoke-debugger e)))
    ;;(assert (node-tree code-obj))
    (values code-obj new-env)))

;;----------------------------------------------------------------------

(defun compile-make-var (name-string type flow-ids)
  (make-code-obj type name-string :flow-ids flow-ids :node-tree :ignored))

;;----------------------------------------------------------------------

(defun typify-code (code-obj &optional new-value)
  (let* ((prefixed-line (prefix-type-declaration code-obj))
	 (current-line
	  (if new-value
	      (%gen-assignment-string prefixed-line (current-line new-value))
	      prefixed-line))
	 (flow-ids
	  (if new-value
	      (flow-ids new-value)
	      (flow-ids code-obj))))
    (copy-code code-obj
	       :type (code-type code-obj)
	       :current-line current-line
	       :flow-ids flow-ids
	       :node-tree :ignored
	       :multi-vals nil
	       :place-tree nil)))

;;----------------------------------------------------------------------

(defun compile-let (name type-spec value-form env &optional glsl-name flow-ids)
  (let* ((value-obj (when value-form (compile-form value-form env)))
	 (glsl-name (or glsl-name (safe-glsl-name-string
				   (free-name name env)))))

    (let ((type-spec (when type-spec (type-spec->type type-spec))))
      (%validate-var-types name type-spec value-obj)
      (let* ((flow-ids
	      (or flow-ids (when value-obj (flow-ids value-obj)) (flow-id!)))
	     (let-obj
	      (if value-obj
		  (typify-code (compile-make-var glsl-name
						 (or type-spec
						     (code-type value-obj))
						 flow-ids)
			       value-obj)
		  (typify-code (compile-make-var glsl-name type-spec
						 (flow-id!))))))
	(values
	 (copy-code let-obj
		    :type (type-spec->type 'v-none)
		    :current-line nil
		    :to-block (cons-end (current-line (end-line let-obj))
					(to-block let-obj))
		    :multi-vals nil
		    :place-tree nil
		    :flow-ids flow-ids
		    :node-tree (if value-form
				   (node-tree value-obj)
				   :ignored))
	 (add-var name
		  (v-make-value (or type-spec (code-type value-obj))
				env
				:glsl-name glsl-name
				:flow-ids flow-ids)
		  env))))))

;;----------------------------------------------------------------------

(defun compile-progn (body env)
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
    (values body-objs env)))

(defun mapcar-progn (func env list &rest more-lists)
  "Mapcar over the lists but pass the env as the first arg to the function
   on each call. If you return a new env it will be used for the remaining
   calls."
  (values (apply #'mapcar
		 (lambda (&rest args)
		   (vbind (code-obj new-env) (apply func (cons env args))
		     (when new-env (setf env new-env))
		     code-obj))
		 (cons list more-lists))
	  env))

(defun %merge-progn (code-objs starting-env final-env)
  (let ((last-obj (last1 code-objs)))
    (merge-obs code-objs
	       :type (code-type last-obj)
	       :current-line (current-line last-obj)
	       :to-block (merge-lines-into-block-list code-objs)
	       :multi-vals (multi-vals (last1 code-objs))
	       :flow-ids (flow-ids last-obj)
	       :node-tree (ast-node! 'progn (mapcar #'node-tree code-objs)
				     (code-type last-obj) (flow-ids last-obj)
				     starting-env final-env))))

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

;; %multi-env-progn functions runs each form one after the other
;; (just like progn) however, unlike progn, each form is evaluated with the
;; same environment this means that bindings in one wont be visable in another.
;; Finally the resulting environement is merged

(defun %mapcar-multi-env-progn (func env list &rest more-lists)
  (let* ((e (apply #'mapcar
		   (lambda (&rest args)
		     (vlist (apply func (cons env args))))
		   (cons list more-lists)))
	 (code-objs (mapcar #'first e))
	 (env-objs (mapcar #'second e))
	 (merged-env (reduce (lambda (_ _1) (merge-env _ _1))
			     env-objs)))
    (values code-objs merged-env)))

(defun %merge-multi-env-progn (code-objs)
  (merge-obs code-objs
	     :type (type-spec->type 'v-none)
	     :current-line nil
	     :to-block (append (mapcan #'to-block code-objs)
			       (mapcar (lambda (_) (current-line (end-line _)))
				       code-objs))
	     :to-top (mapcan #'to-top code-objs)
	     :flow-ids nil
	     :node-tree :ignored))

(defmacro merge-multi-env-progn (code-objs)
  (let ((co (gensym "code-objs"))
	(fe (gensym "final-env")))
    `(vbind (,co ,fe) ,code-objs
       (values (%merge-multi-env-progn ,co)
	       ,fe))))

;;----------------------------------------------------------------------

(defun end-line (obj &optional force)
  (when obj
    (if (and (typep (code-type obj) 'v-none) (not force))
	obj
	(if (null (current-line obj))
	    obj
	    (copy-code obj :current-line (format nil "~a;" (current-line obj))
		       :multi-vals nil
		       :place-tree nil
		       :flow-ids (flow-ids obj))))))

;; [TODO] this shouldnt live here
(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (stage-type :initarg :stage-type :accessor stage-type)
   (out-vars :initarg :out-vars :accessor out-vars)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)
   (function-calls :initarg :function-calls :accessor function-calls)
   (used-macros :initarg :used-macros :reader used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
			 :reader used-compiler-macros)
   (ast :initarg :ast :reader ast)
   (used-symbol-macros :initarg :used-symbol-macros
		       :reader used-symbol-macros)))
