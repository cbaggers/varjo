(in-package :varjo)
(in-readtable fn:fn-reader)

;;{TODO} make it handle multiple assignements like cl version
(v-defmacro setf (&rest args)
  (labels ((make-set-form (p v)
	     (if (symbolp p) `(setq ,p ,v) `(setf-1 ,p ,v))))
    (let ((pairs (group args 2)))
      (if (= (length pairs) 1)
	  (make-set-form (first (first pairs)) (second (first pairs)))
	  `(progn
	     ,@(loop :for (p v) :in pairs :collect (make-set-form p v)))))))

(v-defspecial setf-1 ((place v-type) (val v-type))
  :return
  (cond
    ((not (place-tree place))
     (error 'non-place-assign :place place :val val))
    ((not (v-type-eq (code-type place) (code-type val)))
     (error 'setf-type-match :code-obj-a place :code-obj-b val))
    (t (destructuring-bind (name value) (last1 (place-tree place))
	 (when (v-read-only value)
	   (error 'setf-readonly :var-name name))
	 (unless (or (= (v-function-scope env) (v-function-scope value))
		     (= (v-function-scope value) 0))
	   (error 'cross-scope-mutate :var-name name
		  :code (format nil "(setf (... ~s) ...)" name)))
	 (multiple-value-bind (old-val old-env) (get-var name env)
	   (assert (eq old-val value))
	   (let ((final-env (replace-flow-ids name old-val (flow-ids val)
					      old-env env)))
	     (values (merge-obs (list place val) :type (code-type place)
				:current-line (gen-assignment-string place val)
				:flow-ids (flow-ids val)
				:node-tree (ast-node! 'setf
						      (list (node-tree place)
							    (node-tree val))
						      (code-type place)
						      (flow-ids val)
						      env
						      final-env))
		     final-env)))))))

(v-defspecial setq (var-name new-val-code)
  :args-valid t
  :return
  (let ((new-val (compile-form new-val-code env)))
    (assert (symbolp var-name))
    (multiple-value-bind (old-val old-env)
	(get-var var-name env)
      (assert (and old-val old-env))
      (cond
	((v-read-only old-val)
	 (error 'setq-readonly :code `(setq ,var-name ,new-val-code)
		:var-name var-name))
	((and (not (= (v-function-scope old-val) (v-function-scope env)))
	      (> (v-function-scope old-val) 0)) ;; ok if var is global
	 (error 'cross-scope-mutate :var-name var-name
		:code `(setq ,var-name ,new-val-code))))

      (let ((actual-type (get-setq-type new-val old-val var-name))
	    (final-env (replace-flow-ids var-name old-val
					 (flow-ids new-val)
					 old-env env)))
	(values (copy-code new-val :type actual-type
			   :current-line (gen-setq-assignment-string
					  old-val new-val)
			   :flow-ids (flow-ids new-val)
			   :multi-vals nil
			   :place-tree nil
			   :node-tree (ast-node! 'setq
						 (list var-name
						       (node-tree new-val))
						 actual-type
						 (flow-ids new-val)
						 env
						 final-env))
		final-env)))))

(defun get-setq-type (new-val old-val var-name)
  (restart-case (if (v-type-eq (v-type old-val) (code-type new-val))
		    (code-type new-val)
		    (error 'setq-type-match :var-name var-name
			   :old-value old-val :new-value new-val))
    (setq-supply-alternate-type (replacement-type-spec)
      (type-spec->type replacement-type-spec))))

(defun replace-flow-ids (old-var-name old-val flow-ids old-env env)
  (labels ((w (n)
	     (if (eq n old-env)
		 (env-replace-parent
		  n (v-parent-env n) :variables
		  (a-add old-var-name
			 (v-make-value
			  (v-type old-val)
			  n
			  :read-only (v-read-only old-val)
			  :function-scope (v-function-scope old-val)
			  :flow-ids flow-ids
			  :glsl-name (v-glsl-name old-val))
			 (copy-list (v-variables n))))
		 (env-replace-parent n (w (v-parent-env n))))))
    (if (or (eq old-env *global-env*) (typep old-env 'base-environment))
	env
	(w env))))

(v-defmacro prog1 (&body body)
  (let ((tmp (free-name 'progn-var)))
    `(let ((,tmp ,(first body)))
       ,@(rest body)
       ,tmp)))

(v-defspecial progn (&rest body)
  ;; this is super important as it is the only function that implements
  ;; imperitive coding. It does this by passing the env from one form
  ;; to the next.
  ;; it also returns this mutated env
  :args-valid t
  :return
  (if body
      (merge-progn (compile-progn body env) env)
      (error 'empty-progn)))

(v-defspecial multiple-value-bind (vars value-form &rest body)
  :args-valid t
  :return
  (let* ((base (safe-glsl-name-string (free-name 'mvb)))
	 (new-env (fresh-environment env :multi-val-base base)))
    (let ((value-obj (compile-form value-form new-env)))
      (unless (= (length vars) (+ 1 (length (multi-vals value-obj))))
        (error 'multi-val-bind-mismatch :val-form value-form :bindings vars))
      (let* ((mvals (multi-vals value-obj))
             (v-vals (mapcar #'multi-val-value mvals))
             (types (cons (code-type value-obj) (mapcar #'v-type v-vals))))
	(vbind ((m-objs s-obj b-objs) final-env)
	    (with-fresh-env-scope (fresh-env env)
	      (env-> (p-env fresh-env)
		(%mapcar-multi-env-progn
		 (lambda (env type name i)
		   (compile-let name (type->type-spec type) nil env
				(format nil "~a~a" base i)))
		 p-env types vars (iota (length types)))
		(compile-form `(setq ,(first vars) ,value-obj) p-env)
		(compile-progn body p-env)))
	  (let* ((m-obj (%merge-multi-env-progn m-objs))
		 (merged (merge-progn `(,m-obj ,s-obj ,@b-objs)
				      env final-env)))
	    (values
	     (copy-code
	      merged
	      :node-tree (ast-node! 'multiple-value-bind
				    `(,vars ,(node-tree value-obj)
					    ,@(mapcar #'node-tree b-objs))
				    (code-type merged)
				    (flow-ids merged)
				    env final-env))
	     final-env)))))))

(v-defspecial varjo-lang:values-safe (form)
  ;; this special-form executes the form without destroying
  ;; the multi-return 'values' travalling up the stack.
  ;; Progn is implictly values-safe, but * isnt by default.
  ;;
  ;; it will take the values from whichever argument has them
  ;; if two of the arguments have them then values-safe throws
  ;; an error
  :args-valid t
  :return
  (let ((safe-env (fresh-environment
		   env :multi-val-base (v-multi-val-base env)
		   :multi-val-safe t)))
    (vbind (c e) (compile-list-form form safe-env)
      (let* ((final-env (fresh-environment e :multi-val-safe nil))
	     (ast (ast-node! 'varjo-lang:values-safe
			     (list (node-tree c))
			     (code-type c)
			     (flow-ids c)
			     env
			     final-env)))
	(values (copy-code c :node-tree ast)
		final-env)))))

(v-defspecial values (&rest values)
  :args-valid t
  :return
  (if (v-multi-val-base env)
      (%values values env)
      (expand-and-compile-form `(prog1 ,@values) env)))

(defun %values (values env)
  (let* ((new-env (fresh-environment env :multi-val-base nil))
	 (qualifier-lists (mapcar #'extract-value-qualifiers values))
	 (forms (mapcar #'extract-value-form values))

	 (objs (mapcar λ(compile-form _ new-env) forms))
	 (base (v-multi-val-base env))
	 (glsl-names (loop :for i :below (length forms) :collect
			(format nil "~a~a" base i)))
	 (vals (loop :for o :in objs :for n :in glsl-names :collect
		  (v-make-value (code-type o) env :glsl-name n
				:flow-ids (flow-ids o))))
	 (first-name (free-name 'v-tmp env))
	 (result (expand-and-compile-form
		  `(let ((,first-name ,(first objs)))
		     ,@(loop :for o :in (rest objs)
			  :for v :in (rest vals) :collect
			  `(%assign ,v ,o))
		     ,first-name)
		  env))
	 (ast (ast-node! 'values
			 (mapcar λ(if _1 `(,@_1 ,(node-tree _)) (node-tree _))
				 objs
				 qualifier-lists)
			 (code-type result) (flow-ids result) env env)))
    (values (copy-code result :multi-vals (mapcar #'make-mval (rest vals)
						  (rest qualifier-lists))
		       :node-tree ast)
	    env)))

;; %assign is only used to set the current-line of the code object
;; it has no side effects on the compilation itself
(v-defspecial %assign ((place v-type) (val v-type))
  :return
  (values
   (merge-obs (list place val) :type (code-type place)
	      :current-line (gen-assignment-string place val)
	      :flow-ids (flow-ids val)
	      :node-tree (ast-node! '%assign (list place val)
				    (code-type place) (flow-ids val) env env))
   env))

(defun extract-value-qualifiers (value-form)
  (when (and (listp value-form) (keywordp (first value-form)))
    (butlast value-form)))

(defun extract-value-form (value-form)
  (if (and (listp value-form) (keywordp (first value-form)))
      (last1 value-form)
      value-form))

;;--------------------------------------------------

(v-defspecial %return (form)
  :args-valid t
  :return
  (let ((new-env (fresh-environment
		  env
		  :multi-val-base "return")))
    ;; we create an environment with the signal to let any 'values' forms
    ;; down the tree know they will be caught and what their name prefix should
    ;; be.
    ;; We then compile the form using the augmented environment, the values
    ;; statements will expand and flow back as 'multi-vals' and the current-line
    ;; now there are two styles of return:
    ;; - The first is for a regular function, in which multivals become
    ;;   out-arguments and the current-line is returned
    ;; - The second is for a shader stage in which the multi-vars become
    ;;   output-variables and the current line is handled in a 'context'
    ;;   specific way.
    (let* ((code-obj (compile-form form new-env))
           (result
            (if (member :main (v-context env))
                (%main-return code-obj env)
                (%regular-value-return code-obj))))
      (values (copy-code result
			 :node-tree (ast-node! '%return (node-tree code-obj)
					       (code-type result)
					       (flow-ids result)
					       env env))
	      env))))

;; Used when this is a labels (or otherwise local) function
(defun %regular-value-return (code-obj)
  (let ((flow-result
	 (if (multi-vals code-obj)
	     (m-flow-id! (cons (flow-ids code-obj)
			       (mapcar (lambda (c)
					 (flow-ids (multi-val-value c)))
				       (multi-vals code-obj))))
	     (flow-ids code-obj))))
    (copy-code
     code-obj :type 'v-void
     :current-line (format nil "return ~a" (current-line code-obj))
     :returns (cons (code-type code-obj) (multi-vals code-obj))
     :flow-ids flow-result
     :multi-vals nil
     :place-tree nil)))


;; Used when this is the main stage function
;; this
(defun %main-return (code-obj env)
  (if (multi-vals code-obj)
      (let* ((mvals (multi-vals code-obj))
             (v-vals (mapcar #'multi-val-value mvals))
             (types (mapcar #'v-type v-vals))
             (glsl-lines (mapcar #'v-glsl-name v-vals)))

	(merge-progn
	 (with-fresh-env-scope (fresh-env env)
	   (env-> (p-env fresh-env)
	     (merge-multi-env-progn
	      (%mapcar-multi-env-progn
	       (lambda (p-env type gname)
		 (compile-let (free-name 'x p-env) (type->type-spec type)
			      nil p-env gname))
	       p-env types glsl-lines))
	     (compile-form (%default-out-for-stage code-obj p-env) p-env)
	     (compile-form `(progn ,@(mapcar λ(mval->out-form _ env)
					     (multi-vals code-obj)))
			   p-env)))
	 env))
      (with-fresh-env-scope (fresh-env env)
	(compile-form (%default-out-for-stage code-obj env)
		      fresh-env))))

;; fragment comes first as it doesnt restrict the exit type...this is a bug
;; really as fragment out-var should be vec4...We should have a case for
;; when context includes all stages, in which case any type is allowed
(defun %default-out-for-stage (form env)
  (let ((context (v-context env)))
    (cond ((member :fragment context) `(%out (,(free-name :output-color env))
                                             ,form))
          ((member :vertex context) `(setq varjo-lang::gl-position ,form))
          (t (error "Have not implemented #'values defaults for this stage ~a"
                    env)))))

(defun %validate-var-types (var-name type code-obj)
  (when (and code-obj (typep (code-type code-obj) 'v-stemcell))
    (error "Code not ascertain the type of the stemcell used in the let form:~%(~a ~a)"
           (string-downcase var-name) (current-line code-obj)))
  (when (and (null type) (null code-obj))
    (error "Could not establish the type of the variable: ~s" var-name))
  (when (and code-obj type (not (v-type-eq (code-type code-obj) type)))
    (error "Type specified does not match the type of the form~%~s~%~s"
           (code-type code-obj) type))
  t)


(v-defspecial let (bindings &rest body)
  :args-valid t
  :return
  (progn
    (unless body (error 'body-block-empty :form-name 'let))
    (vbind ((new-var-objs body-obj) final-env)
	(with-fresh-env-scope (fresh-env env)
	  (env-> (p-env fresh-env)
	    (%mapcar-multi-env-progn
	     (lambda (p-env binding)
	       (with-v-let-spec binding
		 (compile-let name type-spec value-form p-env)))
	     p-env bindings)
	    (compile-form `(progn ,@body) p-env)))
      (let* ((merged (merge-progn (list (merge-multi-env-progn new-var-objs)
					body-obj)
				  env final-env))
	     (val-ast-nodes (mapcar λ(unless (eq (node-tree _) :ignored)
				       (list (node-tree _)))
				    new-var-objs))
	     (ast-args
	      (list (mapcar λ(with-v-let-spec _
			       (if type-spec
				   `((,name ,type-spec) ,@_1)
				   `(,name ,@_1)))
			    bindings
			    val-ast-nodes)
		    (node-tree body-obj))))
	(values
	 (copy-code merged :node-tree (ast-node! 'let ast-args (code-type merged)
						 (flow-ids merged)
						 env final-env))
	 final-env)))))

(v-defmacro let* (bindings &rest body)
  (unless body (error 'body-block-empty :form-name 'let))
  (let* ((bindings (reverse bindings))
         (result `(let (,(first bindings)) ,@body)))
    (loop :for binding :in (rest bindings) :do
       (setf result `(let (,binding) ,result)))
    result))

(defun make-func-env (env mainp)
  (if mainp
      (fresh-environment env :function-scope (1+ (v-function-scope env))
			 :context (cons :main (v-context env)))
      (fresh-environment env :function-scope (1+ (v-function-scope env)))))

(v-defspecial labels (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) e)
      (with-fresh-env-scope (fresh-env env)
	(env-> (p-env fresh-env)
	  (mapcar-progn
	   (lambda (env d)
	     (dbind (name args &rest body) d
	       (%make-function name args body t env)))
	   p-env definitions)
	  (compile-form `(progn ,@body) p-env)))
    (let* ((merged (merge-progn (cons-end body-obj func-def-objs) env e))
	   (ast (ast-node! 'labels
			   (list (remove nil (mapcar λ(when _1
							(cons-end
							 (node-tree _1)
							 (subseq _ 0 2)))
						     definitions
						     func-def-objs))
				 (node-tree body-obj))
			   (code-type body-obj) (flow-ids merged) env env)))
      (values (copy-code merged :node-tree ast)
	      e))))

(v-defspecial labels-no-implicit (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) pruned-starting-env) ;;ending-env
      (with-fresh-env-scope (fresh-env env)
	(env-> (p-env fresh-env)
	  (mapcar-progn
	   (lambda (env d)
	     (dbind (name args &rest body) d
	       (%make-function name args body nil env)))
	   p-env definitions)
	  (compile-form `(progn ,@body) p-env)))
    (let* ((merged (merge-progn (cons-end body-obj (remove nil func-def-objs))
				env
				pruned-starting-env))
	   (ast (ast-node! 'labels-no-implicit
			   (list (remove nil (mapcar λ(if _1
							  (cons-end
							   (node-tree _1)
							   (subseq _ 0 2))
							  _)
						     definitions
						     func-def-objs))
				 (node-tree body-obj))
			  (code-type body-obj) (flow-ids merged) env env)))
      (values (copy-code merged :node-tree ast)
	      pruned-starting-env))))

(defun %make-function (name args body allow-implicit-args env)
  (let ((deduped-func (dedup-function `(,args ,body) env)))
    (if (and (not allow-implicit-args) deduped-func)
	(values nil (add-function name deduped-func env))
	(%make-new-function name args body allow-implicit-args env))))

(defun %make-new-function (name args body allow-implicit-args env)
  (unless (function-raw-args-validp args)
    (error 'bad-make-function-args
	   :func-name name
	   :arg-specs (remove-if #'function-raw-arg-validp args)))
  (let* ((mainp (eq name :main))
	 (env (make-func-env env mainp))
	 (in-arg-flow-ids (mapcar (lambda (_)
				    (declare (ignore _))
				    (flow-id!))
				  args))
	 (arg-glsl-names (loop :for (name) :in args :collect
			    (safe-glsl-name-string (free-name name))))
	 (body-env (reduce
		    (lambda (env tripple)
		      (dbind (arg glsl-name flow-ids) tripple
			(dbind (name type-spec) arg
			  (add-var name
				   (v-make-value type-spec env
						 :glsl-name glsl-name
						 :flow-ids flow-ids)
				   env))))
		    (mapcar #'list args arg-glsl-names in-arg-flow-ids)
		    ;; how odd is this?..we use the func if not main
		    :initial-value (if mainp
				       env
				       (process-environment-for-main-labels
					env))))
	 (body-obj (compile-form `(%return (progn ,@body)) body-env))
	 (glsl-name (if mainp "main" (safe-glsl-name-string (free-name name))))
	 (primary-return (first (returns body-obj)))
	 (multi-return-vars (rest (returns body-obj)))
	 (type (if mainp (type-spec->type 'v-void) primary-return))
	 (normalized-out-of-scope-args (normalize-out-of-scope-args
					(out-of-scope-args body-obj)))
	 (implicit-args (when allow-implicit-args
			  (remove-if λ(= (v-function-scope _)
					 (v-function-scope env))
				     normalized-out-of-scope-args))))
    (unless allow-implicit-args
      (unless (every λ(= (v-function-scope _) (v-function-scope env))
		     normalized-out-of-scope-args)
	(error 'illegal-implicit-args :func-name name)))
    (unless (or mainp primary-return) (error 'no-function-returns :name name))
    (when (v-typep type (type-spec->type :none))
      (error 'function-with-no-return-type :func-name name))
    (let* ((arg-pairs (loop :for (ignored type) :in args
			 :for name :in arg-glsl-names
			 :do (identity ignored) :collect
			 `(,(v-glsl-string (type-spec->type type)) ,name)))
	   (out-arg-pairs (loop :for mval :in multi-return-vars :for i :from 1
			     :for name = (v-glsl-name (multi-val-value mval)) :collect
			     `(,(v-glsl-string (v-type (multi-val-value mval)))
				,name)))
	   (sigs (if mainp
		     (signatures body-obj)
		     (cons (gen-function-signature glsl-name arg-pairs
						   out-arg-pairs type
						   (when allow-implicit-args
						     implicit-args))
			   (signatures body-obj))))
	   (top (cons-end (gen-function-body-string
			   glsl-name (unless mainp arg-pairs)
			   out-arg-pairs type body-obj
			   (when allow-implicit-args implicit-args))
			  (to-top body-obj)))
	   (func (func-spec->user-function
		  (v-make-f-spec name
				 (gen-function-transform
				  glsl-name args
				  multi-return-vars
				  (when allow-implicit-args implicit-args))
				 nil ;;should be context
				 (mapcar #'second args)
				 type :glsl-name glsl-name
				 :multi-return-vars multi-return-vars
				 :implicit-args (when allow-implicit-args
						  implicit-args)
				 :flow-ids (flow-ids body-obj)
				 :in-arg-flow-ids in-arg-flow-ids)
		  env))
	   (final-env (add-function name func env)))
      (unless allow-implicit-args
	(push-non-implicit-function-for-dedup `(,args ,body) func env))
      (values (copy-code body-obj
			 :type (type-spec->type 'v-none)
			 :current-line nil
			 :signatures sigs
			 :to-top top
			 :to-block nil
			 :returns nil
			 :out-vars (out-vars body-obj)
			 :multi-vals nil
			 :place-tree nil
			 :out-of-scope-args (when allow-implicit-args
					      implicit-args)
			 :flow-ids nil)
	      final-env))))

(defun function-raw-args-validp (raw-args)
  (every #'function-raw-arg-validp raw-args))

(defun function-raw-arg-validp (raw-arg)
  (and (listp raw-arg)
       (>= (length raw-arg) 2)
       (not (null (first raw-arg)))
       (symbolp (first raw-arg))
       (not (keywordp (first raw-arg)))
       (type-specp (second raw-arg))))


;; {TODO} what if type of form is not value
(v-defspecial %out (name-and-qualifiers form)
  :args-valid t
  :return
  (let* ((form-obj (compile-form form env))
         (out-var-name (if (consp name-and-qualifiers)
                           (first name-and-qualifiers)
                           name-and-qualifiers))
         (qualifiers (when (consp name-and-qualifiers)
                       (rest name-and-qualifiers)))
         (glsl-name (safe-glsl-name-string out-var-name)))
    (if (assoc out-var-name *glsl-variables*)
        (error 'out-var-name-taken :out-var-name out-var-name)
	(values
	 (end-line
	  (copy-code
	   form-obj :type 'v-none
	   :current-line (gen-out-var-assignment-string glsl-name form-obj)
	   :to-block (to-block form-obj)
	   :out-vars (cons `(,out-var-name
			     ,qualifiers
			     ,(v-make-value (code-type form-obj) env
					    :glsl-name glsl-name))
			   (out-vars form-obj))
	   :node-tree (ast-node! '%out (list name-and-qualifiers
					     (node-tree form-obj))
				 nil (flow-ids form-obj) env env)
	   :multi-vals nil
	   :place-tree nil) t)
	 env))))

;; pretty sure env is wrong in 'or and 'and, what if there are side effects in
;; forms?
;; In fact function calls in general should at least propagate the flow ids
;; down the arg compiles..could even the env be passed? may just work

(v-defspecial or (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (compile-form x env)) forms))
	 (flow-id (flow-id!)))
    (unless (loop for o in objs always (v-code-type-eq o (first objs)))
      (error "all forms of an 'OR' form must resolve to the same type"))
    (if (v-typep (code-type (first objs)) (type-spec->type :bool))
        (values (merge-obs objs
			   :type :bool
			   :current-line (gen-bool-or-string objs)
			   :flow-ids flow-id
			   :node-tree (ast-node! 'and (mapcar #'node-tree objs)
						 :bool flow-id env env))
		env)
        (values (first objs) env))))

(v-defspecial and (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (compile-form x env)) forms))
	 (flow-id (flow-id!)))
    (unless (loop for o in objs always (v-code-type-eq o (first objs)))
      (error "all forms of an 'AND' form must resolve to the same type"))
    (if (v-typep (code-type (first objs)) (type-spec->type :bool))
        (values (merge-obs objs
			   :type :bool
			   :current-line (gen-bool-and-string objs)
			   :flow-ids flow-id
			   :node-tree (ast-node! 'and (mapcar #'node-tree objs)
						 :bool flow-id env env))
		env) ;; pretty sure this env is wrong, what if side effects in
	;;              forms?
        (values (last1 objs) env))))

;; note that just like in lisp this only fails if false. 0 does not fail.
;; the then and else statements must have the same type
;; the return type is the type of the then/else form
(v-defspecial if (test-form then-form &optional else-form)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (let ((always-true (or (eq test-form t)
			   (not (v-typep (code-type test-obj) 'v-bool)))))

      (vbind (then-obj then-env) (compile-form then-form test-env)
	(if always-true
	    (values (end-line then-obj) then-env)
	    (vbind (else-obj) (if else-form
				  (compile-form else-form test-env)
				  (error 'if-branch-type-mismatch
					 :then-obj then-obj))
	      (assert (v-code-type-eq then-obj else-obj))
	      (let ((result (free-name :result-from-if))
		    (result-type (type->type-spec (code-type then-obj))))
		(expand-and-compile-form
		 `(let (((,result ,result-type)))
		    (%if ,test-form
			 (setq ,result ,then-form)
			 (setq ,result ,else-form))
		    ,result)
		 env))))))))

;; note that just like in lisp this only fails if false. 0 does not fail.
;; this is the if statement from gl. It has a return type type of :none
;; and allows then and else statements that return different types
(v-defspecial %if (test-form then-form &optional else-form)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (let ((test-mutated-outer-scopes (not (env-same-vars env test-env))))
      (if (v-typep (code-type test-obj) 'v-bool)
	  (compile-regular-%if test-obj test-env then-form else-form env)
	  (if test-mutated-outer-scopes
	      (expand-and-compile-form `(progn ,test-form ,then-form) env)
	      (compile-constant-%if then-form test-env env))))))

(defun compile-constant-%if (then-form env starting-env)
  ;; the test form was not boolean, so the if would always
  ;; be true, so in this case we just use the 'then' form
  (vbind (then-obj then-env) (compile-form then-form env)
    (values (copy-code (end-line then-obj)
		       :type :none
		       :flow-ids nil
		       :node-tree
		       (ast-node!
			'%if
			(mapcar #'node-tree
				(list (node-tree (compile-form t starting-env))
				      (node-tree then-obj)))
			:none nil starting-env then-env)
		       :multi-vals nil
		       :place-tree nil)
	    then-env)))

(defun compile-regular-%if (test-obj test-env then-form else-form
			    starting-env)
  (multiple-value-bind (then-obj then-env)
      (compile-form then-form test-env)
    (multiple-value-bind (else-obj else-env)
	(when else-form (compile-form else-form test-env))
      ;; - - - -
      (let* ((arg-objs (remove-if #'null (list test-obj then-obj else-obj)))
	     (then-obj (end-line then-obj))
	     (else-obj (end-line else-obj)) ;; returns nil if given nil
	     (final-env
	      (if else-obj
		  (apply #'env-merge-history
			 (env-prune* (env-depth test-env) then-env else-env))
		  then-env))
	     (node-tree
	      (if else-obj
		  (ast-node! '%if (mapcar #'node-tree
					  (list test-obj then-obj else-obj))
			     :none nil starting-env final-env)
		  (ast-node! '%if (mapcar #'node-tree (list test-obj then-obj))
			     :none nil starting-env final-env))))
	(values (merge-obs arg-objs :type :none :current-line nil
			   :to-block (list (gen-if-string
					    test-obj then-obj else-obj))
			   :flow-ids nil
			   :node-tree node-tree)
		final-env)))))

;; {TODO} check keys
(v-defspecial switch (test-form &rest clauses)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (let* ((keys (mapcar #'first clauses))
	   (clause-pairs (mapcar λ(multiple-value-list
				   (compile-form `(progn ,(second _)) env))
				 clauses))
	   (clause-objs (mapcar #'first clause-pairs))
	   (final-env
	    (let ((envs (apply #'env-prune* (env-depth test-env)
			       (mapcar #'second clause-pairs))))
	      (reduce #'env-merge-history
		      (rest envs) :initial-value (first envs)))))
      (if (and (v-typep (code-type test-obj) 'v-i-ui)
	       (loop :for key :in keys :always
		  (or (eq key 'default) (integerp key))))
	  (values (merge-obs clause-objs :type 'v-none
			     :current-line nil
			     :to-block (list (gen-switch-string test-obj keys
								clause-objs))
			     :flow-ids nil
			     :node-tree (ast-node!
					 'switch
					 (cons (node-tree test-obj)

					       (mapcar λ`(,(first _)
							   ,(node-tree _1))
						       clauses
						       clause-objs))
					 :none nil env final-env))
		  final-env)
	  (error 'switch-type-error test-obj keys)))))


;;   (for (a 0) (< a 10) (++ a)
;;     (* a 2))
(v-defspecial for (var-form condition update &rest body)
  :args-valid t
  :return
  (if (consp (first var-form))
      (error 'for-loop-only-one-var)
      (multiple-value-bind (code new-env)
	  (with-v-let-spec var-form
	    (compile-let name type-spec value-form env))
	(let* ((var-string (subseq (first (to-block code))
				   0
				   (1- (length (first (to-block code))))))
	       (decl-obj (compile-form (second var-form) new-env))
	       (condition-obj (compile-form condition new-env))
	       (update-obj (compile-form update new-env))
	       (flow-id (flow-id!)))
	  (unless (or (typep (code-type decl-obj) 'v-i-ui)
		      (v-typep (code-type decl-obj) 'v-float))
	    (error 'invalid-for-loop-type :decl-obj decl-obj))
	  (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body) new-env)
	    (if (and (null (to-block condition-obj)) (null (to-block update-obj)))
		(values (copy-code
			 body-obj :type 'v-none :current-line nil
			 :to-block `(,(gen-for-loop-string
				       var-string condition-obj update-obj
				       (end-line body-obj)))
			 :flow-ids flow-id
			 :node-tree (ast-node!
				     'for (cons var-form
						(mapcar #'node-tree
							(list condition-obj
							      update-obj
							      body-obj)))
				     :none flow-id env final-env)
			 :multi-vals nil
			 :place-tree nil)
			final-env)
		(error 'for-loop-simple-expression)))))))


(v-defspecial while (test &rest body)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test env)
    (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body)
							     test-env)
      (if (v-typep (code-type test-obj) 'v-bool)
	  (values (merge-obs (list body-obj test-obj)
			     :type 'v-none :current-line nil
			     :to-block (list (gen-while-string
					      test-obj (end-line body-obj)))
			     :flow-ids nil
			     :node-tree (ast-node!
					 'while (mapcar #'node-tree
							(list test-obj
							      body-obj))
					 :none nil env final-env))
		  final-env)
	  (error 'loop-will-never-halt :test-code test :test-obj test-obj)))))

(defun search-for-flow-id-fixpoint (code starting-env)
  ;; Lets document this a bit and work out how to debug it from a crash

  (let ((envs (list starting-env))
	(last-code-obj nil)
	(flow-ids nil)
	(checkpoint (checkpoint-flow-ids)))
    (loop :for pass :from 0
       :for current-env = (first envs)
       :until (vbind (o new-env) (compile-form code current-env)
		(let* ((new-flow-ids (get-new-flow-ids new-env current-env))
		       (f-ids (or flow-ids
				  (mapcar λ`(,_ . ,(flow-ids (get-var _ starting-env)))
					  (mapcar #'car new-flow-ids)))))
		  (setf last-code-obj o
			envs (cons new-env envs)
			flow-ids (accumulate-flow-ids f-ids new-flow-ids))
		  (let ((done (fixpoint-reached
			       new-flow-ids starting-env pass)))
		    (unless done (reset-flow-ids-to-checkpoint checkpoint))
		    done))))
    (values last-code-obj
	    (create-post-loop-env flow-ids starting-env))))

;; defun replace-flow-ids (old-var-name old-val flow-ids old-env env)
(defun create-post-loop-env (new-flow-id-pairs starting-env)
  (labels ((splice-in-flow-id (accum-env id-pair)
	     (dbind (vname . new-flow-id) id-pair
	       (vbind (old-val old-env) (get-var vname accum-env)
		 (replace-flow-ids vname old-val new-flow-id
				   old-env accum-env)))))
    (reduce #'splice-in-flow-id new-flow-id-pairs :initial-value starting-env)))

(defun accumulate-flow-ids (flow-ids new-flow-ids)
  (labels ((x (accum y)
	     (dbind (vname . fid) y
	       (acons vname (flow-id! (assocr vname accum)
				      fid)
		      accum))))
    (remove-duplicates
     (reduce #'x new-flow-ids :initial-value flow-ids)
     :test #'eq :key #'first :from-end t)))

(defvar *max-resolve-loop-flow-id-pass-count* 100)

(defun get-new-flow-ids (latest-env last-env)
  (let* ((variables-changed (find-env-vars latest-env last-env
					   :test (complement #'eq)
					   :stop-at-base t))
	 ;; now we need to take these a remove any which have the
	 ;; same flow-id. This can happen if a variable is set to
	 ;; itself from within a loop
	 (trimmed-changes
	  (mapcar λ(let ((last-var (get-var _ last-env))
			 (new-var (get-var _ latest-env)))
		     (unless (or (not last-var)
				 (not new-var)
				 (id= (flow-ids last-var)
				      (flow-ids new-var)))
		       _))
		  variables-changed)))
    (mapcar λ`(,_ . ,(flow-ids (get-var _ latest-env)))
	    (remove nil trimmed-changes))))

(defun fixpoint-reached (new-flow-ids starting-env pass)
  (unless (< pass *max-resolve-loop-flow-id-pass-count*)
    (error 'loop-flow-analysis-failure))
  (let* ((variables-changed (mapcar #'car new-flow-ids)))
    (or
     ;; if no variable from outer scope changed then we stop
     (not variables-changed)
     ;; if none of the variables that we changed were set to
     ;; values from the outer scope we stop (as information
     ;; has stopped flowing into the loop, there is nothing
     ;; else to glean)
     (let* ((starting-flow-ids (mapcar λ(flow-ids (get-var _ starting-env))
				       variables-changed))
	    (starting-super-id (reduce #'flow-id! starting-flow-ids)))
       (not (some λ(id~= _ starting-super-id)
		  (mapcar #'cdr new-flow-ids)))))))



(v-defmacro s~ (&rest args) `(swizzle ,@args))
(v-defspecial swizzle (vec-form components)
  :args-valid t
  :return
  (let* ((vec-obj (compile-form vec-form env))
         (allowed (subseq (list #\x #\y #\z #\w) 0
                          (first (v-dimensions (code-type vec-obj)))))
         (comp-string (if (keywordp components)
                          (string-downcase (symbol-name components))
                          (error 'swizzle-keyword :item components)))
         (new-len (length comp-string)))
    (if (and (>= new-len 2) (<= new-len 4)
             (v-typep (code-type vec-obj) 'v-vector)
             (loop :for c :across comp-string
                :always (find c allowed)))
	(let ((r-type (type-spec->type (p-symb 'varjo 'v-vec new-len)))
	      (flow-id (flow-id!)))
	  (values
	   (copy-code vec-obj :type r-type
		      :current-line (gen-swizzle-string vec-obj comp-string)
		      :flow-ids flow-id
		      :node-tree (ast-node! 'swizzle
					    `(,(node-tree vec-obj) ,components)
					    r-type flow-id env env)
		      :multi-vals nil
		      :place-tree nil)
	   env))
        (error "swizzle form invalid"))))


(v-defspecial the (type-name form)
  :args-valid t
  :return
  (let* ((compiled (compile-form form env))
	 (obj (if (stemcellp (code-type compiled))
		  (add-type-to-stemcell-code compiled type-name)
		  (if (v-typep (code-type compiled)
			       (type-spec->type type-name))
		      compiled ;{TODO} proper error here
		      (error "Incorrect declaration that ~a was of type ~a"
			     compiled type-name)))))
    (values
     (copy-code
      obj
      :node-tree (ast-node! 'the (list type-name (node-tree compiled))
			    (code-type compiled) (flow-ids compiled) env env))
     env)))

(v-defspecial %break (&optional datum &rest args)
  :args-valid t
  :return
  (progn
    (break (format nil "Varjo compiler breakpoint:~%~s" (or datum ""))
	   (mapcar λ(compile-form _ env) args))
    (values (make-none-ob) env)))

(v-defspecial %peek (form)
  :args-valid t
  :return
  (vbind (o e) (compile-form form env)
    (break "Varjo Peek:~%:code-obj ~s~%:env ~s" o e)
    (values o e)))

(v-defspecial glsl-expr (glsl-string type-spec)
  :args-valid t
  :return
  (values
   (compile-glsl-expression-string glsl-string type-spec)
   env))

(defun compile-glsl-expression-string (current-line type)
  (let* ((type-obj (if (typep type 'v-t-type) type (type-spec->type type)))
	 (type-spec (type->type-spec type-obj))
	 (flow-id (flow-id!)))
    (code! :type type-obj
	   :current-line current-line
	   :used-types (list type-spec)
	   :node-tree (ast-node! 'glsl-string nil type-obj flow-id nil nil)
	   :flow-ids flow-id)))

(defun glsl-let (name-symbol name-string type value-form env)
  (let ((type-spec (if (typep type 'v-t-type) (type->type-spec type) type)))
    (compile-let name-symbol type-spec value-form env name-string)))
