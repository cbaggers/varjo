;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)
(in-readtable fn:fn-reader)

(v-defspecial %fresh-env-scope (&rest body)
  :args-valid t
  :return (let ((new-env (fresh-environment env)))
	    (vbind (v e) (varjo->glsl `(progn ,@body) new-env)
	      (values v (env-prune (env-depth env) e)))))

;;{TODO} make it handle multiple assignements like cl version
(v-defmacro setf (&rest args)
  (labels ((make-set-form (p v)
	     (if (symbolp p) `(setq ,p ,v) `(setf-1 ,p ,v))))
    (let ((pairs (group args 2)))
      (if (= (length pairs) 1)
	  (make-set-form (first (first pairs)) (second (first pairs)))
	  `(progn ,@(loop :for (p v) :in pairs :collect (make-set-form p v)))))))

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
	   (values (merge-obs (list place val) :type (code-type place)
			      :current-line (gen-assignment-string place val)
			      :flow-ids (flow-ids val))
		   (replace-flow-ids name old-val (flow-ids val)
				     old-env env)))))))

(v-defspecial setq (var-name new-val-code)
  :args-valid t
  :return
  (let ((new-val (varjo->glsl new-val-code env)))
    (assert (symbolp var-name))
    (multiple-value-bind (old-val old-env)
	(get-var var-name env)
      (assert (and old-val old-env))
      (cond
	((v-read-only old-val)
	 (error 'setq-readonly :code `(setq ,var-name ,new-val-code)
		:var-name var-name))
	((not (v-type-eq (v-type old-val) (code-type new-val)))
	 (error 'setq-type-match :var-name var-name :old-value old-val
		:new-value new-val))
	((and (not (= (v-function-scope old-val) (v-function-scope env)))
	      (> (v-function-scope old-val) 0)) ;; ok if var is global
	 (error 'cross-scope-mutate :var-name var-name
		:code `(setq ,var-name ,new-val-code)))
	(t (values (merge-obs new-val :type (code-type new-val)
			      :current-line (gen-setq-assignment-string old-val new-val)
			      :flow-ids (flow-ids new-val))
		   (replace-flow-ids var-name old-val (flow-ids new-val)
				     old-env env)))))))

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

(v-defspecial progn (&rest body)
  ;; this is super important as it is the only function that implements
  ;; imperitive coding. It does this by passing the env from one form
  ;; to the next.
  ;; it also returns this mutated env
  :args-valid t
  :return
  (if body
      (let* ((mvb (v-multi-val-base env))
	     (env (fresh-environment env :multi-val-base nil))
	     (body-objs (append
			 (loop :for code :in (butlast body) :collect
			    (multiple-value-bind (code-obj new-env)
				(varjo->glsl code env)
			      (when new-env (setf env new-env))
			      code-obj))
			 (multiple-value-bind (code-obj new-env)
			     (varjo->glsl
			      (last1 body)
			      (fresh-environment env :multi-val-base mvb))
			   (when new-env (setf env new-env))
			   (list code-obj)))))
	(let ((last-obj (last1 body-objs)))
	  (values
	   (merge-obs body-objs
		      :type (code-type last-obj)
		      :current-line (current-line last-obj)
		      :to-block (merge-lines-into-block-list body-objs)
		      :multi-vals (multi-vals (last1 body-objs))
		      :flow-ids (flow-ids last-obj))
	   env)))
      (values (make-code-obj (type-spec->type :none) "")
	      env)))

(v-defmacro prog1 (&body body)
  (let ((tmp (free-name 'progn-var)))
    `(let ((,tmp ,(first body)))
       ,@(rest body)
       ,tmp)))


(v-defspecial multiple-value-bind (vars value-form &rest body)
  :args-valid t
  :return
  (let* ((base (string-downcase (string (free-name 'mvb))))
	 (new-env (fresh-environment env :multi-val-base base)))
    (let ((code-obj (varjo->glsl value-form new-env)))
      (unless (= (length vars) (+ 1 (length (multi-vals code-obj))))
        (error 'multi-val-bind-mismatch :val-form value-form :bindings vars))
      (let* ((mvals (multi-vals code-obj))
             (v-vals (mapcar (lambda (_)
                               (multi-val-value _))
                             mvals))
             (types (cons (code-type code-obj)
                          (mapcar #'v-type v-vals))))
        (varjo->glsl
         `(%fresh-env-scope
           (%multi-env-progn
            ,@(loop :for type :in types :for name :in vars
                 :for i :from 0 :collect
                 `(%glsl-let ((,name ,(type->type-spec type))) t
                             ,(format nil "~a~a" base i))))
           ;; the meat
           (setq ,(first vars) ,code-obj)
           ,@body)
         env)))))

(v-defspecial values (&rest values)
  :args-valid t
  :return
  (if (v-multi-val-base env)
      (%values values env)
      (expand->varjo->glsl `(prog1 ,@values) env)))

(defun %values (values env)
  (let* ((new-env (fresh-environment env :multi-val-base nil))
	 (qualifier-lists (mapcar #'extract-value-qualifiers values))
	 (forms (mapcar #'extract-value-form values))

	 (objs (mapcar (lambda (_)
			 (varjo->glsl _ new-env))
		       forms))
	 (base (v-multi-val-base env))
	 (glsl-names (loop :for i :below (length forms) :collect
			(format nil "~a~a" base i)))
	 (vals (loop :for o :in objs :for n :in glsl-names :collect
		  (v-make-value (code-type o) env :glsl-name n
				:flow-ids (flow-ids o))))
	 (first-name (free-name 'v-tmp env))
	 (result (expand->varjo->glsl
		  `(let ((,first-name ,(first objs)))
		     ,@(loop :for o :in (rest objs)
			  :for v :in (rest vals) :collect
			  `(%assign ,v ,o))
		     ,first-name)
		  env)))
    (values (copy-code result
		       :multi-vals (mapcar #'make-mval (rest vals)
					   (rest qualifier-lists)))
	    env)))

;; %assign is only used to set the current-line of the code object
;; it has no side effects on the compilation itself
(v-defspecial %assign ((place v-type) (val v-type))
  :return
  (values
   (merge-obs (list place val) :type (code-type place)
	      :current-line (gen-assignment-string place val)
	      :flow-ids (flow-ids val))
   env))

(defun extract-value-qualifiers (value-form)
  (when (and (listp value-form) (keywordp (first value-form)))
    (butlast value-form)))
(defun extract-value-form (value-form)
  (if (and (listp value-form) (keywordp (first value-form)))
      (last1 value-form)
      value-form))

;;--------------------------------------------------

(v-defspecial return (form)
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
    (let* ((code-obj (varjo->glsl form new-env))
           (result
            (if (member :main (v-context env))
                (%main-return code-obj env)
                (%regular-value-return code-obj))))
      (values result env))))

;; Used when this is a labels (or otherwise local) function
(defun %regular-value-return (code-obj)
  (merge-obs
   code-obj :type 'v-void
   :current-line (format nil "return ~a" (current-line code-obj))
   :returns (cons (code-type code-obj) (multi-vals code-obj))
   :flow-ids (cons (flow-ids code-obj)
		   (mapcar (lambda (c) (flow-ids (multi-val-value c)))
			   (multi-vals code-obj)))))


;; Used when this is the main stage function
;; this
(defun %main-return (code-obj env)
  (if (multi-vals code-obj)
      (let* ((mvals (multi-vals code-obj))
             (v-vals (mapcar (lambda (_)
                               (multi-val-value _))
                             mvals))
             (types (mapcar #'v-type v-vals))
             (glsl-lines (mapcar (lambda (_)
                                   (v-glsl-name _))
                                 v-vals)))
        (varjo->glsl
         `(%fresh-env-scope
           (%multi-env-progn
            ,@(loop :for type :in types :for line :in glsl-lines :collect
                 `(%glsl-let ((,(free-name 'x env) ,(type->type-spec type)))
                             t ,line)))
           ;; the meat
           ,(%default-out-for-stage code-obj env)
           ,@(mapcar (lambda (_)
                       (mval->out-form _ env))
                     (multi-vals code-obj)))
         env))
      (varjo->glsl
         `(%fresh-env-scope
           (%multi-env-progn ,(%default-out-for-stage code-obj env)))
         env)))

;; fragment comes first as it doesnt restrict the exit type...this is a bug
;; really as fragment out-var should be vec4...We should have a case for
;; when context includes all stages, in which case any type is allowed
(defun %default-out-for-stage (form env)
  (let ((context (v-context env)))
    (cond ((member :fragment context) `(%out (,(free-name :output-color env))
                                             ,form))
          ((member :vertex context) `(setq gl-position ,form))
          (t (error "Have not implemented #'values defaults for this stage ~a"
                    env)))))



;;--------------------------------------------------
;; %glsl-let modifies the environment

(v-defspecial %glsl-let (form &optional include-type-declaration arg-glsl-name flow-ids)
  :args-valid t
  :return
  (let* ((var-spec (listify (first form)))
         (glsl-name (or arg-glsl-name
                        (safe-glsl-name-string (free-name (first var-spec) env))))
         (code-obj (when (> (length form) 1) (varjo->glsl (second form) env))))
    (destructuring-bind (name &optional type-spec qualifiers) var-spec
      (declare (ignore qualifiers))
      (let ((type-spec (when type-spec (type-spec->type type-spec))))
        (%validate-var-types name type-spec code-obj)
        (let* ((flow-ids (or flow-ids
			     (when code-obj (flow-ids code-obj))
			     (flow-id!)))
	       (glsl-let-code
                (if code-obj
                    (if (eq include-type-declaration :env-and-set)
                        `(setq (%make-var ,glsl-name
					  ,(or type-spec (code-type code-obj))
					  ,flow-ids)
			       ,code-obj)
                        `(%typify
			  (%make-var ,glsl-name
				     ,(or type-spec (code-type code-obj))
				     ,flow-ids)
			  nil
			  ,code-obj))
                    (if (eq include-type-declaration :env-and-set)
                        `(%make-var ,glsl-name ,type-spec ,(flow-id!))
                        `(%typify  (%make-var ,glsl-name ,type-spec ,(flow-id!))))))
               (let-obj (varjo->glsl glsl-let-code env)))
	  ;; make the resulting code-obj and new environment
          (values (if include-type-declaration
                      (merge-obs let-obj
                                 :type (type-spec->type 'v-none)
                                 :current-line nil
                                 :to-block (append (to-block let-obj)
                                                   (list (current-line
                                                          (end-line let-obj))))
				 :flow-ids flow-ids)
                      (make-code-obj 'v-none ""))
		  ;; finally modify the environment
                  (add-var name
			   (v-make-value
			    (or type-spec (code-type code-obj))
			    env :glsl-name glsl-name :flow-ids flow-ids)
			   env)))))))

;; %make-var is another helper like %assign that only
;; adds data to the code object.
(v-defspecial %make-var (name-string type flow-ids)
  :args-valid t
  :return (values
	   (make-code-obj type name-string :flow-ids flow-ids)
	   env))


;; %typify too only modifies the code object, it is not responsible
;; for changing the environment
(v-defspecial %typify (form &optional qualifiers new-value)
  :args-valid t
  :return
  (let* ((code (varjo->glsl form env))
	 (prefixed-line (prefix-type-declaration code qualifiers))
	 (current-line (if new-value
			   (%gen-assignment-string
			    prefixed-line (current-line new-value))
			   prefixed-line))
	 (flow-ids (if new-value
		       (flow-ids new-value)
		       (flow-ids code))))
    (values (merge-obs code :type (code-type code) :current-line current-line
		       :flow-ids flow-ids)
	    env)))

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

;; %multi-env-progn runs each form one after the other (just like progn)
;; however, unlike progn, each form is evaluated with the same environment
;; this means that bindings in one wont be visable in another. Finally the
;; resulting environement is merged
;;
;; This let's us share this implementation with let,labels,make-function etc
(v-defspecial %multi-env-progn (&rest env-local-expessions)
  :args-valid t
  :return
  (if env-local-expessions
      (let* ((e (mapcar (lambda (_)
                          (multiple-value-list (varjo->glsl _ env)))
                        env-local-expessions))
             (code-objs (mapcar #'first e))
             (env-objs (mapcar #'second e))
             (merged-env (reduce (lambda (_ _1) (merge-env _ _1))
                                 env-objs)))
        (values
         (merge-obs code-objs
                    :type (type-spec->type 'v-none)
                    :current-line nil
                    :to-block (append (mapcan #'to-block code-objs)
                                      (mapcar (lambda (_)
                                                (current-line (end-line _)))
                                              code-objs))
                    :to-top (mapcan #'to-top code-objs)
		    :flow-ids nil)
         merged-env))
      (values (make-code-obj (type-spec->type :none) "")
	      env)))

(v-defmacro let (bindings &body body)
  (unless body (error 'body-block-empty :form-name 'let))
  `(%fresh-env-scope
    (%multi-env-progn
     ,@(loop :for b :in bindings :collect `(%glsl-let ,b t)))
    ,@body))

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

(v-defmacro %make-function (name args &body body)
  `(%%make-function ,name ,args ,body t))

(v-defmacro %make-function-no-implicit (name args &body body)
  `(%%make-function ,name ,args ,body nil))

(v-defspecial %%make-function (name args body allow-implicit-args)
  :args-valid t
  :return
  (let ((deduped-func (dedup-function `(,args ,body) env)))
    (unless (function-raw-args-validp args)
      (error 'bad-make-function-args
             :func-name name
             :arg-specs (remove-if #'function-raw-arg-validp args)))
    (if (and (not allow-implicit-args) deduped-func)
	(values (make-none-ob) (add-function name deduped-func env))
	(%make-new-function name args body allow-implicit-args env))))

(defun %make-new-function (name args body allow-implicit-args env)
  (let* ((mainp (eq name :main))
	 (*v-debug* (not mainp))
	 (env (make-func-env env mainp))
	 (in-arg-flow-ids (mapcar (lambda (_)
				    (declare (ignore _))
				    (flow-id!))
				  args))
	 (arg-glsl-names (loop :for (name) :in args :collect
			    (safe-glsl-name-string (free-name name))))
	 (body-code `(return (progn ,@body)))
	 (body-obj (varjo->glsl `(,(if mainp 'progn '%labels-block-for-main)
				   (%multi-env-progn
				    ,@(loop :for b :in args
					 :for g :in arg-glsl-names
					 :for f :in in-arg-flow-ids
					 :collect `(%glsl-let (,b) nil ,g ,f)))
				   ,body-code) env))
	 (glsl-name (if mainp "main" (safe-glsl-name-string (free-name name))))
	 (primary-return (first (returns body-obj)))
	 (multi-return-vars (rest (returns body-obj)))
	 (type (if mainp (type-spec->type 'v-void) primary-return))
	 (normalized-out-of-scope-args (normalize-out-of-scope-args
					(out-of-scope-args body-obj)))
	 (implicit-args (when allow-implicit-args
			  (remove-if (lambda (_)
				       (= (v-function-scope _)
					  (v-function-scope env)))
				     normalized-out-of-scope-args))))
    (unless allow-implicit-args
      (unless (every (lambda (_) (= (v-function-scope _)
				    (v-function-scope env)))
		     normalized-out-of-scope-args)
	(error 'illegal-implicit-args :func-name name)))
    (unless (or mainp primary-return) (error 'no-function-returns :name name))

    (let* ((arg-pairs (loop :for (ignored type) :in args
			 :for name :in arg-glsl-names :collect
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
	   (func (func-spec->function
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
		  env)))
      (unless allow-implicit-args
	(push-non-implicit-function-for-dedup `(,args ,body) func env))
      (values (merge-obs body-obj
			 :type (type-spec->type 'v-none)
			 :current-line nil
			 :signatures sigs
			 :to-top top
			 :to-block nil
			 :returns nil
			 :out-vars (out-vars body-obj)
			 :multi-vals nil
			 :out-of-scope-args (when allow-implicit-args
					      implicit-args)
			 :flow-ids nil)
	      (add-function
	       name
	       func
	       env)))))

(v-defspecial %labels-block-for-main (&rest body)
  :args-valid t
  :return (let ((new-env (process-environment-for-main-labels env)))
            (varjo->glsl `(progn ,@body) new-env)))

(defun function-raw-args-validp (raw-args)
  (every #'function-raw-arg-validp raw-args))

(defun function-raw-arg-validp (raw-arg)
  (and (listp raw-arg)
       (>= (length raw-arg) 2)
       (not (null (first raw-arg)))
       (symbolp (first raw-arg))
       (not (keywordp (first raw-arg)))
       (type-specp (second raw-arg))))

(v-defmacro :labels (definitions &body body)
  `(%fresh-env-scope
    ,@(loop :for d :in definitions :collect `(%make-function ,@d))
    ,@body))

(v-defmacro labels-no-implicit (definitions &body body)
  `(%fresh-env-scope
    ,@(loop :for d :in definitions :collect `(%make-function-no-implicit ,@d))
    ,@body))

;; {TODO} what if type of form is not value
(v-defspecial %out (name-and-qualifiers form)
  :args-valid t
  :return
  (let* ((form-obj (varjo->glsl form env))
         (out-var-name (if (consp name-and-qualifiers)
                           (first name-and-qualifiers)
                           name-and-qualifiers))
         (qualifiers (when (consp name-and-qualifiers)
                       (rest name-and-qualifiers)))
         (glsl-name (safe-glsl-name-string out-var-name)))
    (if (assoc out-var-name *glsl-variables*)
        (error 'out-var-name-taken out-var-name)
	(values
	 (end-line
	  (merge-obs
	   form-obj :type 'v-none
	   :current-line (gen-out-var-assignment-string glsl-name form-obj)
	   :to-block (to-block form-obj)
	   :out-vars (cons `(,out-var-name
			     ,qualifiers
			     ,(v-make-value (code-type form-obj) env
					    :glsl-name glsl-name))
			   (out-vars form-obj))) t)
	 env))))

(v-defspecial or (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (varjo->glsl x env)) forms)))
    (unless (loop for o in objs always (v-code-type-eq o (first objs)))
      (error "all forms of an 'OR' form must resolve to the same type"))
    (if (v-typep (code-type (first objs)) (type-spec->type :bool))
        (merge-obs objs :type :bool :current-line (gen-bool-or-string objs)
		   :flow-ids (flow-id!))
        (first objs))))

(v-defspecial and (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (varjo->glsl x env)) forms)))
    (unless (loop for o in objs always (v-code-type-eq o (first objs)))
      (error "all forms of an 'AND' form must resolve to the same type"))
    (if (v-typep (code-type (first objs)) (type-spec->type :bool))
        (merge-obs objs :type :bool :current-line (gen-bool-and-string objs)
		   :flow-ids (flow-id!))
        (last1 objs))))

;; note that just like in lisp this only fails if false. 0 does not fail.
;; the then and else statements must have the same type
;; the return type is the type of the then/else form
(v-defspecial if (test-form then-form &optional else-form)
  :args-valid t
  :return
  (vbind (test-obj test-env) (varjo->glsl test-form env)
    (let ((always-true (or (eq test-form t)
			   (not (v-typep (code-type test-obj) 'v-bool)))))

      (vbind (then-obj then-env) (varjo->glsl then-form test-env)
	(if always-true
	    (values (end-line then-obj) then-env)
	    (vbind (else-obj) (if else-form
				  (varjo->glsl else-form test-env)
				  (error 'if-branch-type-mismatch
					 :then-obj then-obj))
	      (assert (v-code-type-eq then-obj else-obj))
	      (let ((result (free-name :result-from-if))
		    (result-type (type->type-spec (code-type then-obj))))
		(expand->varjo->glsl
		 `(let (((,result ,result-type)))
		    (%if ,test-form
			 (setq ,result ,then-form)
			 (setq ,result ,else-form))
		    ,result)
		 env))))))))

;; note that just like in lisp this only fails if false. 0 does not fail.
;; this is the if statement from gl. It has a return type type of :none
;; and allows then and else statements that return different types
(v-defspecial :%if (test-form then-form &optional else-form)
  :args-valid t
  :return
  (vbind (test-obj test-env) (varjo->glsl test-form env)
    (let ((test-mutated-outer-scopes (not (env-same-vars env test-env))))
      (if (v-typep (code-type test-obj) 'v-bool)
	  (compile-regular-%if test-obj test-env then-form else-form)
	  (if test-mutated-outer-scopes
	      (expand->varjo->glsl `(progn ,test-form ,then-form) env)
	      (compile-constant-%if then-form test-env))))))

(defun compile-constant-%if (then-form env)
  ;; the test form was not boolean, so the if would always
  ;; be true, so in this case we just use the 'then' form
  (vbind (then-obj then-env) (varjo->glsl then-form env)
    (values (merge-obs (end-line then-obj) :type :none :flow-ids nil)
	    then-env)))

(defun compile-regular-%if (test-obj test-env then-form else-form)
  (multiple-value-bind (then-obj then-env)
      (varjo->glsl then-form test-env)
    (multiple-value-bind (else-obj else-env)
	(when else-form (varjo->glsl else-form test-env))
      ;; - - - -
      (let ((arg-objs (remove-if #'null (list test-obj then-obj else-obj)))
	    (then-obj (end-line then-obj))
	    (else-obj (end-line else-obj))) ;; returns nil if given nil
	(values (merge-obs arg-objs :type :none :current-line nil
			   :to-block (list (gen-if-string
					    test-obj then-obj else-obj))
			   :flow-ids nil)
		(apply #'env-merge-history
		       (env-prune* (env-depth test-env)
				   then-env
				   else-env)))))))

;; {TODO} check keys
(v-defspecial :switch (test-form &rest clauses)
  :args-valid t
  :return
  (vbind (test-obj test-env) (varjo->glsl test-form env)
    (let* ((keys (mapcar #'first clauses))
	   (clause-pairs (mapcar λ(multiple-value-list
				   (varjo->glsl `(progn ,(second _)) env))
				 clauses))
	   (clause-objs (mapcar #'first clause-pairs)))
      (if (and (v-typep (code-type test-obj) 'v-i-ui)
	       (loop :for key :in keys :always
		  (or (eq key 'default) (integerp key))))
	  (values (merge-obs clause-objs :type 'v-none
			     :current-line nil
			     :to-block (list (gen-switch-string test-obj keys
								clause-objs))
			     :flow-ids nil)
		  (let ((envs (apply #'env-prune* (env-depth test-env)
				     (mapcar #'second clause-pairs))))
		    (reduce #'env-merge-history
			    (rest envs) :initial-value (first envs))))
	  (error 'switch-type-error test-obj keys)))))


;;   (for (a 0) (< a 10) (++ a)
;;     (* a 2))
(v-defspecial :for (var-form condition update &rest body)
  :args-valid t
  :return
  (if (consp (first var-form))
      (error 'for-loop-only-one-var)
      (multiple-value-bind (code new-env)
	  (varjo->glsl `(%glsl-let ,var-form t) env)
        (let* ((var-string (subseq (first (to-block code)) 0 (1- (length (first (to-block code))))))
               (decl-obj (varjo->glsl (second var-form) new-env))
               (condition-obj (varjo->glsl condition new-env))
               (update-obj (varjo->glsl update new-env)))
          (unless (or (typep (code-type decl-obj) 'v-i-ui)
		      (v-typep (code-type decl-obj) 'v-float))
            (error 'invalid-for-loop-type decl-obj))
	  (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body) new-env)
	    (if (and (null (to-block condition-obj)) (null (to-block update-obj)))
		(values (merge-obs
			 body-obj :type 'v-none :current-line nil
			 :to-block `(,(gen-for-loop-string var-string condition-obj
							   update-obj body-obj))
			 :flow-ids (flow-id!))
			final-env)
		(error 'for-loop-simple-expression)))))))


(v-defspecial :while (test &rest body)
  :args-valid t
  :return
  (vbind (test-obj test-env) (varjo->glsl test env)
    (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body) test-env)
      (if (v-typep (code-type test-obj) 'v-bool)
	  (values (merge-obs (list body-obj test-obj)
			     :type 'v-none :current-line nil
			     :to-block (list (gen-while-string test-obj body-obj))
			     :flow-ids nil)
		  final-env)
	  (error 'loop-will-never-halt :test-code test :test-obj test-obj)))))

(defun search-for-flow-id-fixpoint (code starting-env)
  (let ((envs (list starting-env))
	(last-code-obj nil)
	(flow-ids nil))
    (loop :for pass :from 0
       :for current-env = (first envs)
       :until (vbind (o new-env) (varjo->glsl code current-env)
		(let* ((new-flow-ids (get-new-flow-ids new-env current-env))
		       (f-ids (or flow-ids
				  (mapcar λ`(,_ . ,(flow-ids (get-var _ starting-env)))
					  (mapcar #'car new-flow-ids)))))
		  (setf last-code-obj o
			envs (cons new-env envs)
			flow-ids (accumulate-flow-ids f-ids new-flow-ids))
		  (fixpoint-reached new-flow-ids starting-env pass))))
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
					   :stop-at-base t)))
    (mapcar λ`(,_ . ,(flow-ids (get-var _ latest-env)))
	    variables-changed)))

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



(v-defmacro :s~ (&rest args) `(swizzle ,@args))
(v-defspecial :swizzle (vec-form components)
  :args-valid t
  :return
  (let* ((vec-obj (varjo->glsl vec-form env))
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
        (merge-obs vec-obj :type (type-spec->type
                                  (p-symb 'varjo 'v-vec new-len))
                   :current-line (gen-swizzle-string vec-obj comp-string)
		   :flow-ids (flow-id!))
        (error "swizzle form invalid"))))


(v-defspecial the (type-name form)
  :args-valid t
  :return
  (let ((compiled (varjo->glsl form env)))
    (if (stemcellp (code-type compiled))
        (add-type-to-stemcell-code compiled type-name)
        (if (v-typep (code-type compiled)
                     (type-spec->type type-name))
            compiled
            (error "Incorrect declaration that ~a was of type ~a"
                   compiled type-name))))) ;{TODO} proper error here

(v-defspecial %break ()
  :return
  (progn
    (break "Varjo compiler breakpoint" env)
    (make-none-ob)))
