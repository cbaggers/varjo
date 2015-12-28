(in-package :varjo)
(in-readtable fn:fn-reader)

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
