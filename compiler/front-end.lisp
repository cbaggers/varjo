(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------


(defun v-compile (uniforms version
                  &key vertex tesselation-control tesselation-evaluation
                    geometry fragment)
  "
This function takes lisp code as lists and returns the results of compiling that
code to glsl.

Each result is an object of type 'varjo-compile-result.

The stages must be defined in the following way.

- The first element of the list is the input args to the stage as pairs of
  names and types.
- The rest of the list is the body code of that stage.

Example:

    (v-compile '((a :float)) :330
               :vertex '(((pos :vec3))
                         (values (v! pos 1.0) a))
               :fragment '(((hmm :float))
                           (labels ((fun ((x :float))
                                      (* x x)))
                             (v! 1.0 1.0 hmm (fun a)))))
"
  (let ((stages (list (when vertex
                        (list (first vertex)
                              uniforms
                              (list  :vertex version)
                              `(progn ,@(rest vertex))))
                      (when tesselation-control
                        (list (first tesselation-control)
                              uniforms
                              (list :tesselation-control version)
                              `(progn ,@(rest tesselation-control))))
                      (when tesselation-evaluation
                        (list (first tesselation-evaluation)
                              uniforms
                              (list :tesselation-evaluation version)
                              `(progn ,@(rest tesselation-evaluation))))
                      (when geometry
                        (list (first geometry)
                              uniforms
                              (list :geometry version)
                              `(progn ,@(rest geometry))))
                      (when fragment
                        (list (first fragment)
                              uniforms
                              (list :fragment version)
                              `(progn ,@(rest fragment)))))))
    (rolling-translate (remove nil stages))))

(defun v-macroexpand (form &optional (env (%make-base-environment)))
  (identity
   (pipe-> (form env)
     (equalp #'symbol-macroexpand-pass
             #'macroexpand-pass
             #'compiler-macroexpand-pass))))

;;----------------------------------------------------------------------

(defmacro with-stage ((&optional (in-args (symb :in-args))
                                 (uniforms (symb :uniforms))
                                 (context (symb :context))
                                 (code (symb :code))
				 (tp-meta (symb :tp-meta)))
                         stage &body body)
  `(destructuring-bind (,in-args ,uniforms ,context ,code &optional ,tp-meta)
       ,stage
     (declare (ignorable ,in-args ,uniforms ,context ,code ,tp-meta))
     ,@body))

(defclass rolling-result ()
  ((remaining-stages :initform *stage-types* :initarg :remaining-stages)
   (compiled-stages :initform nil :initarg :compiled-stages)))

(defun rolling-translate (stages &optional (compile-func #'translate))
  (let ((result (reduce λ(compile-stage _ _1 compile-func)
                        stages :initial-value (make-instance 'rolling-result))))
    (reverse (slot-value result 'compiled-stages))))

(defun merge-in-previous-stage-args (previous-stage stage)
  (declare (optimize debug))
  (if previous-stage
      (let ((out-vars (transform-previous-stage-out-vars previous-stage stage)))
	(with-stage () stage

	  (list (if (and (in-args-compatiblep in-args out-vars)
			 (uniforms-compatiblep
			  uniforms (uniforms previous-stage))
			 (context-compatiblep stage previous-stage))
		    (mapcar #'%merge-in-arg
			    out-vars
			    in-args)
		    (error 'args-incompatible
			   in-args (out-vars previous-stage)))
		uniforms
		context
		code
		tp-meta)))
      (with-stage () stage
        (list in-args
	      uniforms
	      context
	      code
	      (or tp-meta (make-hash-table))))))


(defun %merge-in-arg (previous current)
  (with-v-arg (c-name c-type c-qual c-glsl-name) current
    (with-v-arg (p-name p-type p-qual p-glsl-name) previous
      `(,c-name
        ,(or c-type p-type)
        ,@(union c-qual p-qual)
        ,(or p-glsl-name c-glsl-name)))))

(defun splice-in-precompiled-stage (last-stage stage remaining-stage-types
				    accum)
  (labels ((gen-aliases ()
	     (loop :for (out-name . out-rest) :in (out-vars last-stage)
		:for (in-name type . in-rest) :in (in-args stage) :append
		(let ((out-glsl-name (last1 out-rest))
		      (in-glsl-name (subseq (last1 in-rest) 1)))
		  (when (not (equal out-glsl-name in-glsl-name))
		    (flow-id-scope
		      (to-block
		       (glsl-let in-name in-glsl-name type
				 (compile-glsl-expression-string out-glsl-name type)
				 (%make-base-environment))))))))
	   (swap-out-args (glsl-string)
	     (loop :for out :in (out-vars last-stage)
		:for in :in (in-args stage)
		:for out-glsl-name := (last1 out)
		:for in-glsl-name := (subseq (last1 in) 1) :do
		(setf glsl-string
		      (ppcre:regex-replace (format nil "@~a" in-glsl-name)
					   glsl-string out-glsl-name)))
	     glsl-string))

    (let ((out-vars (transform-previous-stage-out-vars last-stage stage)))
      (when (and (in-args-compatiblep (in-args stage) out-vars)
		 (uniforms-compatiblep (uniforms stage)
				       (uniforms last-stage))
		 (context-compatiblep (context stage)
				      (context last-stage)))
	;; we need to modify the result of the compiled stage if the in-args names
	;; dont match the names of the out args
	(let* ((glsl-aliases (gen-aliases))
	       (glsl-code-0 (glsl-code stage))
	       (glsl-code-1 (swap-out-args glsl-code-0))
	       (final-glsl-code (ppcre:regex-replace
				 "void main"
				 glsl-code-1
				 (format nil "~{~a~}~%~%void main"
					 glsl-aliases)))
	       (new-compile-result
		(clone-compile-result stage :glsl-code final-glsl-code)))
	  (cons (list new-compile-result remaining-stage-types)
		(cons last-stage (cddr accum))))))))

(defun compile-stage (accum stage compile-func)
  (with-slots (remaining-stages compiled-stages) accum
    (let* ((last-stage (first compiled-stages))
           (remaining-stages (check-order (extract-stage-type stage)
                                          remaining-stages)))
      (if (typep stage 'varjo-compile-result)
          (splice-in-precompiled-stage
           last-stage stage remaining-stages accum )
          (let ((new-compile-result
                 (apply compile-func (merge-in-previous-stage-args last-stage
                                                                   stage))))
            (make-instance 'rolling-result
                           :compiled-stages (cons new-compile-result
                                                  compiled-stages)
                           :remaining-stages remaining-stages))))))

(defgeneric extract-stage-type (stage))

(defmethod extract-stage-type ((stage list))
  (let ((context (with-stage () stage context)))
    (find-if λ(when (member _ context) _)
             *stage-types*)))

(defmethod extract-stage-type ((stage varjo-compile-result))
  (let ((context (context stage)))
    (find-if λ(when (member _ context) _)
             *stage-types*)))

(defgeneric args-compatiblep (stage previous-stage))
(defgeneric in-args-compatiblep (in-args last-out-vars))
(defgeneric in-args-compatible-for-stage-p
    (in-args stage last-out-vars last-stage))
(defgeneric uniforms-compatiblep (uniforms last-uniforms))
(defgeneric context-compatiblep (stage previous-stage))

(defmethod in-args-compatible-for-stage-p
    ((in-args list) (stage (eql :fragment))
     (last-out-vars list) (last-stage (eql :geometry)))
  (declare (ignore in-args last-out-vars))
  (warn "Varjo: Cannot currently check the validity of type passed from geometry stages to fragment stages.")
  t)

(defmethod in-args-compatible-for-stage-p
    ((in-args list) (stage (eql :tesselation-control))
     (last-out-vars list) (last-stage (eql :vertex)))
  (declare (ignore in-args last-out-vars))
  (warn "Varjo: Cannot currently check the validity of type passed from vertex stages to tesselation-control stages.")
  t)

(defmethod in-args-compatible-for-stage-p
    ((in-args list) (stage (eql :tesselation-evaluation))
     (last-out-vars list) (last-stage (eql :tesselation-control)))
  (declare (ignore in-args last-out-vars))
  (warn "Varjo: Cannot currently check the validity of type passed from tesselation-control to tesselation-evaluation stages.")
  t)

(defmethod in-args-compatible-for-stage-p
    ((in-args list) (stage symbol) (last-out-vars list) (last-stage symbol))
  (declare (ignore stage last-stage))
  (in-args-compatiblep in-args last-out-vars))

(defmethod in-args-compatiblep ((in-args list) (last-out-vars list))
  (loop :for p :in last-out-vars
     :for c :in in-args :always
     (and (v-type-eq (type-spec->type (second p))
		     (type-spec->type (second c)))
	  (%suitable-qualifiersp p c))))

(defmethod uniforms-compatiblep ((uniforms list) (last-uniforms list))
  (loop :for u :in last-uniforms :always
     (let ((match (find (first u) uniforms :key #'first)))
       (if match
	   (v-type-eq (type-spec->type (second u))
		      (type-spec->type (second match)))
	   t))))

(defmethod context-compatiblep ((stage list)
				(previous-stage varjo-compile-result))
  (with-stage () stage
    (context-ok-given-restriction
     (remove (extract-stage-type previous-stage) (context previous-stage))
     (remove (extract-stage-type stage) context))))


(defun in-arg-qualifiers (in-arg)
  (with-v-arg (_ _1 q) in-arg
    q))

(defun %suitable-qualifiersp (prev-stage-in-arg in-arg)
  (let ((pq (in-arg-qualifiers prev-stage-in-arg))
        (cq (in-arg-qualifiers in-arg)))
    (every (lambda (_)
             (member _ pq))
           cq)))

;;----------------------------------------------------------------------

(defgeneric transform-arg-types (last next stage))

(defmethod transform-arg-types ((last (eql :vertex)) (next (eql :geometry))
				stage)
  (declare (optimize debug))
  (mapcar λ(with-v-arg (name type qualifiers glsl-name) _
	     `(,name (,type *) ,@qualifiers
		     ,@(when glsl-name (list glsl-name))))
	  (out-vars stage)))

(defmethod transform-arg-types (last next (stage list))
  (declare (optimize debug))
  (with-stage () stage
    (out-vars stage)))

(defmethod transform-arg-types (last next (stage varjo-compile-result))
  (declare (optimize debug))
  (out-vars stage))

(defmethod transform-previous-stage-out-vars (stage next-stage)
  (declare (optimize debug))
  (transform-arg-types (extract-stage-type stage)
		       (extract-stage-type next-stage)
		       stage))

;;----------------------------------------------------------------------

(defun check-order (stage-type remaining-stage-types)
  (let ((check (member stage-type remaining-stage-types)))
    (if check
        (rest check)
        (error 'stage-order-error :stage-type stage-type))))
