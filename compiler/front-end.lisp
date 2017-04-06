(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------


(defun v-compile (uniforms version
                  &key vertex tesselation-control tesselation-evaluation
                    geometry fragment allow-stemcells)
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
                        (make-stage (first vertex)
                                    uniforms
                                    (list  :vertex version)
                                    (rest vertex)
                                    allow-stemcells))
                      (when tesselation-control
                        (make-stage (first tesselation-control)
                                    uniforms
                                    (list :tesselation-control version)
                                    (rest tesselation-control)
                                    allow-stemcells))
                      (when tesselation-evaluation
                        (make-stage (first tesselation-evaluation)
                                    uniforms
                                    (list :tesselation-evaluation version)
                                    (rest tesselation-evaluation)
                                    allow-stemcells))
                      (when geometry
                        (make-stage (first geometry)
                                    uniforms
                                    (list :geometry version)
                                    (rest geometry)
                                    allow-stemcells))
                      (when fragment
                        (make-stage (first fragment)
                                    uniforms
                                    (list :fragment version)
                                    (rest fragment)
                                    allow-stemcells)))))
    (rolling-translate (remove nil stages))))

(defun v-macroexpand (form &optional (env (%make-base-environment)))
  (flow-id-scope (ast->code (compile-form form env))))

;;----------------------------------------------------------------------

(defun rolling-translate (stages &optional (compile-func #'translate))
  (let ((result (reduce λ(compile-stage _ _1 compile-func) stages
                        :initial-value (make-instance 'rolling-result))))
    (reverse (slot-value result 'compiled-stages))))

(defun compile-stage (accum stage compile-func)
  ;;(break "foo ~a" stage)
  (with-slots (remaining-stages compiled-stages) accum
    (let* ((last-stage (first compiled-stages))
           (remaining-stages (check-order (extract-stage-type stage)
                                          remaining-stages)))
      (if (typep stage 'varjo-compile-result)
          (splice-in-precompiled-stage
           last-stage stage remaining-stages accum )
          (let* ((merged-stage (merge-in-previous-stage-args last-stage stage))
                 (new-compile-result (funcall compile-func merged-stage)))
            (make-instance 'rolling-result
                           :compiled-stages (cons new-compile-result
                                                  compiled-stages)
                           :remaining-stages remaining-stages))))))

(defun merge-in-previous-stage-args (previous-stage stage)
  (declare (optimize debug))
  (labels ((merge-in-arg (previous current)
             (make-instance
              'input-variable
              :name (name current)
              :glsl-name (or (glsl-name previous) (glsl-name current))
              :type (or (v-type-of current) (v-type-of previous))
              :qualifiers (union (qualifiers current)
                                 (qualifiers previous)))))
    (if previous-stage
        (let ((out-vars (transform-previous-stage-out-vars previous-stage
                                                           stage))
              (in-vars (input-variables stage)))
          (make-instance
           'stage
           :input-variables
           (if (and (in-args-compatiblep in-vars out-vars)
                    (uniforms-compatiblep
                     (uniform-variables stage)
                     (uniform-variables previous-stage))
                    (context-compatiblep stage previous-stage))
               (mapcar #'merge-in-arg out-vars in-vars)
               (error 'args-incompatible
                      :current-args (mapcar λ(subseq _ 0 2)
                                            (mapcar #'to-arg-form in-vars))
                      :previous-args (mapcar λ(subseq _ 0 2)
                                             (mapcar #'to-arg-form out-vars))))
           :uniform-variables (uniform-variables stage)
           :context (context stage)
           :lisp-code (lisp-code stage)
           :previous-stage previous-stage
           :stemcells-allowed (stemcells-allowed stage)))
        stage)))


(defun splice-in-precompiled-stage (last-stage stage remaining-stage-types
                                    accum)
  (labels ((gen-aliases ()
             (let ((in-args (input-variables stage))
                   (out-vars (out-vars last-stage)))
               ;; :for (nil . out-rest) :in out-vars
               ;; :for (in-name type . in-rest) :in in-args
               (loop :for out-var :in out-vars
                  :for in-var :in in-args :append
                  (let ((out-glsl-name (glsl-name out-var))
                        (in-glsl-name (subseq (glsl-name in-var) 1)))
                    (when (not (equal out-glsl-name in-glsl-name))
                      (flow-id-scope
                        (to-block
                         (glsl-let (name in-var) in-glsl-name (v-type-of in-var)
                                   (compile-glsl-expression-string
                                    out-glsl-name (v-type-of in-var))
                                   (%make-base-environment)))))))))
           (swap-out-args (glsl-string)
             (let ((in-args (input-variables stage))
                   (out-vars (out-vars last-stage)))
               (loop :for out :in out-vars
                  :for in :in in-args
                  :for out-glsl-name := (glsl-name out)
                  :for in-glsl-name := (subseq (glsl-name in) 1)
                  :do
                  (setf glsl-string
                        (ppcre:regex-replace (format nil "@~a" in-glsl-name)
                                             glsl-string out-glsl-name))))
             glsl-string))

    (let ((in-args (input-variables stage))
          (out-vars (transform-previous-stage-out-vars last-stage stage)))
      (assert (in-args-compatiblep in-args out-vars))
      (assert (uniforms-compatiblep (uniform-variables stage)
                                    (uniform-variables last-stage)))
      (assert (context-compatiblep stage last-stage))
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
        (with-slots (compiled-stages) accum
          (make-instance 'rolling-result
                         :compiled-stages (cons new-compile-result
                                                compiled-stages)
                         :remaining-stages remaining-stage-types))))))

(defgeneric extract-stage-type (stage)
  (:method ((stage stage))
    (let ((context (context stage)))
      (find-if λ(when (member _ context) _)
               *stage-types*)))
  (:method ((ppp post-compile-process))
    (extract-stage-type (stage ppp))))

(defgeneric args-compatiblep (stage previous-stage))

(defgeneric in-args-compatible-for-stage-p
    (in-args stage last-out-vars last-stage))

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


(defgeneric in-args-compatiblep (in-args last-out-vars)
  ;;
  (:method ((in-args list) (last-out-vars list))
    (every (lambda (out in)
             (and (v-type-eq (v-type-of out) (v-type-of in))
                  (%suitable-qualifiersp out in)))
           last-out-vars in-args)))

(defun %suitable-qualifiersp (out-arg in-arg)
  (let ((out-qual (qualifiers out-arg)))
    (every λ(member _ out-qual) (qualifiers in-arg))))


(defgeneric uniforms-compatiblep (uniforms last-uniforms)
  ;;
  (:method ((uniforms list) (last-uniforms list))
    (loop :for u :in last-uniforms :always
       (let ((match (find (name u) uniforms :key #'name)))
         (if match
             (v-type-eq (v-type-of u) (v-type-of match))
             t)))))

(defgeneric context-compatiblep (stage previous-stage)
  (:method ((stage stage) (previous-stage stage))
    (context-ok-given-restriction
     (remove (extract-stage-type previous-stage) (context previous-stage))
     (remove (extract-stage-type stage) (context stage)))))

;;----------------------------------------------------------------------

(defgeneric transform-previous-stage-out-vars (stage next-stage)
  (:method (stage next-stage)
    (transform-arg-types (extract-stage-type stage)
                         (extract-stage-type next-stage)
                         stage)))

(defgeneric transform-arg-types (last next stage)
  ;;
  (:method ((last (eql :vertex))
            (next (eql :geometry))
            (stage stage))
    (declare (optimize debug))
    (mapcar λ(make-instance
              'output-variable
              :name (name _)
              :glsl-name (glsl-name _)
              :type (type-spec->type (list (type->type-spec (v-type-of _))
                                           '*))
              :qualifiers (qualifiers _))
            (out-vars stage)))
  ;;
  (:method (last next (stage stage))
    (declare (ignore last next))
    (out-vars stage)))

;;----------------------------------------------------------------------

(defun check-order (stage-type remaining-stage-types)
  (let ((check (member stage-type remaining-stage-types)))
    (if check
        (rest check)
        (error 'stage-order-error :stage-type stage-type))))

;;----------------------------------------------------------------------

(defmethod to-arg-form ((uniform uniform-variable))
  `(,(name uniform)
     ,(type->type-spec (v-type-of uniform))
     ,@(qualifiers uniform)))

(defmethod to-arg-form ((in-var input-variable))
  `(,(name in-var)
    ,(type->type-spec (v-type-of in-var))
     ,@(qualifiers in-var)
     ,@(when (glsl-name in-var) (list (glsl-name in-var)))))

(defmethod to-arg-form ((out-var output-variable))
  `(,(name out-var)
     ,(type->type-spec (v-type-of out-var))
     ,@(qualifiers out-var)
     ,@(when (glsl-name out-var) (list (glsl-name out-var)))))

;;----------------------------------------------------------------------

(defmacro with-v-arg ((&optional (name (gensym "name")) (type (gensym "type"))
                                 (qualifiers (gensym "qualifiers"))
                                 (glsl-name (gensym "glsl-name")))
                         arg-form &body body)
  (let ((qn (gensym "qn")))
    `(destructuring-bind (,name ,type . ,qn) ,arg-form
       (declare (ignorable ,name ,type))
       (let* ((,glsl-name (when (stringp (last1 ,qn)) (last1 ,qn)))
              (,qualifiers (if ,glsl-name (butlast ,qn) ,qn)))
         (declare (ignorable ,qualifiers ,glsl-name))
         ,@body))))
