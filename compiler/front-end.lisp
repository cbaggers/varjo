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
                        (make-stage :vertex
                                    (first vertex)
                                    uniforms
                                    (list version)
                                    (rest vertex)
                                    allow-stemcells))
                      (when tesselation-control
                        (make-stage :tesselation-control
                                    (first tesselation-control)
                                    uniforms
                                    (list version)
                                    (rest tesselation-control)
                                    allow-stemcells))
                      (when tesselation-evaluation
                        (make-stage :tesselation-evaluation
                                    (first tesselation-evaluation)
                                    uniforms
                                    (list version)
                                    (rest tesselation-evaluation)
                                    allow-stemcells))
                      (when geometry
                        (make-stage :geometry
                                    (first geometry)
                                    uniforms
                                    (list version)
                                    (rest geometry)
                                    allow-stemcells))
                      (when fragment
                        (make-stage :fragment
                                    (first fragment)
                                    uniforms
                                    (list version)
                                    (rest fragment)
                                    allow-stemcells)))))
    (rolling-translate (remove nil stages))))

(defun v-macroexpand (form env)
  (flow-id-scope (ast->code (compile-form form env))))

;;----------------------------------------------------------------------

(defgeneric copy-stage (stage &key)
  (:method ((stage stage)
            &key (input-variables nil iv-set)
              (uniform-variables nil uv-set)
              (context nil c-set)
              (lisp-code nil lc-set)
              (previous-stage nil ps-set)
              (stemcells-allowed nil sa-set))
    (make-instance
     (type-of stage)
     :input-variables (if iv-set
                          input-variables
                          (input-variables stage))
     :uniform-variables (if uv-set
                            uniform-variables
                            (uniform-variables stage))
     :context (if c-set
                  context
                  (context stage))
     :lisp-code (if lc-set
                    lisp-code
                    (lisp-code stage))
     :previous-stage (if ps-set
                         previous-stage
                         (previous-stage stage))
     :stemcells-allowed (if sa-set
                            stemcells-allowed
                            (stemcells-allowed stage)))))

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
                                 (qualifiers previous))))
           (arg-for-error (x)
             (subseq (to-arg-form x) 0 2))
           (args-for-error (x)
             (mapcar #'arg-for-error x)))
    (if previous-stage
        (let ((out-vars (transform-previous-stage-out-vars previous-stage
                                                           stage))
              (in-vars (input-variables stage)))
          (copy-stage
           stage
           :previous-stage previous-stage
           :input-variables
           (if (and (in-args-compatiblep in-vars out-vars)
                    (uniforms-compatiblep
                     (uniform-variables stage)
                     (uniform-variables previous-stage))
                    (context-compatiblep stage previous-stage))
               (mapcar #'merge-in-arg out-vars in-vars)
               (error 'args-incompatible
                      :current-args (args-for-error in-vars)
                      :previous-args (args-for-error out-vars)))))
        stage)))


(defun splice-in-precompiled-stage (last-stage stage remaining-stage-types
                                    accum)
  (let ((out-vars
         (if (stage-is last-stage :vertex)
             (rest (out-vars last-stage))
             (out-vars last-stage))))
    (labels ((gen-aliases ()
               (let ((in-args (input-variables stage)))
                 ;; :for (nil . out-rest) :in out-vars
                 ;; :for (in-name type . in-rest) :in in-args
                 (loop :for out-var :in out-vars
                    :for in-var :in in-args :append
                    (let ((out-glsl-name (glsl-name out-var))
                          (in-glsl-name (subseq (glsl-name in-var) 1)))
                      (when (not (equal out-glsl-name in-glsl-name))
                        (flow-id-scope
                          (to-block
                           (let ((env (%make-base-environment stage)))
                             (glsl-let
                              (name in-var) in-glsl-name (v-type-of in-var)
                              (compile-glsl-expression-string
                               out-glsl-name (v-type-of in-var) env nil)
                              env)))))))))
             (swap-out-args (glsl-string)
               (let ((in-args (input-variables stage)))
                 (loop :for out :in out-vars
                    :for in :in in-args
                    :for out-glsl-name := (glsl-name out)
                    :for in-glsl-name := (subseq (glsl-name in) 1)
                    :do
                    (setf glsl-string
                          (ppcre:regex-replace (format nil "@~a" in-glsl-name)
                                               glsl-string out-glsl-name))))
               glsl-string)
             (swap-in-block (glsl-string)
               (ppcre:regex-replace "_IN_BLOCK_" glsl-string
                                    (block-name-string
                                     (out-block-name-for last-stage)))))
      (let ((in-args (input-variables stage))
            (out-vars (transform-previous-stage-out-vars last-stage stage)))
        (assert (in-args-compatiblep in-args out-vars))
        (assert (uniforms-compatiblep (uniform-variables stage)
                                      (uniform-variables last-stage)))
        (assert (context-compatiblep stage last-stage))
        ;; we need to modify the result of the compiled stage if the in-args names
        ;; dont match the names of the out args
        (let* ((glsl-aliases (gen-aliases))
               (glsl-code (glsl-code stage))
               (glsl-code (swap-out-args glsl-code))
               (glsl-code (swap-in-block glsl-code))
               (final-glsl-code (ppcre:regex-replace
                                 "void main" glsl-code
                                 (format nil "~{~a~}~%~%void main"
                                         glsl-aliases)))
               (new-compile-result
                (clone-compile-result stage :glsl-code final-glsl-code)))
          (with-slots (compiled-stages) accum
            (make-instance 'rolling-result
                           :compiled-stages (cons new-compile-result
                                                  compiled-stages)
                           :remaining-stages remaining-stage-types)))))))

;;------------------------------------------------------------

(defmethod extract-stage-type ((stage vertex-stage))
  :vertex)

(defmethod extract-stage-type ((stage tesselation-control-stage))
  :tesselation-control)

(defmethod extract-stage-type ((stage tesselation-evaluation-stage))
  :tesselation-evaluation)

(defmethod extract-stage-type ((stage geometry-stage))
  :geometry)

(defmethod extract-stage-type ((stage fragment-stage))
  :fragment)

(defmethod extract-stage-type ((env environment))
  (extract-stage-type (stage env)))

(defmethod extract-stage-type ((ppp post-compile-process))
  (extract-stage-type (stage ppp)))

;;------------------------------------------------------------

(defmethod stage-is ((stage stage) name)
  (typep stage (stage-kind-to-type name)))

(defmethod stage-is ((env environment) name)
  (stage-is (stage env) name))

(defmethod stage-is ((ppp post-compile-process) name)
  (stage-is (stage ppp) name))

;;------------------------------------------------------------

(defgeneric in-args-compatiblep (in-args last-out-vars)
  ;;
  (:method ((in-args list) (last-out-vars list))
    (and (= (length in-args) (length last-out-vars)))
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
    (context-ok-given-restriction (context previous-stage) (context stage))))

;;----------------------------------------------------------------------

(defgeneric transform-previous-stage-out-vars (stage next-stage)
  (:method (stage next-stage)
    (transform-arg-types (extract-stage-type stage)
                         (extract-stage-type next-stage)
                         stage)))

(defun primitive-size (stage)
  (error "OH jesus, lord help me jesus, it's all fucked up the Lord jesus mercy jesus lord~%~a"
         stage))

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
                                           (primitive-size stage)))
              :qualifiers (qualifiers _))
            (rest (out-vars stage))))
  (:method ((last (eql :vertex))
            next
            (stage stage))
    (rest (out-vars stage)))
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
