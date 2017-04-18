(in-package :varjo)
(in-readtable fn:fn-reader)

(defun rolling-translate (stages &optional (compile-func #'translate))
  (labels ((valid-for-rt (x) (typep x 'stage)))
    (assert (every #'valid-for-rt stages)
            () 'rolling-translate-invalid-stage
            :invalid (remove-if #'valid-for-rt stages)))
  ;;
  (let ((result (reduce λ(compile-stage _ _1 compile-func) stages
                        :initial-value (make-instance 'rolling-result))))
    (reverse (slot-value result 'compiled-stages))))

(defun compile-stage (accum stage compile-func)
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
        (vbind (out-vars primitive-kind)
            (transform-previous-stage-out-data previous-stage stage)
          (let ((in-vars (input-variables stage)))
            (copy-stage
             stage
             :previous-stage previous-stage
             :input-variables
             (if (and (in-args-compatiblep in-vars out-vars)
                      (uniforms-compatiblep (uniform-variables stage)
                                            (uniform-variables previous-stage))
                      (context-compatiblep stage previous-stage))
                 (mapcar #'merge-in-arg out-vars in-vars)
                 (error 'args-incompatible
                        :current-args (args-for-error in-vars)
                        :previous-args (args-for-error out-vars)))
             :primitive primitive-kind)))
        stage)))

(defun splice-in-precompiled-stage (last-stage stage remaining-stage-types
                                    accum)
  (vbind (out-vars primitive)
      (transform-previous-stage-out-data last-stage stage)
    (let ((out-vars
           (if (stage-is last-stage :vertex)
               (rest out-vars)
               out-vars)))
      (labels ((swap-out-arg (glsl-string old-name new-name)
                 (let* (;; regex matches the name and the surrounding
                        ;; characters that are not alphanumeric or underscores
                        (regex (format nil "[^\w_]~a[^\w_]" old-name))
                        (matches (group (ppcre:all-matches regex glsl-string)
                                        2))
                        (result "")
                        (last 0))
                   ;; the 1+ and 1- below compensate for the overcapture of the
                   ;; regex
                   (loop :for (start end) :in matches
                      :for chunk := (subseq glsl-string last (1+ start)) :do
                      (setf result (concatenate 'string result chunk new-name))
                      (setf last (1- end)))
                   (concatenate 'string result (subseq glsl-string last))))
               ;;
               (swap-out-args (glsl-string)
                 (let ((in-args (input-variables stage)))
                   (loop :for out :in out-vars
                      :for in :in in-args
                      :for out-glsl-name := (glsl-name out)
                      :for in-glsl-name := (glsl-name in)
                      :do (setf glsl-string (swap-out-arg glsl-string
                                                          in-glsl-name
                                                          out-glsl-name))))
                 glsl-string)
               ;;
               (swap-in-block (glsl-string)
                 (let ((block-name (block-name-string *fallback-block-name*)))
                   (ppcre:regex-replace block-name glsl-string
                                        (block-name-string
                                         (out-block-name-for last-stage)))))
               ;;
               (arg-for-error (x)
                 (subseq (to-arg-form x) 0 2))
               (args-for-error (x)
                 (mapcar #'arg-for-error x)))
        ;;
        (let ((in-args (input-variables stage))
              (out-prim (type-of primitive))
              (in-prim (type-of (primitive-in stage))))
          (assert (in-args-compatiblep in-args out-vars) ()
                  'args-incompatible
                  :current-args (args-for-error in-args)
                  :previous-args (args-for-error out-vars))
          (assert (uniforms-compatiblep (uniform-variables stage)
                                        (uniform-variables last-stage)))
          (assert (context-compatiblep stage last-stage))
          (when (stage-is stage :geometry)
            (assert (eq in-prim out-prim) () 'primitives-dont-match
                    :out-stage (stage-type last-stage) :out out-prim
                    :in-stage (type-of stage) :in in-prim)))
        ;; we need to modify the result of the compiled stage if the in-args
        ;; names dont match the names of the out args
        (let* ((glsl-code (glsl-code stage))
               (glsl-code (swap-out-args glsl-code))
               (glsl-code (swap-in-block glsl-code))
               (final-glsl-code glsl-code)
               (new-compile-result
                (clone-compile-result stage :glsl-code final-glsl-code)))
          (with-slots (compiled-stages) accum
            (make-instance 'rolling-result
                           :compiled-stages (cons new-compile-result
                                                  compiled-stages)
                           :remaining-stages remaining-stage-types)))))))

;;----------------------------------------------------------------------

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

(defgeneric transform-previous-stage-out-data (stage next-stage)
  (:method ((stage varjo-compile-result) next-stage)
    (let ((next-primitive (compute-next-primitive stage next-stage)))
      (values (transform-arg-types (extract-stage-type stage)
                                   (extract-stage-type next-stage)
                                   stage
                                   next-primitive)
              next-primitive))))

(defun compute-next-primitive (compiled-stage next-stage)
  (let ((primitive (primitive-out compiled-stage))
        (stage (stage-type compiled-stage))
        (next-stage (extract-stage-type next-stage)))
    (%compute-next-primitive primitive stage next-stage)))

(defgeneric %compute-next-primitive (primitive stage next-stage)
  (:method (primitive
            (stage (eql :vertex))
            (next-stage (eql :geometry)))
    (assert (or (typep primitive 'draw-mode)
                (typep primitive 'geometry-primitive)))
    (make-instance
     (typecase primitive
       (points :points)
       ((or lines line-loop line-strip) 'lines)
       ((or lines-adjacency line-strip-adjacency) 'lines-adjacency)
       ((or triangles triangle-fan triangle-strip) 'triangles)
       ((or triangles-adjacency triangle-strip-adjacency) 'triangles-adjacency)
       (t (error 'couldnt-convert-primitive-for-geometry-stage
                 :prim (type-of primitive)
                 :prev-stage stage)))))

  (:method (primitive
            (stage (eql :vertex))
            (next-stage (eql :tesselation-control)))
    (error "IMPLEMENT ME!"))

  (:method (primitive
            (stage (eql :vertex))
            (next-stage (eql :tesselation-evaluation)))
    (error "IMPLEMENT ME!"))

  (:method (primitive
            stage
            (next-stage (eql :fragment)))
    nil))

(defgeneric transform-arg-types (last next stage primitive)
  (:method ((last (eql :vertex))
            (next (eql :geometry))
            (stage stage)
            primitive)
    (declare (optimize debug))
    (mapcar λ(make-instance
              'output-variable
              :name (name _)
              :glsl-name (glsl-name _)
              :type (type-spec->type (list (type->type-spec (v-type-of _))
                                           (vertex-count primitive)))
              :qualifiers (qualifiers _))
            (rest (out-vars stage))))

  (:method ((last (eql :vertex)) next (stage stage) primitive)
    (declare (ignore last next primitive))
    (rest (out-vars stage)))

  (:method (last next (stage stage) primitive)
    (declare (ignore last next primitive))
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
