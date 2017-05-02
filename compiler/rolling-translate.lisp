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
           (remaining-stages (check-order (type-of stage)
                                          remaining-stages)))
      (if (typep stage 'compiled-stage)
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
        (vbind (output-variables primitive-kind)
            (transform-previous-stage-out-data previous-stage stage)
          (let ((in-vars (input-variables stage)))
            (copy-stage
             stage
             :previous-stage previous-stage
             :input-variables
             (if (and (input-variables-compatiblep in-vars output-variables)
                      (uniforms-compatiblep (uniform-variables stage)
                                            (uniform-variables previous-stage))
                      (context-compatiblep stage previous-stage))
                 (mapcar #'merge-in-arg output-variables in-vars)
                 (error 'args-incompatible
                        :current-args (args-for-error in-vars)
                        :previous-args (args-for-error output-variables)))
             :primitive-in primitive-kind)))
        stage)))

(defun splice-in-precompiled-stage (last-stage stage remaining-stage-types
                                    accum)
  (vbind (output-variables primitive)
      (transform-previous-stage-out-data last-stage stage)
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
               (let ((input-variables (input-variables stage)))
                 (loop :for out :in output-variables
                    :for in :in input-variables
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
      (let ((input-variables (input-variables stage))
            (out-prim (type-of primitive))
            (in-prim (type-of (primitive-in stage))))
        (assert (input-variables-compatiblep input-variables output-variables)
                () 'args-incompatible
                :current-args (args-for-error input-variables)
                :previous-args (args-for-error output-variables))
        (assert (uniforms-compatiblep (uniform-variables stage)
                                      (uniform-variables last-stage)))
        (assert (context-compatiblep stage last-stage))
        (when (typep stage 'geometry-stage)
          (assert (eq in-prim out-prim) () 'primitives-dont-match
                  :out-stage (type-of last-stage) :out out-prim
                  :in-stage (type-of stage) :in in-prim)))
      ;; we need to modify the result of the compiled stage if the
      ;; input-variables names dont match the names of the out args
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
                         :remaining-stages remaining-stage-types))))))

;;----------------------------------------------------------------------

(defgeneric input-variables-compatiblep (input-variables last-output-variables)
  ;;
  (:method ((input-variables list) (last-output-variables list))
    (and (= (length input-variables) (length last-output-variables)))
    (every (lambda (out in)
             (and (v-type-eq (v-type-of out) (v-type-of in))
                  (%suitable-qualifiersp out in)))
           last-output-variables input-variables)))

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
  (:method ((compile-result compiled-stage) next-stage)
    (let ((next-primitive (compute-next-primitive compile-result next-stage)))
      (values (transform-arg-types (starting-stage compile-result)
                                   next-stage
                                   compile-result
                                   next-primitive)
              next-primitive))))

(defun compute-next-primitive (compiled-stage next-stage)
  (let ((primitive (primitive-out compiled-stage))
        (stage (starting-stage compiled-stage)))
    (unless (typep next-stage 'fragment-stage)
      (%compute-next-primitive primitive stage next-stage))))

(defgeneric %compute-next-primitive (primitive stage next-stage)
  (:method (primitive
            (stage vertex-stage)
            (next-stage geometry-stage))
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
            (stage vertex-stage)
            (next-stage tessellation-control-stage))
    (assert (typep primitive 'patches) ()
            'tessellation-control-expects-patches
            :primitive primitive)
    primitive)

  (:method (primitive
            (stage vertex-stage)
            (next-stage tessellation-evaluation-stage))
    (assert (typep primitive 'patches) ()
            'tessellation-control-expects-patches
            :primitive primitive)
    primitive)

  (:method (primitive
            (stage tessellation-control-stage)
            (next-stage tessellation-evaluation-stage))
    (declare (ignore stage next-stage))
    primitive)

  (:method (primitive
            (stage tessellation-evaluation-stage)
            (next-stage geometry-stage))
    (declare (ignore next-stage))
    (primitive-name-to-instance
     (typecase primitive
       (points :points)
       (iso-lines :lines)
       (triangles :triangles)
       (t (error 'couldnt-convert-primitive-for-geometry-stage
                 :prim (type-of primitive)
                 :prev-stage stage)))))

  (:method (primitive
            stage
            (next-stage fragment-stage))
    (declare (ignore primitive stage next-stage))
    nil))

(defun %array-the-output-variables-for-primitive (primitive output-variables)
  (mapcar λ(make-instance
            'output-variable
            :name (name _)
            :glsl-name (glsl-name _)
            :type (v-array-type-of (v-type-of _) (vertex-count primitive) nil)
            :qualifiers (qualifiers _))
          output-variables))

(defgeneric transform-arg-types (last next stage primitive)
  (:method ((last vertex-stage)
            (next geometry-stage)
            (stage stage)
            primitive)
    (declare (ignore last next))
    (%array-the-output-variables-for-primitive
     primitive
     (rest (output-variables stage))))

  (:method ((last vertex-stage)
            (next fragment-stage)
            (stage stage)
            primitive)
    (declare (ignore last next primitive))
    (rest (output-variables stage)))

  (:method ((last vertex-stage)
            (next tessellation-control-stage)
            (stage stage)
            primitive)
    (declare (ignore last next))
    (%array-the-output-variables-for-primitive
     primitive
     (rest (output-variables stage))))

  (:method ((last vertex-stage)
            (next tessellation-evaluation-stage)
            (stage stage)
            primitive)
    (declare (ignore last next))
    (%array-the-output-variables-for-primitive
     primitive
     (rest (output-variables stage))))

  (:method ((last tessellation-evaluation-stage)
            (next geometry-stage)
            (stage stage)
            primitive)
    (declare (ignore last next))
    (%array-the-output-variables-for-primitive
     primitive
     (rest (output-variables stage))))

  (:method ((last vertex-stage) next (stage stage) primitive)
    (declare (ignore last next primitive))
    (rest (output-variables stage)))

  (:method (last next (stage stage) primitive)
    (declare (ignore last next primitive))
    (output-variables stage)))

;;----------------------------------------------------------------------

(defun check-order (stage-type remaining-stage-types)
  (let ((check (member stage-type remaining-stage-types
                       :test #'subtypep)))
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
