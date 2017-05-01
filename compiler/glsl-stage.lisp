(in-package :varjo)

(defun check-for-input-output-name-clashes (stage-kind in-args outputs)
  (let* ((in-args (remove-if-not #'listp in-args))
         (in-args (mapcar (lambda (in-arg)
                            (dbind (nil type name) in-arg
                              (list name type)))
                          in-args))
         (outputs (remove-if-not #'listp outputs))
         (clashes (intersection in-args outputs :key #'first :test #'string=)))
    (when clashes
      (error 'clashes-found-between-input-and-output-names
             :stage-kind stage-kind
             :inputs in-args
             :outputs outputs
             :clashes clashes))))

(defun glsl-to-compile-result
    (stage-kind in-args uniforms outputs context body-string)
  "Here our goal is to simple reuse as much from varjo as possible.
   This will mean we have less duplication, even if things seem a little
   ugly here"
  (assert (not (eq stage-kind :vertex)) ()
          'inline-glsl-vertex-stage-not-supported)
  (check-for-input-output-name-clashes stage-kind in-args outputs)
  (let* ((none-type (type-spec->type :none))
         (arg-types (mapcar #'type-spec->type
                            (append (mapcar #'second in-args)
                                    (mapcar #'second uniforms))))
         (primitive-kind (get-primitive-type-from-context context))
         (primitive (when primitive-kind
                      (primitive-name-to-instance primitive-kind)))
         (context (when primitive-kind
                    (remove primitive-kind context :test #'equal)))
         (stage (make-stage stage-kind in-args uniforms context
                            nil nil primitive)))
    (first
     (multiple-value-list
      (flow-id-scope
        (let ((env (%make-base-environment stage)))
          (pipe-> (stage env)
            #'set-env-context
            #'process-primitive-type
            #'add-context-glsl-vars
            #'expand-input-variables
            #'process-uniforms
            #'(lambda (stage env)
                (values (make-instance
                         'compiled-function-result
                         :function-obj nil
                         :signatures nil
                         :ast (ast-node! :error nil none-type nil nil)
                         :used-types nil
                         :glsl-code body-string
                         :stemcells nil
                         :used-types arg-types
                         :return-set (unless (eq stage-kind :geometry)
                                       (make-glsl-ret-set stage outputs))
                         :emit-set (when (eq stage-kind :geometry)
                                     (make-glsl-ret-set stage outputs)))
                        stage
                        env))
            #'make-post-process-obj
            #'(lambda (pp)
                (process-glsl-output-primitive stage-kind body-string pp))
            #'make-out-set
            #'check-stemcells
            #'filter-used-items
            #'gen-in-arg-strings
            #'gen-in-decl-strings
            #'gen-out-var-strings
            #'final-uniform-strings
            #'final-string-compose
            #'package-as-final-result-object)))))))

(defun process-glsl-output-primitive (stage-kind body-string post-proc-obj)
  (case stage-kind
    (:geometry
     (let* ((map '(("points" . :points)
                   ("line_strip" . :line-strip)
                   ("triangle_strip" . :triangle-strip)))
            (layout-string
             (first
              (cl-ppcre:all-matches-as-strings
               "layout.*\\(.*(points|line_strip|triangle_strip).*\\).*out.*;"
               body-string)))
            (primitive-str
             (when layout-string
               (first
                (cl-ppcre:all-matches-as-strings
                 "(points|line_strip|triangle_strip)" layout-string))))
            (primitive-name (or (assocr primitive-str map :test #'equal)
                                (error 'glsl-geom-stage-no-out-layout
                                       :glsl-body body-string)))
            (primitive (primitive-name-to-instance primitive-name)))
       (setf (primitive-out post-proc-obj) primitive)))
    (:tessellation-control
     (let* ((layout-string
             (first
              (cl-ppcre:all-matches-as-strings "layout.*out.*;" body-string)))
            (patch-size-str
             (when layout-string
               (first (cl-ppcre:all-matches-as-strings "[0-9]+" layout-string))))
            (patch-size (if patch-size-str
                            (parse-integer patch-size-str)
                            3))
            (primitive-name `(:patch ,patch-size))
            (primitive (primitive-name-to-instance primitive-name)))
       (setf (primitive-out post-proc-obj) primitive)))
    (:tessellation-evaluation
     (let* ((map '(("isolines" . :iso-lines)
                   ("triangles" . :triangles)
                   ("quads" . :quads)))
            (layout-string
             (first
              (cl-ppcre:all-matches-as-strings
               "layout.*\\(.*(isolines|triangles|quads).*\\).*in.*;"
               body-string)))
            (primitive-str
             (when layout-string
               (first
                (cl-ppcre:all-matches-as-strings
                 "(isolines|triangles|quads)" layout-string))))
            (primitive-name (or (assocr primitive-str map :test #'equal)
                                (error 'glsl-geom-stage-no-out-layout
                                       :glsl-body body-string)))
            (primitive (primitive-name-to-instance primitive-name)))
       (setf (primitive-out post-proc-obj) primitive)))
    (otherwise
     (setf (primitive-out post-proc-obj)
           (primitive-in (stage post-proc-obj)))))
  post-proc-obj)

(defun make-glsl-ret-set (stage outputs)
  (let ((outputs (if (stage-where-first-return-is-position-p stage)
                     (cons `("dummy" :vec4) outputs)
                     outputs)))
    (apply #'vector
           (loop :for (glsl-name type . qualifiers) :in outputs :collect
              (make-typed-out-name
               glsl-name (type-spec->type type (flow-id!))
               (sort (copy-list qualifiers) #'<))))))
