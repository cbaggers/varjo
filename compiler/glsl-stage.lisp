(in-package :varjo)

(defun glsl-to-compile-result
    (stage-kind in-args uniforms outputs context body-string)
  "Here our goal is to simple reuse as much from varjo as possible.
   This will mean we have less duplication, even if things seem a little
   ugly here"
  (let* ((none-type (type-spec->type :none))
         (arg-types (mapcar #'type-spec->type
                            (append (mapcar #'second in-args)
                                    (mapcar #'second uniforms))))
         (primitive-kind (get-primitive-type-from-context context))
         (primitive (when primitive-kind
                      (primitive-name-to-instance primitive-kind)))
         (context (when primitive-kind
                    (remove primitive-kind context)))
         (stage (make-stage stage-kind in-args uniforms context
                            nil nil primitive)))
    (first
     (multiple-value-list
      (flow-id-scope
        (let ((env (%make-base-environment stage)))
          (pipe-> (stage env)
            #'set-env-context
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
                                       (make-glsl-ret-set outputs))
                         :emit-set (when (eq stage-kind :geometry)
                                     (make-glsl-ret-set outputs)))
                        stage
                        env))
            #'make-post-process-obj
            #'check-stemcells
            #'filter-used-items
            #'gen-in-arg-strings
            #'gen-in-decl-strings
            #'gen-out-var-strings
            #'(lambda (pp)
                (process-glsl-output-primtive stage-kind body-string pp))
            #'final-uniform-strings
            #'final-string-compose
            #'package-as-final-result-object)))))))

(defun process-glsl-output-primtive (stage-kind body-string post-proc-obj)
  (if (eq stage-kind :geometry)
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
        (setf (primitive-out post-proc-obj) primitive))
      (setf (primitive-out post-proc-obj)
            (primitive-in (stage post-proc-obj))))
  post-proc-obj)

(defun make-glsl-ret-set (outputs)
  (apply #'vector
         (loop :for (glsl-name type . qualifiers) :in outputs :collect
            (make-external-return-val
             glsl-name (type-spec->type type (flow-id!))
             (sort (copy-list qualifiers) #'<)))))
