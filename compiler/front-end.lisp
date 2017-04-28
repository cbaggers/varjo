(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun v-compile (uniforms version
                  &key vertex tessellation-control tessellation-evaluation
                    geometry fragment allow-stemcells (draw-mode :triangles))
  "
This function takes lisp code as lists and returns the results of compiling that
code to glsl.

Each result is an object of type 'compiled-stage.

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
  (assert (or vertex tessellation-control tessellation-evaluation geometry fragment))
  (let* ((stages (list (when vertex
                         (make-stage :vertex
                                     (first vertex)
                                     uniforms
                                     (list version)
                                     (rest vertex)
                                     allow-stemcells
                                     nil))
                       (when tessellation-control
                         (make-stage :tessellation-control
                                     (first tessellation-control)
                                     uniforms
                                     (list version)
                                     (rest tessellation-control)
                                     allow-stemcells
                                     nil))
                       (when tessellation-evaluation
                         (make-stage :tessellation-evaluation
                                     (first tessellation-evaluation)
                                     uniforms
                                     (list version)
                                     (rest tessellation-evaluation)
                                     allow-stemcells
                                     nil))
                       (when geometry
                         (make-stage :geometry
                                     (first geometry)
                                     uniforms
                                     (list version)
                                     (rest geometry)
                                     allow-stemcells
                                     nil))
                       (when fragment
                         (make-stage :fragment
                                     (first fragment)
                                     uniforms
                                     (list version)
                                     (rest fragment)
                                     allow-stemcells
                                     nil))))
         (stages (remove nil stages))
         (first (first stages)))
    (setf (primitive-in first) draw-mode)
    (rolling-translate (remove nil stages))))

;;----------------------------------------------------------------------

(defun v-macroexpand (form &optional (as-stage :vertex) (as-version :450))
  (assert (find as-stage *stage-names*))
  (assert (find as-version *supported-versions*))
  (let* ((stage (make-stage as-stage nil nil (list as-version)
                            (list form) nil :triangles))
         (env (%make-base-environment stage :version as-version)))
    (flow-id-scope (ast->code (compile-form form env)))))

;;----------------------------------------------------------------------

(defun largest-primitive-for-stage (type)
  (case type
    (vertex-stage :triangles-adjacency)
    (tessellation-control-stage :patch)
    (tessellation-evaluation-stage :quads)
    (geometry-stage :triangles-adjacency)
    (fragment-stage nil)
    (otherwise (error "Varjo: Invalid stage kind name ~a" type))))

(defun test-translate (stage &key (stages *stage-names*))
  (loop :for kind :in stages :collect
     (with-slots (input-variables
                  uniform-variables context lisp-code
                  stemcells-allowed previous-stage primitive-in)
         stage
       (let* ((type (stage-kind-to-type kind))
              (stage (make-instance
                      type
                      :input-variables input-variables
                      :uniform-variables uniform-variables
                      :context context
                      :lisp-code lisp-code
                      :stemcells-allowed stemcells-allowed
                      :previous-stage previous-stage
                      :primitive-in (unless primitive-in
                                      (largest-primitive-for-stage type)))))
         (handler-case (translate stage)
           (error (e) e))))))

(defun test-translate-raising
    (stage &key (stages *stage-names*) (error-on-any-p nil))
  (let ((results (test-translate stage :stages stages)))
    (labels ((errorp (x) (typep x 'error)))
      (if (or (and error-on-any-p (find-if #'errorp results))
              (every #'errorp results))
          (raise-test-translate-error results stages)
          (remove-if #'errorp results)))))

(defun raise-test-translate-error (errors stage-types)
  (let ((groups (group-by (compose #'princ-to-string #'first)
                          (remove-if-not
                           (lambda (x) (typep (first x) 'condition))
                           (mapcar #'list errors stage-types)))))
    (if (= 1 (length groups))
        (error (caaar groups))
        (let ((grouped (mapcar (lambda (grp)
                                 (cons (mapcar #'second grp)
                                       (princ-to-string (caar grp))))
                               groups)))
          (error 'test-translate-failed :grouped-errors grouped)))))
