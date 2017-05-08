(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun v-compile (uniforms version
                  &key vertex tessellation-control tessellation-evaluation
                    geometry fragment allow-stemcells (draw-mode :triangles))
  "
This function takes lisp code as lists and returns the results of compiling
that code to glsl.

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
  (assert (or vertex tessellation-control tessellation-evaluation
              geometry fragment))
  (let* ((prim (list draw-mode))
         (stages (list (when vertex
                         (make-stage :vertex
                                     (first vertex)
                                     uniforms
                                     (list version)
                                     (rest vertex)
                                     allow-stemcells
                                     (pop prim)))
                       (when tessellation-control
                         (make-stage :tessellation-control
                                     (first tessellation-control)
                                     uniforms
                                     (list version)
                                     (rest tessellation-control)
                                     allow-stemcells
                                     (pop prim)))
                       (when tessellation-evaluation
                         (make-stage :tessellation-evaluation
                                     (first tessellation-evaluation)
                                     uniforms
                                     (list version)
                                     (rest tessellation-evaluation)
                                     allow-stemcells
                                     (pop prim)))
                       (when geometry
                         (make-stage :geometry
                                     (first geometry)
                                     uniforms
                                     (list version)
                                     (rest geometry)
                                     allow-stemcells
                                     (pop prim)))
                       (when fragment
                         (make-stage :fragment
                                     (first fragment)
                                     uniforms
                                     (list version)
                                     (rest fragment)
                                     allow-stemcells
                                     (pop prim)))))
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
         (env (%make-base-environment stage)))
    (flow-id-scope (ast->code (compile-form form env)))))
