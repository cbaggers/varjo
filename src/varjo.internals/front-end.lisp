(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun v-compile (uniforms version
                  &key vertex tessellation-control tessellation-evaluation
                    geometry fragment compute allow-stemcells
                    draw-mode (primitive :triangles))
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
                         (values (vec4 pos 1.0) a))
               :fragment '(((hmm :float))
                           (labels ((fun ((x :float))
                                      (* x x)))
                             (vec4 1.0 1.0 hmm (fun a)))))
"
  (assert (or vertex tessellation-control tessellation-evaluation
              geometry fragment compute))
  (when draw-mode
    (warn 'v-deprecated :old ":draw-mode" :new ":primitive"))
  (let* ((primitive (or primitive draw-mode))
         (prims (list primitive))
         (stages (list (when vertex
                         (make-stage :vertex
                                     (first vertex)
                                     uniforms
                                     (list version)
                                     (rest vertex)
                                     allow-stemcells
                                     (pop prims)))
                       (when tessellation-control
                         (make-stage :tessellation-control
                                     (first tessellation-control)
                                     uniforms
                                     (list version)
                                     (rest tessellation-control)
                                     allow-stemcells
                                     (pop prims)))
                       (when tessellation-evaluation
                         (make-stage :tessellation-evaluation
                                     (first tessellation-evaluation)
                                     uniforms
                                     (list version)
                                     (rest tessellation-evaluation)
                                     allow-stemcells
                                     (pop prims)))
                       (when geometry
                         (make-stage :geometry
                                     (first geometry)
                                     uniforms
                                     (list version)
                                     (rest geometry)
                                     allow-stemcells
                                     (pop prims)))
                       (when fragment
                         (make-stage :fragment
                                     (first fragment)
                                     uniforms
                                     (list version)
                                     (rest fragment)
                                     allow-stemcells
                                     (pop prims)))
                       (when compute
                         (make-stage :compute
                                     nil
                                     uniforms
                                     (list version)
                                     (rest compute)
                                     allow-stemcells
                                     (pop prims)))))
         (stages (remove nil stages))
         (first (first stages)))
    (setf (primitive-in first) primitive)
    (rolling-translate stages)))

;;----------------------------------------------------------------------
