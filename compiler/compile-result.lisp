(in-package :varjo)

(defun clone-compile-result
    (original
     &key
       (glsl-code nil glsl-code-set)
       (lisp-code nil lisp-code-set)
       (output-variables nil output-variables-set)
       (starting-stage nil starting-stage-set)
       (input-variables nil inp-vars-set)
       (uniform-variables nil uniforms-set)
       (implicit-uniforms nil implicit-uniforms-set)
       (context nil context-set)
       (stemcells-allowed nil a-s-set)
       (used-external-functions nil used-external-functions-set)
       (previous-stage nil previous-stage-set)
       (primitive-in nil primitive-in-set)
       (primitive-out nil primitive-out-set)
       (function-asts nil function-asts-set))
  (make-instance
   (compiled-stage-type-for original)
   :glsl-code (if glsl-code-set
                  glsl-code
                  (glsl-code original))
   :lisp-code (if lisp-code-set
                  lisp-code
                  (lisp-code original))
   :output-variables (if output-variables-set
                         output-variables
                         (output-variables original))
   :starting-stage (if starting-stage-set
                       starting-stage
                       (starting-stage original))
   :input-variables (if inp-vars-set
                        input-variables
                        (input-variables original))
   :uniform-variables (if uniforms-set
                          uniform-variables
                          (uniform-variables original))
   :implicit-uniforms (if implicit-uniforms-set
                          implicit-uniforms
                          (implicit-uniforms original))
   :context (if context-set
                context
                (context original))
   :stemcells-allowed (if a-s-set
                          stemcells-allowed
                          (stemcells-allowed original))
   :used-external-functions (if used-external-functions-set
                                used-external-functions
                                (used-external-functions original))
   :function-asts (if function-asts-set
                      function-asts
                      (function-asts original))
   :previous-stage (if previous-stage-set
                       previous-stage
                       (previous-stage original))
   :primitive-in (if primitive-in-set
                     primitive-in
                     (primitive-in original))
   :primitive-out (if primitive-out-set
                      primitive-out
                      (primitive-out original))))

(defmethod ast ((obj compiled-stage))
  (let* ((res (first (function-asts obj)))
         (ending-env (ast-ending-env res))
         (context (v-context ending-env)))
    (assert (member :main context) ()
            "The compiled-stage object should have the ast for the main
function at the head of the list returned by #'function-asts.
However this was not the case.

This is a compiler bug. If you have the time then please report it at:
https://github.com/cbaggers/varjo.git

Sorry for the inconvenience")
    res))

(defmethod print-object ((obj implicit-uniform-variable) stream)
  (format stream "#<IMPLICIT-UNIFORM ~a>" (name obj)))

(defmethod glsl-code ((objs list))
  (mapcar #'glsl-code objs))
