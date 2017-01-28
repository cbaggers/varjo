(in-package :varjo)

(defun clone-compile-result (original
                             &key
                               (glsl-code nil glsl-code-set)
                               (out-vars nil out-vars-set)
                               (stage-type nil stage-type-set)
                               (in-args nil in-args-set)
                               (input-variables nil inp-vars-set)
                               (uniforms nil uniforms-set)
                               (implicit-uniforms nil implicit-uniforms-set)
                               (context nil context-set)
                               (stemcells-allowed nil a-s-set)
                               (used-external-functions nil used-external-functions-set)
                               (function-asts nil function-asts-set))
  (warn "clone-compile-result is incomplete")
  (make-instance
   'varjo-compile-result
   :glsl-code (if glsl-code-set glsl-code (glsl-code original))
   :out-vars (if out-vars-set out-vars (out-vars original))
   :stage-type (if stage-type-set stage-type (stage-type original))
   :in-args (if in-args-set in-args (in-args original))
   :input-variables (if inp-vars-set input-variables (input-variables original))
   :uniforms (if uniforms-set uniforms (uniforms original))
   :implicit-uniforms (if implicit-uniforms-set implicit-uniforms (implicit-uniforms original))
   :context (if context-set context (context original))
   :stemcells-allowed (if a-s-set stemcells-allowed (stemcells-allowed original))
   :used-external-functions (if used-external-functions-set
                                used-external-functions
                                (used-external-functions original))
   :function-asts (if function-asts-set function-asts (function-asts original))))

(defmethod ast ((obj varjo-compile-result))
  (let* ((res (first (function-asts obj)))
         (ending-env (ast-ending-env res))
         (context (v-context ending-env)))
    (assert (member :main context) ()
            "The varjo-compile-result object should have the ast for the main
function at the head of the list returned by #'function-asts.
However this was not the case.

This is a compiler bug. If you have the time then please report it at:
https://github.com/cbaggers/varjo.git

Sorry for the inconvenience")
    res))

(defmethod print-object ((obj implicit-uniform-variable) stream)
  (format stream "#<IMPLICIT-UNIFORM ~a>" (name obj)))
