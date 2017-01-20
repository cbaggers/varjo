(in-package :varjo)

;; {NOTE} third-party-metadata only applies to rolling-translate

(defun clone-compile-result (original
                             &key
                               (glsl-code nil glsl-code-set)
                               (out-vars nil out-vars-set)
                               (stage-type nil stage-type-set)
                               (in-args nil in-args-set)
                               (uniforms nil uniforms-set)
                               (implicit-uniforms nil implicit-uniforms-set)
                               (context nil context-set)
                               (allowed-stemcells nil a-s-set)
                               (used-external-functions nil used-external-functions-set)
                               (used-macros nil used-macros-set)
                               (used-compiler-macros nil used-compiler-macros-set)
                               (function-asts nil function-asts-set)
                               (used-symbol-macros nil used-symbol-macros-set)
                               (third-party-metadata nil third-party-metadata-set))
  (make-instance
   'varjo-compile-result
   :glsl-code (if glsl-code-set glsl-code (glsl-code original))
   :out-vars (if out-vars-set out-vars (out-vars original))
   :stage-type (if stage-type-set stage-type (stage-type original))
   :in-args (if in-args-set in-args (in-args original))
   :uniforms (if uniforms-set uniforms (uniforms original))
   :implicit-uniforms (if implicit-uniforms-set implicit-uniforms (implicit-uniforms original))
   :context (if context-set context (context original))
   :allowed-stemcells (if a-s-set allowed-stemcells (allowed-stemcells original))
   :used-external-functions (if used-external-functions-set
                                used-external-functions
                                (used-external-functions original))
   :used-macros (if used-macros-set used-macros (used-macros original))
   :used-compiler-macros (if used-compiler-macros-set used-compiler-macros (used-compiler-macros original))
   :function-asts (if function-asts-set function-asts (function-asts original))
   :used-symbol-macros (if used-symbol-macros-set used-symbol-macros (used-symbol-macros original))
   :third-party-metadata (if third-party-metadata-set third-party-metadata (third-party-metadata original))))

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

(defmethod print-object ((obj implicit-uniform) stream)
  (format stream "#<IMPLICIT-UNIFORM ~a>" (name obj)))
