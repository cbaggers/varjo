(in-package :varjo)

;;*shader-context*
(defun substitutionp (symbol)
  (let ((sub (assoc symbol *glsl-substitutions*
                    :test #'symbol-name-equal)))
    (and (not (null sub))
         (or (third sub) (eq symbol (first sub)))
         (if (fourth sub) 
             (context-ok-given-restriction *shader-context* (fourth sub))
             t))))

(defun substitution (symbol)
  (when (substitutionp symbol)
    (first (assocr symbol *glsl-substitutions*
                   :test #'symbol-name-equal))))

(defmacro vdefmacro (name lambda-list &body body)
  `(register-substitution
    ',name
    (lambda ,lambda-list
      ,@body)))

(defmacro %vdefmacro (name packageless context-restriction lambda-list &body body)
  `(register-substitution
    ',name
    (lambda ,lambda-list
      ,@body)
    ,packageless
    ,context-restriction))
