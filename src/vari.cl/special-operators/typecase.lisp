(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; typecase

(v-defspecial typecase (keyform &rest cases)
  :args-valid t
  :return
  (vbind (key-obj key-env) (compile-form keyform env)
    (let* ((type-specs (mapcar #'first cases))
           (otherwise-pos (position 'otherwise type-specs))
           (types (mapcar #'type-spec->type (remove 'otherwise type-specs)))
           (key-type (primary-type key-obj))
           (match-pos (or (position key-type types :test #'v-typep)
                          otherwise-pos))
           (form `(typecase ,keyform ,@cases)))
      (when otherwise-pos
        (assert (= otherwise-pos (- (length cases) 1)) ()
                "Varjo: 'otherwise' in an invalid position in typecase~%~a"
                form))
      (assert match-pos ()
              "Varjo: no types in typecase matched type of keyform~%keyform type:~a~%form:~%~a"
              (type->type-spec key-type) form)

      (compile-form
       `(progn
          ,key-obj
          ,@(rest (elt cases match-pos)))
       key-env))))
