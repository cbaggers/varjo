(in-package :varjo.internals)

;;----------------------------------------------------------------------

(defun coerce-real (obj dst-type env code)
  (compile-form
   (let ((src-type (primary-type obj)))
     (typecase dst-type
       (v-float `(vari.glsl:float ,obj))
       (v-double `(vari.glsl:double ,obj))
       (v-complex `(complex ,obj))
       (otherwise (error 'invalid-coerce
                         :code code
                         :src-type src-type
                         :dst-type dst-type))))
   env))

(v-defspecial coerce (object output-type-spec)
  :args-valid t
  :return
  (vbind (new-obj new-env) (compile-form object env)
    (let ((src-type (primary-type new-obj))
          (dst-type (type-spec->type output-type-spec))
          (code `(coerce ,object ,output-type-spec)))
      (if (or (v-type-eq src-type dst-type)
              (eq (type-of dst-type) 'v-type))
          (values new-obj new-env)
          (typecase src-type
            (v-real (coerce-real new-obj dst-type new-env code))
            (otherwise (error 'invalid-coerce
                              :code code
                              :src-type src-type
                              :dst-type dst-type)))))))

;;----------------------------------------------------------------------
