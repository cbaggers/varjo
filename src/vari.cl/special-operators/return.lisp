(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Return

(v-defspecial return (&optional (form '(values)))
  :args-valid t
  :return
  ;; we create an environment with the signal to let any 'values' forms
  ;; down the tree know they will be caught and what their name prefix should
  ;; be.
  ;; We then compile the form using the augmented environment, the values
  ;; statements will expand and flow back as 'multi-vals' and the
  ;; current-line
  (let ((stage (stage env))
        (is-main-p (member :main (v-context env)))
        (new-env (fresh-environment
                  env :multi-val-base *return-var-name-base*)))
    (vbind (code-obj code-env) (compile-form form new-env)
      (if (or (v-returned-p code-obj)
              (v-discarded-p code-obj))
          ;;
          ;; no-op
          (values code-obj code-env)
          ;;
          ;; emit the 'return'
          (vbind (final-obj final-env)
              (compile-form
               (cond
                 ((v-voidp code-obj)
                  `(progn
                     ,code-obj
                     (%glsl-expr "return" v-returned)))
                 (is-main-p
                  (let* ((glsl-name (nth-return-name 0 stage t))
                         (code `(progn
                                  (glsl-expr
                                   ,(format nil "~a = ~~a" glsl-name)
                                   ,(primary-type code-obj) ,code-obj)
                                  (%glsl-expr "return" v-returned))))
                    (when (typep stage 'vertex-stage)
                      (let ((expected (type-spec->type :vec4)))
                        (assert (v-type-eq (primary-type code-obj)
                                           expected)
                                () 'stage-primary-type-mismatch
                                :stage-kind (type-of stage)
                                :type-expected expected
                                :type-found (primary-type code-obj))))
                    code))
                 (t
                  `(%glsl-expr "return ~a"
                               ,(primary-type code-obj)
                               ,code-obj)))
               code-env)
            (let* ((ast (ast-node! 'return (node-tree code-obj)
                                   (make-type-set)
                                   env env))
                   (returned (type-spec->type 'v-returned
                                              (or (flow-ids code-obj)
                                                  (flow-id!))))
                   (type-set (make-type-set returned))
                   (return-set (varjo.internals::merge-return-sets
                                (remove nil (list (return-set code-obj)
                                                  (type-set code-obj))))))
              (values (copy-compiled final-obj
                                     :return-set return-set
                                     :type-set type-set
                                     :node-tree ast)
                      final-env)))))))

;; tests

#+nil
(glsl-code
 (varjo.tests::compile-vert () :450 t
   (labels ((gen-line ((x :int))
              (values x 1 2)))
     (gen-line 2)
     (multiple-value-bind (a b c) (gen-line 2)
       (values (v! a b c 4) (v! 2 2))))))

#+nil
(glsl-code
 (compile-vert ((x :int)) :450 t
   (flet ((foo ()
            (if (= x 1)
                (return (v! 1 1 1 1))
                (v! 2 2 2 2))))
     (foo))))

#+nil
(glsl-code
 (compile-vert ((x :int)) :450 t
   (flet ((foo ()
            (if (= x 1)
                (return (values (v! 1 1 1 1)
                                (v! 2 2)))
                (values (v! 3 3 3 3)
                        (v! 4 4)))))
     (foo))))
