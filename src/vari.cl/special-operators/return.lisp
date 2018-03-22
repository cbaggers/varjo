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
                 ((or (v-voidp code-obj)
                      (ephemeral-p code-obj))
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
            (let* ((returned (type-spec->type 'v-returned
                                              (or (flow-ids code-obj)
                                                  (flow-id!))))
                   (type-set (make-type-set returned))
                   (return-set (varjo.internals::merge-return-sets
                                (remove nil (list (return-set code-obj)
                                                  (type-set code-obj))))))
              (values (copy-compiled final-obj
                                     :return-set return-set
                                     :type-set type-set
                                     :used-types (append
                                                  (used-types final-obj)
                                                  (coerce return-set 'list)))
                      final-env)))))))
