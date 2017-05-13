(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Assignment

;;{TODO} make it handle multiple assignements like cl version
(v-defmacro setf (&rest args)
  (let ((pairs (group args 2)))
    (if (= (length pairs) 1)
        `(%modify-place setf = ,@(first pairs))
        `(progn
           ,@(mapcar λ`(%modify-place setf = ,@_) pairs)))))

(v-defmacro incf (place &optional (val 1))
  :args-valid t
  :return
  `(%modify-place incf += ,place ,val))

(v-defmacro decf (place &optional (val 1))
  :args-valid t
  :return
  `(%modify-place decf -= ,place ,val))

(v-defmacro multf (place &optional (val 1))
  :args-valid t
  :return
  `(%modify-place multf *= ,place ,val))

(v-defmacro divf (place &optional (val 1))
  :args-valid t
  :return
  `(%modify-place divf /= ,place ,val))

(v-defspecial %modify-place (lisp-op-name glsl-op-symbol place val)
  :args-valid t
  :return
  (multiple-value-bind (place env-0) (compile-place place env :allow-unbound t)
    (multiple-value-bind (val env) (compile-form val env-0)
      (assert (member lisp-op-name '(setf incf decf multf divf)))
      (cond
        ((not (place-tree place))
         (error 'non-place-assign :glsl-op glsl-op-symbol
                :place place :val val))
        ((not (v-type-eq (primary-type place) (primary-type val)))
         (error 'assignment-type-match :op lisp-op-name
                :code-obj-a place :code-obj-b val))
        (t (destructuring-bind (name value) (last1 (place-tree place))
             (when (v-read-only value)
               (error 'assigning-to-readonly :var-name name))
             (unless (or (= (v-function-scope env) (v-function-scope value))
                         (= (v-function-scope value) 0))
               (error 'cross-scope-mutate :var-name name
                      :code (format nil "(setf (... ~s) ...)" name)))
             (vbind (old-val old-env) (get-symbol-binding name nil env)
               (assert (eq old-val value))
               (let ((final-env (replace-flow-ids name old-val (flow-ids val)
                                                  old-env env))
                     (type-set (make-type-set (primary-type val)))
                     (cline (gen-bin-op-string glsl-op-symbol place val)))
                 (values (merge-compiled
                          (list place val)
                          :type-set type-set
                          :current-line cline
                          :node-tree (ast-node! lisp-op-name
                                                (list (node-tree place)
                                                      (node-tree val))
                                                type-set
                                                env
                                                final-env)
                          :pure nil)
                         final-env)))))))))

(v-defspecial setq (var-name new-val-code)
  :args-valid t
  :return
  (multiple-value-bind (old-val old-env) (get-symbol-binding var-name nil env)
    (assert (and old-val old-env))
    (if (typep old-val 'v-symbol-macro)
        (compile-form `(setf ,var-name ,new-val-code) env)
        (compile-regular-setq-form var-name old-val old-env new-val-code env))))

(defun compile-regular-setq-form (var-name old-val old-env new-val-code env)
  (let ((new-val (compile-form new-val-code env)))
    (cond
      ((v-read-only old-val)
       (error 'setq-readonly :code `(setq ,var-name ,new-val-code)
              :var-name var-name))
      ((and (not (= (v-function-scope old-val) (v-function-scope env)))
            (> (v-function-scope old-val) 0)) ;; ok if var is global
       (error 'cross-scope-mutate :var-name var-name
              :code `(setq ,var-name ,new-val-code))))

    (let ((final-env (replace-flow-ids var-name old-val
                                       (flow-ids new-val)
                                       old-env env))
          (actual-type (calc-setq-type new-val old-val var-name)))
      (values (copy-compiled
               new-val
               :type-set (make-type-set actual-type)
               :current-line (gen-setq-assignment-string
                              old-val new-val)
               :place-tree nil
               :node-tree (ast-node!
                           'setq
                           (list (ast-node! :get var-name
                                            (make-type-set actual-type)
                                            env env)
                                 (node-tree new-val))
                           (make-type-set actual-type)
                           env
                           final-env)
               :pure nil)
              final-env))))

(defun calc-setq-type (new-val old-val var-name)
  (restart-case (if (v-type-eq (v-type-of old-val) (primary-type new-val))
                    (primary-type new-val)
                    (error 'setq-type-match :var-name var-name
                           :old-value old-val :new-value new-val))
    (setq-supply-alternate-type (replacement-type-spec)
      (type-spec->type replacement-type-spec (flow-ids new-val)))))

(defun replace-flow-ids (old-var-name old-val flow-ids old-env env)
  (assert (typep old-val 'v-value))
  (labels ((walk-envs (n)
             (if (eq n old-env)
                 (env-replace-parent
                  n
                  (v-parent-env n)
                  :symbol-bindings
                  (a-add old-var-name
                         (v-make-value
                          (replace-flow-id (v-type-of old-val) flow-ids)
                          n
                          :read-only (v-read-only old-val)
                          :function-scope (v-function-scope old-val)
                          :glsl-name (glsl-name old-val))
                         (copy-list (v-symbol-bindings n))))
                 (env-replace-parent n (walk-envs (v-parent-env n))))))
    (if (or (eq old-env *global-env*) (typep old-env 'base-environment))
        env
        (walk-envs env))))

;;------------------------------------------------------------
