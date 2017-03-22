(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Assignment

;;{TODO} make it handle multiple assignements like cl version
(v-defmacro setf (&rest args)
  (labels ((make-set-form (p v) `(setf-1 ,p ,v)))
    (let ((pairs (group args 2)))
      (if (= (length pairs) 1)
          (make-set-form (first (first pairs)) (second (first pairs)))
          `(progn
             ,@(loop :for (p v) :in pairs :collect (make-set-form p v)))))))

(v-defspecial setf-1 (place val)
  :args-valid t
  :return
  (multiple-value-bind (place env-0) (compile-place place env :allow-unbound t)
    (multiple-value-bind (val env) (compile-form val env-0)
      (cond
        ((not (place-tree place))
         (error 'non-place-assign :place place :val val))
        ((not (v-type-eq (code-type place) (code-type val)))
         (error 'setf-type-match :code-obj-a place :code-obj-b val))
        (t (destructuring-bind (name value) (last1 (place-tree place))
             (when (v-read-only value)
               (error 'setf-readonly :var-name name))
             (unless (or (= (v-function-scope env) (v-function-scope value))
                         (= (v-function-scope value) 0))
               (error 'cross-scope-mutate :var-name name
                      :code (format nil "(setf (... ~s) ...)" name)))
             (multiple-value-bind (old-val old-env) (get-symbol-binding name nil env)
               (assert (eq old-val value))
               (let ((final-env (replace-flow-ids name old-val (flow-ids val)
                                                  old-env env)))
                 (values (merge-obs (list place val)
                                    :type (code-type val)
                                    :current-line (gen-assignment-string place val)
                                    :node-tree (ast-node! 'setf
                                                          (list (node-tree place)
                                                                (node-tree val))
                                                          (code-type place)
                                                          env
                                                          final-env))
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
      (values (copy-code new-val :type actual-type
                         :current-line (gen-setq-assignment-string
                                        old-val new-val)
                         :multi-vals nil
                         :place-tree nil
                         :node-tree (ast-node!
                                     'setq
                                     (list (ast-node! :get var-name
                                                      actual-type
                                                      env env)
                                           (node-tree new-val))
                                     actual-type
                                     env
                                     final-env))
              final-env))))

(defun calc-setq-type (new-val old-val var-name)
  (restart-case (if (v-type-eq (v-type-of old-val) (code-type new-val))
                    (code-type new-val)
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
                          :glsl-name (v-glsl-name old-val))
                         (copy-list (v-symbol-bindings n))))
                 (env-replace-parent n (walk-envs (v-parent-env n))))))
    (if (or (eq old-env *global-env*) (typep old-env 'base-environment))
        env
        (walk-envs env))))

;; %assign is only used to set the current-line of the code object
;; it has no side effects on the compilation itself
(v-defspecial %assign ((place v-type) (val v-type))
  :return
  (values
   (merge-obs (list place val) :type (code-type place)
              :current-line (gen-assignment-string place val)
              :node-tree (ast-node! '%assign (list place val)
                                    (code-type place) env env))
   env))

;;------------------------------------------------------------
