(in-package :vari.cl)
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
  ;; compile place and val so we can see what we have to work with
  (multiple-value-bind (place-obj env-0) (compile-place place env :allow-unbound t)
    (multiple-value-bind (val-obj env) (compile-form val env-0)
      ;; make sure we in the compiler havent been dumbasses
      (assert (member lisp-op-name '(setf incf decf multf divf)))
      (cond
        ((not (place-tree place-obj))
         (error 'non-place-assign :glsl-op glsl-op-symbol
                :place place-obj :val val-obj))
        ((not (v-type-eq (primary-type place-obj) (primary-type val-obj)))
         (error 'assignment-type-match :op lisp-op-name
                :code-obj-a place-obj :code-obj-b val-obj
                :form `(,lisp-op-name ,place ,val)))
        (t (destructuring-bind (name value) (last1 (place-tree place-obj))
             (when (v-read-only value)
               ;; The one time we can write to a uniform is when
               ;; it's an ssbo. We do make sure that the place-tree
               ;; is deeper than 1 though because otherwise we are
               ;; setting the uniform itself rather than an
               ;; element/slot
               (let* ((uniform (find (flow-ids (v-type-of value))
                                     (v-uniforms env)
                                     :key λ(flow-ids (v-type-of _))
                                     :test #'id=))
                      (is-ssbo (find :ssbo (qualifiers (v-type-of uniform))
                                     :test #'qualifier=)))
                 (assert (and is-ssbo (> (length (place-tree place-obj)) 1))
                         () 'assigning-to-readonly :var-name name)))
             (unless (or (= (v-function-scope env) (v-function-scope value))
                         (= (v-function-scope value) 0))
               (error 'cross-scope-mutate :var-name name
                      :code (format nil "(setf (... ~s) ...)" name)))
             (vbind (old-val old-env) (get-symbol-binding name nil env)
               (assert (eq old-val value))
               (let ((final-env (replace-flow-ids
                                 name old-val (flow-ids val-obj) old-env env))
                     (type-set (make-type-set (primary-type val-obj)))
                     (cline (gen-bin-op-string
                             glsl-op-symbol place-obj val-obj)))
                 (values (merge-compiled
                          (list place-obj val-obj)
                          :type-set type-set
                          :current-line cline
                          :node-tree (ast-node! lisp-op-name
                                                (list (node-tree place-obj)
                                                      (node-tree val-obj))
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

;;------------------------------------------------------------
