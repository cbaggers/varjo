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
      (let ((code (list lisp-op-name place val)))
        (assert (v-type-eq (primary-type place-obj) (primary-type val-obj)) ()
                'assignment-type-match
                :code-obj-a place-obj
                :code-obj-b val-obj
                :form code)
        (let* ((modified-env (make-env-with-place-modification
                              place-obj
                              (flow-ids val-obj)
                              env
                              code))
               (type-set (make-type-set (primary-type val-obj)))
               (cline (gen-bin-op-string
                       glsl-op-symbol place-obj val-obj)))
          (values (merge-compiled
                   (list place-obj val-obj)
                   :type-set type-set
                   :current-line cline
                   :pure nil)
                  modified-env))))))

(v-defspecial setq (var-name new-val-code)
  :args-valid t
  :return
  (multiple-value-bind (current-value env-holding-var)
      (get-symbol-binding var-name nil env)
    (assert (and current-value env-holding-var))
    (if (typep current-value 'v-symbol-macro)
        (compile-form `(setf ,var-name ,new-val-code)
                      env)
        (compile-regular-setq-form var-name
                                   current-value
                                   env-holding-var
                                   new-val-code
                                   env))))

(defun compile-regular-setq-form (var-name
                                  current-value
                                  env-holding-var
                                  new-val-code
                                  env)
  (let ((new-val (compile-form new-val-code env)))
    (cond
      ((v-read-only current-value)
       (error 'setq-readonly :code `(setq ,var-name ,new-val-code)
              :var-name var-name))
      ((and (not (= (v-function-scope current-value)
                    (v-function-scope env)))
            (> (v-function-scope current-value) 0)) ;; ok if var is global
       (error 'cross-scope-mutate :var-name var-name
              :code `(setq ,var-name ,new-val-code))))

    (let ((final-env (replace-flow-ids-for-specific-value var-name
                                                          current-value
                                                          env-holding-var
                                                          (flow-ids new-val)
                                                          env))
          (actual-type (calc-setq-type new-val current-value var-name)))
      (values (copy-compiled
               new-val
               :type-set (make-type-set actual-type)
               :current-line (gen-setq-assignment-string
                              current-value new-val)
               :place-tree nil
               :pure nil)
              final-env))))

(defun calc-setq-type (new-val current-value var-name)
  (restart-case (if (v-type-eq (v-type-of current-value) (primary-type new-val))
                    (primary-type new-val)
                    (error 'setq-type-match :var-name var-name
                           :old-value current-value :new-value new-val))
    (setq-supply-alternate-type (replacement-type-spec)
      (type-spec->type replacement-type-spec (flow-ids new-val)))))

;;------------------------------------------------------------
