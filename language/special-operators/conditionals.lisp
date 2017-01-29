(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; If

;; note that just like in lisp this only fails if false. 0 does not fail.
(v-defspecial if (test-form then-form &optional else-form)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (let ((always-true (or (not (v-typep (code-type test-obj) 'v-bool))
                           (eq test-form t)))
          (always-false (eq test-form nil))
          (has-else (not (or (null else-form) (equal else-form '(values)))))
          (else-form (or else-form '(values))))
      (cond
        ;; constant true
        (always-true (compile-form `(progn ,test-obj ,then-form) test-env))
        ;;
        (always-false (compile-form `(progn ,test-obj ,else-form) test-env))
        ;;
        (t (compile-the-regular-form-of-if test-obj test-env then-form else-form
                                           has-else env))))))

(defun compile-the-regular-form-of-if (test-obj test-env then-form else-form
                                       has-else starting-env)
  (multiple-value-bind (then-obj then-env) (compile-form then-form test-env)
    (multiple-value-bind (else-obj else-env) (compile-form else-form test-env)
      ;;
      (let* ((arg-objs (remove-if #'null (list test-obj then-obj else-obj)))
             (final-env
              (apply #'env-merge-history
                     (env-prune* (env-depth test-env) then-env else-env)))
             (result-type (gen-or-type (list (code-type then-obj)
                                             (code-type else-obj))))
             (node-tree (ast-node! 'if
                                   (mapcar #'node-tree
                                           (list test-obj then-obj else-obj))
                                   result-type
                                   starting-env final-env)))
        (vbind (block-string current-line-string)
            (gen-string-for-if-form test-obj then-obj else-obj result-type
                                    has-else)
          (values (merge-obs arg-objs
                             :type result-type
                             :current-line current-line-string
                             :to-block (list block-string)
                             :node-tree node-tree)
                  final-env))))))

(defun gen-string-for-if-form (test-obj then-obj else-obj result-type has-else)
  (let* ((will-assign (and (not (typep result-type 'v-void))
                           (not (typep result-type 'v-or))))
         (tmp-var (when will-assign (safe-glsl-name-string (gensym "tmp")))))
    (values
     (format nil "~@[~a~%~]if (~a)~%~a~@[~%else~%~a~]"
             (when tmp-var
               (prefix-type-to-string result-type (end-line-str tmp-var)))
             (current-line test-obj)
             (gen-string-for-if-block then-obj tmp-var)
             (when has-else
               (gen-string-for-if-block else-obj tmp-var)))
     (when will-assign
       tmp-var))))

(defun gen-string-for-if-block (code-obj glsl-tmp-var-name)
  (format nil "{~a~a~%}"
          (indent-for-block (to-block code-obj))
          (when (current-line code-obj)
            (let ((current (end-line-str (current-line code-obj))))
              (indent-for-block
               (if glsl-tmp-var-name
                   (%gen-assignment-string glsl-tmp-var-name current)
                   current))))))

;;------------------------------------------------------------
;; When

(v-defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       (values)))

;;------------------------------------------------------------
;; Unless

(v-defmacro unless (test &body body)
  `(if (not ,test)
       (progn ,@body)
       (values)))

;;------------------------------------------------------------
;; Switch

;; {TODO} check keys
(v-defspecial switch (test-form &rest clauses)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (let* ((keys (mapcar #'first clauses))
           (clause-pairs (mapcar λ(multiple-value-list
                                   (compile-form `(progn ,(second _)) env))
                                 clauses))
           (clause-objs (mapcar #'first clause-pairs))
           (final-env
            (let ((envs (apply #'env-prune* (env-depth test-env)
                               (mapcar #'second clause-pairs))))
              (reduce #'env-merge-history
                      (rest envs) :initial-value (first envs)))))
      (if (and (or (v-typep (code-type test-obj) 'v-uint)
                   (v-typep (code-type test-obj) 'v-int))
               (loop :for key :in keys :always
                  (or (eq key 'default) (integerp key))))
          (let ((type (type-spec->type :void (flow-id!))))
            (values (merge-obs clause-objs
                               :type type
                               :current-line nil
                               :to-block (list (gen-switch-string test-obj keys
                                                                  clause-objs))
                               :node-tree (ast-node!
                                           'switch
                                           (cons (node-tree test-obj)

                                                 (mapcar λ`(,(first _)
                                                             ,(node-tree _1))
                                                         clauses
                                                         clause-objs))
                                           type env final-env))
                    final-env))
          (error 'switch-type-error :test-obj test-obj :keys keys)))))

;;------------------------------------------------------------
