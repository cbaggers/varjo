(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; If

;; note that just like in lisp this only fails if false. 0 does not fail.
(v-defspecial if (test-form then-form &optional (else-form nil has-else))
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test-form env)
    (assert (not (v-voidp test-obj)) () 'void-type-for-conditional-test
            :kind "if"
            :form test-form)
    (assert (not (v-discarded-p test-obj)) () 'discarded-for-conditional-test
            :kind "if"
            :form test-form)
    (let ((always-true (or (not (v-typep (primary-type test-obj) 'v-bool))
                           (eq test-form t)))
          (always-false (eq test-form nil))
          (has-else (and has-else (not (equal else-form '(values)))))
          (else-form (if has-else else-form '(values))))
      (cond
        ;; constant true
        (always-true (compile-form `(progn ,test-obj ,then-form) test-env))
        ;;
        (always-false (compile-form `(progn ,test-obj ,else-form) test-env))
        ;;
        (t (compile-the-regular-form-of-if test-form test-obj test-env
                                           then-form else-form has-else
                                           env))))))

(defun compile-the-regular-form-of-if (test-form test-obj test-env
                                       then-form else-form has-else
                                       starting-env)
  (multiple-value-bind (then-obj then-env) (compile-form then-form test-env)
    (multiple-value-bind (else-obj else-env) (compile-form else-form test-env)
      ;;
      (let* ((arg-objs (remove-if #'null (list test-obj then-obj else-obj)))
             (final-env
              (apply #'env-merge-history
                     (env-prune* (env-depth test-env) then-env else-env)))
             (type-set (compute-if-type-set then-obj else-obj starting-env)))
        (vbind (to-block current-line)
            (if (and has-else
                     (satifies-ternary-style-restrictions-p
                      test-form test-obj
                      then-form then-obj
                      else-form else-obj))
                (gen-string-for-ternary-form test-obj then-obj else-obj)
                (gen-string-for-if-form test-obj
                                        then-obj
                                        else-obj
                                        (primary-type type-set)
                                        has-else))
          ;; this next check is to preempt a possible return-type-mismatch
          ;; error. The reason we do this is to give a better error.
          (let ((return-sets (remove nil (mapcar #'return-set arg-objs))))
            (when (and (some #'v-voidp return-sets)
                       (some (complement #'v-voidp) return-sets))
              (error 'conditional-return-type-mismatch
                     :sets return-sets)))
          (values (merge-compiled arg-objs
                                  :type-set type-set
                                  :current-line current-line
                                  :to-block to-block)
                  final-env))))))

(defun compute-if-type-set (then-obj else-obj env)
  ;; The type-set needs to include all the results
  ;; so we need to do two things.
  ;; - assert the same number of values returned
  ;; - make v-or types for each pair
  ;; - add logic to multiple-value-bind & co to check
  ;;   for v-or types and complain
  ;; Only need this if multi-val-base is not nil
  (let* ((then-set (type-set then-obj))
         (else-set (type-set else-obj))
         (then-terminated (varjo.internals::v-terminated-p then-set))
         (else-terminated (varjo.internals::v-terminated-p else-set)))
    (cond
      ((and then-terminated (not else-terminated))
       else-set)
      ((and else-terminated (not then-terminated))
       then-set)
      ((v-multi-val-base env)
       (flet ((gen-or-pair (a b)
                (gen-or-type (list a b) (flow-id!))))
         (assert (= (length then-set) (length else-set)) ()
                 'if-form-multiple-vals-mismatch
                 :then-set then-set
                 :else-set else-set)
         (make-type-set* (map 'list #'gen-or-pair then-set else-set))))
      (t
       (let ((primary-result-type
              (gen-or-type (list (primary-type then-obj)
                                 (primary-type else-obj))
                           (flow-id!))))
         (if (v-voidp primary-result-type)
             (make-type-set)
             (make-type-set primary-result-type)))))))

(defun satifies-ternary-style-restrictions-p (test-form test-obj
                                              then-form then-obj
                                              else-form else-obj)
  (declare (ignorable test-form test-obj
                      then-form then-obj
                      else-form else-obj))
  (and (glsl-chunk-emptyp (to-block test-obj))
       (glsl-chunk-emptyp (to-block then-obj))
       (glsl-chunk-emptyp (to-block else-obj))
       (pure-p test-obj)
       (pure-p then-obj)
       (pure-p else-obj)
       (= (length (type-set then-obj)) 1)
       (= (length (type-set else-obj)) 1)
       (v-type-eq (primary-type then-obj) (primary-type else-obj))
       (not (v-voidp then-obj))
       (not (v-voidp else-obj))
       (not (v-discarded-p then-obj))
       (not (v-discarded-p else-obj))
       (not (v-returned-p then-obj))
       (not (v-returned-p else-obj))
       (< (+ (length (current-line test-obj))
             (length (current-line then-obj))
             (length (current-line else-obj)))
          100)))

(defun gen-string-for-ternary-form (test-obj then-obj else-obj)
  (values nil
          (format nil "(~a ? ~a : ~a)"
                  (current-line test-obj)
                  (current-line then-obj)
                  (current-line else-obj))))

(defun gen-string-for-if-form (test-obj then-obj else-obj primary-result-type
                               has-else)
  (flet ((needs-assign-p (obj)
           (and (not (v-voidp obj))
                (not (v-returned-p obj))
                (not (v-discarded-p obj)))))
    (let* ((will-assign (and (not (typep primary-result-type 'v-or))
                             (or (needs-assign-p then-obj)
                                 (needs-assign-p else-obj))))
           (tmp-var (when will-assign
                      (safe-glsl-name-string (gensym "if-tmp-"))))
           (then-chunk (gen-chunk-for-if-block then-obj tmp-var))
           (else-chunk (when has-else
                          (gen-chunk-for-if-block else-obj tmp-var))))
      (values
       (when (or then-chunk else-chunk)
         (glsl-chunk-splicing
           :chunk (to-block test-obj)
           :line (when tmp-var
                   (glsl-line
                    (prefix-type-to-string primary-result-type
                                           (end-line-str tmp-var))))
           :line (glsl-line "if (~a)" (current-line test-obj))
           :chunk (or then-chunk
                      (glsl-chunk
                       (glsl-line "{")
                       (glsl-line "}")))
           :line (when else-chunk (glsl-line "else"))
           :chunk else-chunk))
       (when will-assign
         tmp-var)))))

(defun gen-chunk-for-if-block (code-obj glsl-tmp-var-name)
  (let ((to-block (to-block code-obj))
        (current-line (current-line code-obj)))
    ;;
    (when (or current-line (not (glsl-chunk-emptyp to-block)))
      (glsl-chunk-splicing
        :line (glsl-line "{")
        :chunk (indent to-block)
        :line (when current-line
                (let ((current (end-line-str current-line)))
                  (indent
                   (glsl-line
                    (if (and glsl-tmp-var-name
                             (not (v-discarded-p code-obj))
                             (not (v-returned-p code-obj)))
                        (%gen-assignment-string glsl-tmp-var-name current)
                        current)))))
        :line (glsl-line "}")))))

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
