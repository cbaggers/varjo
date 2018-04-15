(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Case

(v-defspecial case (key-form &rest clauses)
  :args-valid t
  :return
  (vbind (key-obj key-env) (compile-form key-form env)
    (assert (not (v-voidp key-obj)) () 'void-type-for-conditional-test
            :kind "case"
            :form key-form)
    (assert (not (v-discarded-p key-obj)) () 'discarded-for-conditional-test
            :kind "case"
            :form key-form)
    (if clauses
        (let* ((form `(case ,key-form ,@clauses))
               (key-prim-type (primary-type key-obj))
               (clause-test-forms (mapcar #'first clauses))
               (otherwise-pos (position 'otherwise clause-test-forms))
               (otherwise (when otherwise-pos
                            (rest (nth otherwise-pos clauses))))
               (clause-test-forms (mapcar #'first clauses))
               (clauses-sans-otherwise
                (if otherwise-pos
                    (butlast clauses)
                    clauses)))
          (when otherwise-pos
            (assert (= otherwise-pos (- (length clauses) 1)) ()
                    "Varjo: 'otherwise' in an invalid position in typecase~%~a"
                    form))
          (assert (every (lambda (x) (or (constantp x) (eq x 'otherwise)))
                         clause-test-forms)
                  () "For now every clause test needs to be constantp. bah")
          (let* ((clauses-obj-primtype-body-env
                  (loop :for (test . body) :in clauses-sans-otherwise
                     :for ntest := test
                     :collect (vbind (obj env) (compile-form ntest key-env)
                                (list obj (primary-type obj) body env))))
                 (clauses-obj-primtype-body-env
                  (remove-if-not λ(v-type-eq (second _) key-prim-type)
                                 clauses-obj-primtype-body-env)))
            (assert (or clauses-obj-primtype-body-env otherwise-pos) ()
                    "No clause matches the type ~a" key-prim-type)
            (if (valid-for-switch-statementp key-prim-type)
                (gen-case-using-switch key-obj key-env
                                       clauses-obj-primtype-body-env
                                       otherwise env)
                (gen-case-using-cond key-obj key-env
                                     clauses-obj-primtype-body-env
                                     otherwise))))
        (compile-form '(values) env))))

(defun gen-case-using-cond (key-obj
                            key-env
                            clauses-obj-primtype-body-env
                            otherwise)
  (let ((gkey (gensym "case-key-form")))
    (compile-form
     `(let ((,gkey ,key-obj))
        (cond
          ,@(loop :for (obj nil body) :in clauses-obj-primtype-body-env
               :collect `((eql ,gkey ,obj) ,@body))
          ,@(when otherwise
              `((t ,@otherwise)))))
     key-env)))

(defun gen-case-using-switch (key-obj key-env clauses-obj-primtype-body-env
                              otherwise starting-env)
  (dbind (test-objs body-objs body-envs)
      (loop :for quad :in clauses-obj-primtype-body-env
         :for test-obj := (first quad)
         :for body := (third quad)
         :for pair := (multiple-value-list
                       (compile-form `(progn ,@body) key-env))
         :collect test-obj :into tobjs
         :collect (first pair) :into bobjs
         :collect (second pair) :into envs
         :finally (return (list tobjs bobjs envs)))
    (vbind (othwise-obj othwise-env)
        (when otherwise
          (compile-form `(progn ,@otherwise) key-env))
      (let* ((envs-with-otherwise (if otherwise
                                      ;; {TODO} does order matter here?
                                      (cons othwise-env body-envs)
                                      body-envs))
             (final-env
              (reduce #'env-merge-history
                      (env-prune-many (env-depth key-env)
                                      envs-with-otherwise)))
             (type-set
              (compute-conditional-type-set body-objs
                                            othwise-obj
                                            starting-env)))
        (vbind (to-block current-line)
            (gen-string-for-switch-form key-obj test-objs body-objs othwise-obj
                                        (primary-type type-set))
          (let ((return-sets (remove nil (mapcar #'return-set body-objs))))
            ;; this next check is to preempt a possible return-type-mismatch
            ;; error. The reason we do this is to give a better error.
            (when (and (some #'v-voidp return-sets)
                       (some (complement #'v-voidp) return-sets))
              (error 'conditional-return-type-mismatch
                     :sets return-sets)))
          (values (merge-compiled (cons key-obj body-objs)
                                  :type-set type-set
                                  :current-line current-line
                                  :to-block to-block)
                  final-env))))))

(defun gen-string-for-switch-form (key-obj test-objs body-objs othwise-obj
                                   primary-result-type)
  (flet ((needs-assign-p (obj)
           (and othwise-obj
                (not (v-voidp obj))
                (not (v-returned-p obj))
                (not (v-discarded-p obj)))))
    (let* ((will-assign (and (not (typep primary-result-type 'v-or))
                             (or (some #'needs-assign-p body-objs)
                                 (needs-assign-p othwise-obj))))
           (tmp-var (when will-assign
                      (safe-glsl-name-string (gensym "switch-tmp-"))))
           (arm-chunks (mapcar λ(gen-chunk-for-conditional-arm _0 _1 tmp-var)
                               test-objs
                               body-objs))
           (otherwise-chunk (when othwise-obj
                              (gen-chunk-for-default-arm othwise-obj
                                                         tmp-var))))
      (values
       (when (or arm-chunks otherwise-chunk)
         (glsl-chunk-splicing
           :chunk (to-block key-obj)
           :line (when tmp-var
                   (glsl-line
                    (prefix-type-to-string primary-result-type
                                           (end-line-str tmp-var))))
           :line (glsl-line "switch (~a)" (current-line key-obj))
           :line (glsl-line "{")
           :chunk (join-glsl-chunks arm-chunks)
           :chunk otherwise-chunk
           :line (glsl-line "}")))
       (when will-assign
         tmp-var)))))

(defun gen-chunk-for-conditional-arm (test-obj body-obj glsl-tmp-var-name)
  (let ((to-block (to-block body-obj))
        (current-line (current-line body-obj)))
    ;;
    (when (or current-line (not (glsl-chunk-emptyp to-block)))
      (glsl-chunk-splicing
        :line (glsl-line "case (~a):" (current-line test-obj))
        :chunk (indent to-block)
        :line (when current-line
                (let ((current (end-line-str current-line)))
                  (indent
                   (glsl-line
                    (if (and glsl-tmp-var-name
                             (not (v-discarded-p body-obj))
                             (not (v-returned-p body-obj)))
                        (%gen-assignment-string glsl-tmp-var-name current)
                        current)))))))))

(defun gen-chunk-for-default-arm (body-obj glsl-tmp-var-name)
  (let ((to-block (to-block body-obj))
        (current-line (current-line body-obj)))
    ;;
    (glsl-chunk-splicing
      :line (glsl-line "default:")
      :chunk (indent to-block)
      :line (when current-line
              (let ((current (end-line-str current-line)))
                (indent
                 (glsl-line
                  (if (and glsl-tmp-var-name
                           (not (v-discarded-p body-obj))
                           (not (v-returned-p body-obj)))
                      (%gen-assignment-string glsl-tmp-var-name current)
                      current))))))))

(defun compute-conditional-type-set (body-objs otherwise-body-obj env)
  ;; The type-set needs to include all the results
  ;; so we need to do two things.
  ;; - assert the same number of values returned
  ;; - make v-or types for each pair
  ;; - add logic to multiple-value-bind & co to check
  ;;   for v-or types and complain
  ;; Only need this if multi-val-base is not nil
  (let* ((type-sets (mapcar #'type-set body-objs))
         (type-sets (if otherwise-body-obj
                        (cons-end (type-set otherwise-body-obj) type-sets)
                        (cons-end (make-type-set) type-sets)))
         (terminated (mapcar #'v-terminated-p type-sets))
         (num-terminated (count-if #'identity terminated)))
    (cond
      ((v-multi-val-base env)
       (assert (loop :for set :in (rest type-sets) :always
                  (= (length set) (length (first type-sets))))
               () 'conditional-multiple-vals-mismatch
               :kind "case"
               :sets type-sets)
       (make-type-set*
        (loop :for i :below (length (first type-sets)) :collect
           (gen-or-type (loop :for set :in type-sets :collect
                           (aref set i))
                        (flow-id!)))))
      ((= num-terminated 1)
       (find-if-not #'v-terminated-p type-sets))
      (t
       (let ((primary-result-type
              (gen-or-type (mapcar #'primary-type body-objs) (flow-id!))))
         (if (v-voidp primary-result-type)
             (make-type-set)
             (make-type-set primary-result-type)))))))

(defun valid-for-switch-statementp (key-primary-type)
  (or (v-typep key-primary-type 'v-integer)
      (v-typep key-primary-type :float)
      (v-typep key-primary-type :double)))

;;------------------------------------------------------------
