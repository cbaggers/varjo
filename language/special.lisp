(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Regular Macros

(v-defmacro macrolet (definitons &rest body)
  (unless body (error 'body-block-empty :form-name 'macrolet))
  (reduce (lambda (accum definition)
            `(macrolet-1 ,definition ,accum))
          definitons
          :initial-value `(progn ,@body)))

(v-defspecial macrolet-1 (definition &rest body)
  :args-valid t
  :return
  (let ((macro
         (dbind (name lambda-list &body body) definition
           (declare (ignore lambda-list body))
           (make-regular-macro
            name
            (lambda (form env)
              (declare (ignore form env))
              (error "IMPLEMENT ME!"))
            nil
            env))))
    (with-fresh-env-scope (fresh-env env)
      (let ((new-env (add-form-binding macro fresh-env)))
        (compile-form `(progn ,@body) new-env)))))

;;------------------------------------------------------------
;; Symbol Macros

(v-defmacro symbol-macrolet (macrobindings &rest body)
  (unless body (error 'body-block-empty :form-name 'symbol-macrolet))
  (reduce (lambda (accum binding)
            (dbind (name expansion) binding
              `(symbol-macrolet-1 ,name ,expansion ,accum)))
          macrobindings
          :initial-value `(progn ,@body)))


(v-defspecial symbol-macrolet-1 (name expansion &rest body)
  :args-valid t
  :return
  (let* ((scope (v-function-scope env))
         (macro (make-symbol-macro expansion scope env)))
    (with-fresh-env-scope (fresh-env env)
      (let ((new-env (add-symbol-binding name macro fresh-env)))
        (compile-form `(progn ,@body) new-env)))))

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

(v-defspecial setf-1 ((place v-type) (val v-type))
  :return
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
                     final-env)))))))

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
  (restart-case (if (v-type-eq (v-type old-val) (code-type new-val))
                    (code-type new-val)
                    (error 'setq-type-match :var-name var-name
                           :old-value old-val :new-value new-val))
    (setq-supply-alternate-type (replacement-type-spec)
      (type-spec->type replacement-type-spec (flow-ids new-val)))))

(defun replace-flow-ids (old-var-name old-val flow-ids old-env env)
  (assert (typep old-val 'v-value))
  (labels ((w (n)
             (if (eq n old-env)
                 (env-replace-parent
                  n
                  (v-parent-env n)
                  :symbol-bindings
                  (a-add old-var-name
                         (v-make-value
                          (replace-flow-id (v-type old-val) flow-ids)
                          n
                          :read-only (v-read-only old-val)
                          :function-scope (v-function-scope old-val)
                          :glsl-name (v-glsl-name old-val))
                         (copy-list (v-symbol-bindings n))))
                 (env-replace-parent n (w (v-parent-env n))))))
    (if (or (eq old-env *global-env*) (typep old-env 'base-environment))
        env
        (w env))))

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
;; Progn

(v-defmacro prog1 (&body body)
  (let ((tmp (gensym "PROG1-TMP")))
    `(let ((,tmp ,(first body)))
       ,@(rest body)
       ,tmp)))

(v-defspecial progn (&rest body)
  ;; this is super important as it is the only function that implements
  ;; imperitive coding. It does this by passing the env from one form
  ;; to the next.
  ;; it also returns this mutated env
  :args-valid t
  :return
  (if body
      (merge-progn (compile-progn body env) env)
      (error 'empty-progn)))

;;------------------------------------------------------------
;; Multiple Values

(v-defspecial multiple-value-bind (vars value-form &rest body)
  :args-valid t
  :return
  (let* ((base (lisp-name->glsl-name 'mvb env))
         (new-env (fresh-environment env :multi-val-base base)))
    (let ((value-obj (compile-form value-form new-env)))
      (unless (= (length vars) (+ 1 (length (multi-vals value-obj))))
        (error 'multi-val-bind-mismatch :val-form value-form :bindings vars))
      (let* ((mvals (multi-vals value-obj))
             (v-vals (mapcar #'multi-val-value mvals))
             (types (cons (code-type value-obj) (mapcar #'v-type v-vals))))
        (vbind ((m-objs s-obj b-objs) final-env)
            (with-fresh-env-scope (fresh-env env)
              (env-> (p-env fresh-env)
                (%mapcar-multi-env-progn
                 (lambda (env type name i)
                   (compile-let name (type->type-spec type) nil env
                                (format nil "~a~a" base i)))
                 p-env types vars (iota (length types)))
                (compile-form `(setq ,(first vars) ,value-obj) p-env)
                (compile-progn body p-env)))
          (let* ((m-obj (%merge-multi-env-progn m-objs))
                 (merged (merge-progn `(,m-obj ,s-obj ,@b-objs)
                                      env final-env)))
            (values
             (copy-code
              merged
              :node-tree (ast-node! 'multiple-value-bind
                                    `(,vars ,(node-tree value-obj)
                                            ,@(mapcar #'node-tree b-objs))
                                    (code-type merged)
                                    env final-env))
             final-env)))))))

(v-defspecial varjo-lang:values-safe (form)
  ;; this special-form executes the form without destroying
  ;; the multi-return 'values' travalling up the stack.
  ;; Progn is implictly values-safe, but * isnt by default.
  ;;
  ;; it will take the values from whichever argument has them
  ;; if two of the arguments have them then values-safe throws
  ;; an error
  :args-valid t
  :return
  (if (listp form)
      (let ((safe-env (fresh-environment
                       env :multi-val-base (v-multi-val-base env)
                       :multi-val-safe t)))
        (vbind (c e) (compile-list-form form safe-env)
          (let* ((final-env (fresh-environment e :multi-val-safe nil))
                 (ast (ast-node! 'varjo-lang:values-safe
                                 (list (node-tree c))
                                 (code-type c)
                                 env
                                 final-env)))
            (values (copy-code c :node-tree ast)
                    final-env))))
      (compile-form form env )))

(v-defspecial values (&rest values)
  :args-valid t
  :return
  (if values
      (if (v-multi-val-base env)
          (%values values env)
          (expand-and-compile-form `(prog1 ,@values) env))
      (%values-void env)))

(defun %values (values env)
  (let* ((new-env (fresh-environment env :multi-val-base nil))
         (qualifier-lists (mapcar #'extract-value-qualifiers values))
         (forms (mapcar #'extract-value-form values))

         (objs (mapcar λ(compile-form _ new-env) forms))
         (base (v-multi-val-base env))
         (glsl-names (loop :for i :below (length forms) :collect
                        (format nil "~a~a" base i)))
         (vals (loop :for o :in objs :for n :in glsl-names :collect
                  (v-make-value (code-type o) env :glsl-name n)))
         (first-name (gensym))
         (result (expand-and-compile-form
                  `(let ((,first-name ,(first objs)))
                     ,@(loop :for o :in (rest objs)
                          :for v :in (rest vals) :collect
                          `(%assign ,v ,o))
                     ,first-name)
                  env))
         (ast (ast-node! 'values
                         (mapcar λ(if _1 `(,@_1 ,(node-tree _)) (node-tree _))
                                 objs
                                 qualifier-lists)
                         (code-type result) env env)))
    (values (copy-code result :multi-vals (mapcar #'make-mval (rest vals)
                                                  (rest qualifier-lists))
                       :node-tree ast)
            env)))

(defun %values-void (env)
  (let ((void (type-spec->type :void (flow-id!))))
    (values (code! :type void
                   :current-line nil
                   :node-tree (ast-node! 'values nil void env env))
            env)))

(defun extract-value-qualifiers (value-form)
  (when (and (listp value-form) (keywordp (first value-form)))
    (butlast value-form)))

(defun extract-value-form (value-form)
  (if (and (listp value-form) (keywordp (first value-form)))
      (last1 value-form)
      value-form))

;;------------------------------------------------------------
;; Return

(v-defspecial %return (form)
  :args-valid t
  :return
  (let ((new-env (fresh-environment
                  env
                  :multi-val-base "return")))
    ;; we create an environment with the signal to let any 'values' forms
    ;; down the tree know they will be caught and what their name prefix should
    ;; be.
    ;; We then compile the form using the augmented environment, the values
    ;; statements will expand and flow back as 'multi-vals' and the current-line
    ;; now there are two styles of return:
    ;; - The first is for a regular function, in which multivals become
    ;;   out-arguments and the current-line is returned
    ;; - The second is for a shader stage in which the multi-vars become
    ;;   output-variables and the current line is handled in a 'context'
    ;;   specific way.
    (let* ((code-obj (compile-form form new-env))
           (result
            (if (member :main (v-context env))
                (%main-return code-obj env)
                (%regular-value-return code-obj))))
      (values (copy-code result
                         :node-tree (ast-node! '%return (node-tree code-obj)
                                               (code-type result)
                                               env env))
              env))))

;; Used when this is a labels (or otherwise local) function
(defun %regular-value-return (code-obj)
  (let ((flow-result
         (if (multi-vals code-obj)
             (m-flow-id! (cons (flow-ids code-obj)
                               (mapcar (lambda (c)
                                         (flow-ids (multi-val-value c)))
                                       (multi-vals code-obj))))
             (flow-ids code-obj))))
    ;; {TODO} why not 'end-line' here? who is doing that?
    (let ((suppress-glsl (or (v-typep (code-type code-obj) 'v-void)
                             (v-typep (code-type code-obj)
                                      'v-ephemeral-type))))
      (copy-code
       code-obj :type (type-spec->type 'v-void flow-result)
       :current-line (unless suppress-glsl
                       (format nil "return ~a" (current-line code-obj)))
       :returns (cons (code-type code-obj) (multi-vals code-obj))
       :multi-vals nil
       :place-tree nil))))


;; Used when this is the main stage function
;; this
(defun %main-return (code-obj env)
  (if (multi-vals code-obj)
      (let* ((mvals (multi-vals code-obj))
             (v-vals (mapcar #'multi-val-value mvals))
             (types (mapcar #'v-type v-vals))
             (glsl-lines (mapcar #'v-glsl-name v-vals)))

        (merge-progn
         (with-fresh-env-scope (fresh-env env)
           (env-> (p-env fresh-env)
             (merge-multi-env-progn
              (%mapcar-multi-env-progn
               (lambda (p-env type gname)
                 (compile-let (gensym) (type->type-spec type)
                              nil p-env gname))
               p-env types glsl-lines))
             (compile-form (%default-out-for-stage code-obj p-env) p-env)
             (compile-form `(progn ,@(mapcar λ(mval->out-form _ env)
                                             (multi-vals code-obj)))
                           p-env)))
         env))
      (with-fresh-env-scope (fresh-env env)
        (compile-form (%default-out-for-stage code-obj env)
                      fresh-env))))

;; fragment comes first as it doesnt restrict the exit type...this is a bug
;; really as fragment out-var should be vec4...We should have a case for
;; when context includes all stages, in which case any type is allowed
(defun %default-out-for-stage (form env)
  (let ((context (v-context env)))
    (cond ((member :fragment context) `(%out (,(gensym "OUTPUT-COLOR"))
                                             ,form))
          ((member :vertex context) `(setq varjo-lang::gl-position ,form))
          (t `(%out (,(gensym "OUTPUT-VAR"))
                    ,form)))))

;; {TODO} what if type of form is not value
(v-defspecial %out (name-and-qualifiers form)
  :args-valid t
  :return
  (let* ((form-obj (compile-form form env))
         (out-var-name (if (consp name-and-qualifiers)
                           (first name-and-qualifiers)
                           name-and-qualifiers))
         (qualifiers (when (consp name-and-qualifiers)
                       (rest name-and-qualifiers)))
         (glsl-name (safe-glsl-name-string out-var-name))
         (type (type-spec->type :void (flow-id!))))
    (if (assoc out-var-name *glsl-variables*)
        (error 'out-var-name-taken :out-var-name out-var-name)
        (values
         (end-line
          (copy-code
           form-obj :type type
           :current-line (gen-out-var-assignment-string glsl-name form-obj)
           :to-block (to-block form-obj)
           :out-vars (cons `(,out-var-name
                             ,qualifiers
                             ,(v-make-value (code-type form-obj) env
                                            :glsl-name glsl-name))
                           (out-vars form-obj))
           :node-tree (ast-node! '%out (list name-and-qualifiers
                                             (node-tree form-obj))
                                 type env env)
           :multi-vals nil
           :place-tree nil) t)
         env))))

;;------------------------------------------------------------
;; Let

(v-defspecial let (bindings &rest body)
  :args-valid t
  :return
  (progn
    (unless body (error 'body-block-empty :form-name 'let))
    (vbind ((new-var-objs body-obj) final-env)
        (with-fresh-env-scope (fresh-env env)
          (env-> (p-env fresh-env)
            (%mapcar-multi-env-progn
             (lambda (p-env binding)
               (with-v-let-spec binding
                 (compile-let name type-spec value-form p-env)))
             p-env bindings)
            (compile-form `(progn ,@body) p-env)))
      (let* ((merged (merge-progn (list (merge-multi-env-progn new-var-objs)
                                        body-obj)
                                  env final-env))
             (val-ast-nodes (mapcar λ(unless (eq (node-tree _) :ignored)
                                       (list (node-tree _)))
                                    new-var-objs))
             (ast-args
              (list (mapcar λ(with-v-let-spec _
                               (if type-spec
                                   `((,name ,type-spec) ,@_1)
                                   `(,name ,@_1)))
                            bindings
                            val-ast-nodes)
                    (node-tree body-obj))))
        (values
         (copy-code merged :node-tree (ast-node! 'let ast-args (code-type merged)
                                                 env final-env))
         final-env)))))

(v-defmacro let* (bindings &rest body)
  (unless body (error 'body-block-empty :form-name 'let))
  (let* ((bindings (reverse bindings))
         (result `(let (,(first bindings)) ,@body)))
    (loop :for binding :in (rest bindings) :do
       (setf result `(let (,binding) ,result)))
    result))

;;------------------------------------------------------------
;; Labels, flet & labels-no-implicit

(v-defspecial labels (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) e)
      (with-fresh-env-scope (fresh-env env)
        (env-> (p-env fresh-env)
          (mapcar-progn
           (lambda (env d)
             (dbind (name args &rest body) d
               (vbind (fn code) (build-function name args body t env)
                 (values code (add-form-binding fn env)))))
           p-env definitions)
          (compile-form `(progn ,@body) p-env)))
    (assert body-obj)
    ;; can be nil in case of cvt funcs--↓↓↓
    (let* ((merged (merge-progn (remove nil (cons-end body-obj func-def-objs))
                                env e))
           (ast (ast-node!
                 'labels
                 (list (remove nil (mapcar λ(when _1
                                              (cons-end
                                               (node-tree _1)
                                               (subseq _ 0 2)))
                                           definitions
                                           func-def-objs))
                       (node-tree body-obj))
                 (code-type body-obj) env env)))
      (values (copy-code merged :node-tree ast)
              e))))

(v-defspecial flet (definitions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) e)
      (with-fresh-env-scope (fresh-env env)
        (env-> (p-env fresh-env)
          (%mapcar-multi-env-progn
           (lambda (env d)
             (dbind (name args &rest body) d
               (vbind (fn code) (build-function name args body t env)
                 (values code (add-form-binding fn env)))))
           p-env definitions)
          (compile-form `(progn ,@body) p-env)))
    (let* ((merged (merge-progn (remove nil (cons-end body-obj func-def-objs))
                                env e))
           (ast (ast-node!
                 'flet
                 (list (remove nil (mapcar λ(when _1
                                              (cons-end
                                               (node-tree _1)
                                               (subseq _ 0 2)))
                                           definitions
                                           func-def-objs))
                       (node-tree body-obj))
                 (code-type body-obj) env env)))
      (values (copy-code merged :node-tree ast)
              e))))

(v-defspecial labels-no-implicit (definitions exceptions &rest body)
  :args-valid t
  :return
  (vbind ((func-def-objs body-obj) pruned-starting-env) ;;ending-env
      (with-fresh-env-scope (fresh-env env)
        ;;
        (env-> (p-env fresh-env)
          (mapcar-progn (lambda (env d)
                          (dbind (name args &rest body) d
                            (vbind (fn code)
                                (build-function name args body exceptions env)
                              (values code (add-form-binding fn env)))))
                        p-env definitions)
          (compile-form `(progn ,@body) p-env)))
    ;;
    (let* ((merged (merge-progn
                    (remove nil (cons-end body-obj func-def-objs))
                    env
                    pruned-starting-env))
           (ast (ast-node!
                 'labels-no-implicit
                 (list (remove nil (mapcar λ(if _1
                                                (cons-end
                                                 (node-tree _1)
                                                 (subseq _ 0 2))
                                                _)
                                           definitions
                                           func-def-objs))
                       exceptions
                       (node-tree body-obj))
                 (code-type body-obj) env env)))
      (values (copy-code merged :node-tree ast)
              pruned-starting-env))))

;;------------------------------------------------------------
;;

;; pretty sure env is wrong in 'or and 'and, what if there are side effects in
;; forms?
;; In fact function calls in general should at least propagate the flow ids
;; down the arg compiles..could even the env be passed? may just work
(v-defspecial or (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (compile-form x env)) forms))
         (flow-id (flow-id!)))
    (unless (loop for o in objs always (v-code-type-eq o (first objs)))
      (error "all forms of an 'OR' form must resolve to the same type"))
    (if (v-typep (code-type (first objs)) (type-spec->type :bool))
        (values (merge-obs objs
                           :type (type-spec->type :bool flow-id)
                           :current-line (gen-bool-or-string objs)
                           :node-tree (ast-node! 'or (mapcar #'node-tree objs)
                                                 (type-spec->type :bool)
                                                 env env))
                env)
        (values (first objs) env))))

(v-defspecial and (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (compile-form x env)) forms))
         (flow-id (flow-id!)))
    (unless (loop for o in objs always (v-code-type-eq o (first objs)))
      (error "all forms of an 'AND' form must resolve to the same type"))
    (if (v-typep (code-type (first objs)) (type-spec->type :bool))
        (values (merge-obs objs
                           :type (type-spec->type :bool flow-id)
                           :current-line (gen-bool-and-string objs)
                           :node-tree (ast-node! 'and (mapcar #'node-tree objs)
                                                 (type-spec->type :bool)
                                                 env env))
                env) ;; pretty sure this env is wrong, what if side effects in
        ;;              forms?
        (values (last1 objs) env))))

;;------------------------------------------------------------
;; If

(v-defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       (values)))

(v-defmacro unless (test &body body)
  `(if (not ,test)
       (progn ,@body)
       (values)))

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
         (tmp-var (when will-assign (safe-glsl-name-string 'tmp))))
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
;; Iteration

;;   (for (a 0) (< a 10) (++ a)
;;     (* a 2))
(v-defspecial for (var-form condition update &rest body)
  :args-valid t
  :return
  (if (consp (first var-form))
      (error 'for-loop-only-one-var)
      (multiple-value-bind (code new-env)
          (with-v-let-spec var-form
            (compile-let name type-spec value-form env))
        (let* ((var-string (subseq (first (to-block code))
                                   0
                                   (1- (length (first (to-block code))))))
               (decl-obj (compile-form (second var-form) new-env))
               (condition-obj (compile-form condition new-env))
               (update-obj (compile-form update new-env)))
          (unless (or (v-typep (code-type decl-obj) 'v-uint)
                      (v-typep (code-type decl-obj) 'v-int)
                      (v-typep (code-type decl-obj) 'v-float))
            (error 'invalid-for-loop-type :decl-obj decl-obj))
          (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body) new-env)
            (if (and (null (to-block condition-obj)) (null (to-block update-obj)))
                (let ((loop-str (gen-for-loop-string
                                 var-string condition-obj update-obj
                                 (end-line body-obj)))
                      (void (type-spec->type :void (flow-id!))))
                  (values (copy-code
                           body-obj :type void
                           :current-line nil
                           :to-block (list loop-str)
                           :node-tree (ast-node!
                                       'for (cons var-form
                                                  (mapcar #'node-tree
                                                          (list condition-obj
                                                                update-obj
                                                                body-obj)))
                                       void env final-env)
                           :multi-vals nil
                           :place-tree nil)
                          final-env))
                (error 'for-loop-simple-expression)))))))


(v-defspecial while (test &rest body)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test env)
    (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body)
                                                             test-env)
      (if (v-typep (code-type test-obj) 'v-bool)
          (let ((type (type-spec->type :void (flow-id!))))
            (values (merge-obs (list body-obj test-obj)
                               :type type
                               :current-line nil
                               :to-block (list (gen-while-string
                                                test-obj (end-line body-obj)))
                               :node-tree (ast-node!
                                           'while (mapcar #'node-tree
                                                          (list test-obj
                                                                body-obj))
                                           type env final-env))
                    final-env))
          (error 'loop-will-never-halt :test-code test :test-obj test-obj)))))

(defun search-for-flow-id-fixpoint (code starting-env)
  ;; Lets document this a bit and work out how to debug it from a crash
  (labels ((names-to-new-flow-bindings (x)
             (let ((binding (get-symbol-binding x nil starting-env)))
               ;; This should never be an issue as we are working from
               ;; #'get-new-flow-ids which itself works on variables.
               ;;                       ↓↓↓↓
               (assert (not (typep binding 'v-symbol-macro)))
               `(,x . ,(flow-ids binding)))))
    ;;
    (let ((envs (list starting-env))
          (last-code-obj nil)
          (flow-ids nil)
          (checkpoint (checkpoint-flow-ids)))
      (loop :for pass :from 0
         :for current-env = (first envs)
         :until (vbind (o new-env) (compile-form code current-env)
                  (let* ((new-flow-ids (get-new-flow-ids new-env current-env))
                         (f-ids (or flow-ids
                                    (mapcar #'names-to-new-flow-bindings
                                            (mapcar #'car new-flow-ids)))))
                    (setf last-code-obj o
                          envs (cons new-env envs)
                          flow-ids (accumulate-flow-ids f-ids new-flow-ids))
                    (let ((done (fixpoint-reached
                                 new-flow-ids starting-env pass)))
                      (unless done (reset-flow-ids-to-checkpoint checkpoint))
                      done))))
      (values last-code-obj
              (create-post-loop-env flow-ids starting-env)))))

;; defun replace-flow-ids (old-var-name old-val flow-ids old-env env)
(defun create-post-loop-env (new-flow-id-pairs starting-env)
  (labels ((splice-in-flow-id (accum-env id-pair)
             (dbind (vname . new-flow-id) id-pair
               (vbind (old-val old-env) (get-symbol-binding vname nil accum-env)
                 (replace-flow-ids vname old-val new-flow-id
                                   old-env accum-env)))))
    (reduce #'splice-in-flow-id new-flow-id-pairs :initial-value starting-env)))

(defun accumulate-flow-ids (flow-ids new-flow-ids)
  (labels ((x (accum y)
             (dbind (vname . fid) y
               (acons vname (flow-id! (assocr vname accum)
                                      fid)
                      accum))))
    (remove-duplicates
     (reduce #'x new-flow-ids :initial-value flow-ids)
     :test #'eq :key #'first :from-end t)))

(defvar *max-resolve-loop-flow-id-pass-count* 100)

(defun get-new-flow-ids (latest-env last-env)
  (let* ((variables-changed (find-env-bindings latest-env last-env
                                               :test (complement #'eq)
                                               :stop-at-base t
                                               :variables-only t))
         ;; now we need to take these a remove any which have the
         ;; same flow-id. This can happen if a variable is set to
         ;; itself from within a loop
         (trimmed-changes
          ;; We don't worry about recieving a macro here as we called
          ;; find-env-bindings with :variables-only t
          ;;                           ↓↓↓↓↓↓↓↓
          (mapcar λ(let ((last-var (get-symbol-binding _ nil last-env))
                         (new-var (get-symbol-binding _ nil latest-env)))
                     (unless (or (not last-var)
                                 (not new-var)
                                 (id= (flow-ids last-var)
                                      (flow-ids new-var)))
                       _))
                  variables-changed)))
    (mapcar λ`(,_ . ,(flow-ids (get-symbol-binding _ nil latest-env)))
            (remove nil trimmed-changes))))

(defun fixpoint-reached (new-flow-ids starting-env pass)
  (unless (< pass *max-resolve-loop-flow-id-pass-count*)
    (error 'loop-flow-analysis-failure))
  (let* ((variables-changed (mapcar #'car new-flow-ids)))
    (or
     ;; if no variable from outer scope changed then we stop
     (not variables-changed)
     ;; if none of the variables that we changed were set to
     ;; values from the outer scope we stop (as information
     ;; has stopped flowing into the loop, there is nothing
     ;; else to glean)
     (let* ((starting-flow-ids (mapcar λ(flow-ids (get-symbol-binding _ nil starting-env))
                                       variables-changed))
            (starting-super-id (reduce #'flow-id! starting-flow-ids)))
       (not (some λ(id~= _ starting-super-id)
                  (mapcar #'cdr new-flow-ids)))))))

;;------------------------------------------------------------
;; Swizzle

(v-defmacro s~ (&rest args) `(swizzle ,@args))
(v-defspecial swizzle (vec-form components)
  :args-valid t
  :return
  (let* ((vec-obj (compile-form vec-form env))
         (allowed (subseq (list #\x #\y #\z #\w) 0
                          (first (v-dimensions (code-type vec-obj)))))
         (comp-string (if (keywordp components)
                          (string-downcase (symbol-name components))
                          (error 'swizzle-keyword :item components)))
         (new-len (length comp-string))
         (vec-type (code-type vec-obj))
         (element-type (v-element-type vec-type)))
    (if (and (>= new-len 1) (<= new-len 4)
             (v-typep vec-type 'v-vector)
             (loop :for c :across comp-string
                :always (find c allowed)))
        (let* ((flow-id (flow-id!))
               (r-type (set-flow-id (if (= new-len 1)
                                        element-type
                                        (vec-of element-type new-len))
                                    flow-id)))
          (values
           (copy-code vec-obj :type r-type
                      :current-line (gen-swizzle-string vec-obj comp-string)
                      :node-tree (ast-node! 'swizzle
                                            `(,(node-tree vec-obj) ,components)
                                            r-type env env)
                      :multi-vals nil
                      :place-tree nil)
           env))
        (error "swizzle form invalid"))))


;;------------------------------------------------------------
;; Types

(v-defspecial the (type-name form)
  :args-valid t
  :return
  (let* ((compiled (compile-form form env))
         (obj (if (stemcellp (code-type compiled))
                  (add-type-to-stemcell-code compiled type-name)
                  (if (v-typep (code-type compiled)
                               (type-spec->type type-name))
                      compiled ;{TODO} proper error here
                      (error "Incorrect declaration that ~a was of type ~a"
                             compiled type-name)))))
    (values
     (copy-code
      obj
      :node-tree (ast-node! 'the (list type-name (node-tree compiled))
                            (code-type compiled) env env))
     env)))


;;------------------------------------------------------------
;; Debugging Compilation

(v-defspecial %break (&optional datum &rest args)
  :args-valid t
  :return
  (progn
    (break (format nil "Varjo compiler breakpoint:~%~s" (or datum ""))
           (mapcar λ(compile-form _ env) args))
    (let* ((none-type (gen-none-type))
           (node (make-code-obj
                  none-type nil
                  :node-tree (ast-node! :break (cons datum args)
                                        none-type nil nil))))
      (values node env))))

(v-defspecial %peek (form)
  :args-valid t
  :return
  (vbind (o e) (compile-form form env)
    (break "Varjo Peek:~%:code-obj ~s~%:env ~s" o e)
    (values o e)))


;;------------------------------------------------------------
;; Inline GLSL

(v-defspecial glsl-expr (glsl-string type-spec)
  :args-valid t
  :return
  (values
   (compile-glsl-expression-string glsl-string type-spec)
   env))

(defun compile-glsl-expression-string (current-line type)
  (let* ((flow-id (flow-id!))
         (type-obj (set-flow-id (if (typep type 'v-type)
                                    type
                                    (type-spec->type type))
                                flow-id)))
    (code! :type type-obj
           :current-line current-line
           :used-types (list type-obj)
           :node-tree (ast-node! 'glsl-string nil type-obj nil nil))))

(defun glsl-let (name-symbol name-string type value-form env)
  (let ((type-spec (if (typep type 'v-type) (type->type-spec type) type)))
    (compile-let name-symbol type-spec value-form env name-string)))


;;------------------------------------------------------------
;; First class functions

;; {TODO} qualify the arg types to disambiguate from overloads.
;; {TODO} proper error
(v-defspecial function (func-name)
  :args-valid t
  :return
  (let ((func (find-form-binding-by-literal func-name env)))
    (etypecase func
      (v-regular-macro (error "Varjo: Although legal in CL, Varjo does not allow taking a reference to a macro function"))
      (external-function (%function-for-external-funcs func func-name env))
      (v-function (%function-for-regular-funcs func-name func env))
      (v-function-set (%function-for-func-sets func-name func env)))))

(defun %function-for-func-sets (func-name-form func-set env)
  (let ((functions (functions func-set)))
    (let* ((type (v-type-of func-set)))
      (when (or (some #'implicit-args functions)
                (and (some #'captured-vars functions)))
        (error 'closures-not-supported :func func-name-form))
      (values
       (code! :type type
              :current-line nil
              :used-types (list type)
              :node-tree (ast-node! 'function (list func-name-form)
                                    type nil nil))
       env))))

;; {TODO} shouldnt this have a new environment?
(defun %function-for-external-funcs (func func-name-form env)
  (record-func-usage func env)
  (compile-with-external-func-in-scope func `(function ,func-name-form) env))

(defun %function-for-regular-funcs (func-name-form func env)
  (let* ((flow-id (flow-id!))
         (type (set-flow-id (v-type-of func) flow-id)))
    (when (implicit-args func)
      (error 'closures-not-supported :func func-name-form))
    (values
     (code! :type type
            :current-line nil
            :used-types (list type)
            :node-tree (ast-node! 'function (list func-name-form)
                                  type nil nil))
     env)))

;;------------------------------------------------------------
