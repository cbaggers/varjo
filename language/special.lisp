;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;; special functions need to specify:
;; :args-valid
;;  * if not present then the types from the arguments are used
;;    with basic type checking. the args are then populated with
;;    the compiled code-objects for the :return section
;;  * if set to t any arguments are accepted and the raw code is
;;    passed to the :return section.
;;  * you can specify code here and it will be used to validate the
;;    arguments, you must return a list of compiled arg objects if successfull
;;    and you MUST throw an error if not.
;; Finally if you modify the environment in :return you must return the new
;; environment in the second value position

;; example for case 1
;; (v-defspecial test-1 ((a v-int) (b v-float))
;; ;;   :return (make-instance 'code :current-line "booyah!"
;;                          :type (type-spec->type 'v-int)))

;; example for case 2
;; (v-defspecial test-2 (name args &rest body)
;; ;;   :args-valid t
;;   :return (progn (format nil "name:~s args:~s body:~s" name args body)
;;                  (make-instance 'code :current-line "booyah!"
;;                                 :type (type-spec->type 'v-int))))

;;[TODO] make it handle multiple assignements like cl version
(v-defspecial :setf ((place v-type) (val v-type))
  :return
  (cond ((not (v-placep (code-type place)))
         (error 'non-place-assign :place place :val val))
        ((not (v-type-eq (code-type place) (code-type val)))
         (error 'setf-type-match :code-obj-a place :code-obj-b val))
        (t (merge-obs (list place val) :type (code-type place)
                      :current-line (gen-assignment-string place val)))))

(v-defspecial :progn (&rest body)
  ;; this is super important as it is the only function that implements
  ;; imperitive coding. It does this by passing the env from one form
  ;; to the next.
  :args-valid t
  :return
  (values
   (let ((body-objs (loop :for code :in body :collect
                       (multiple-value-bind (code-obj new-env)
                           (varjo->glsl code env)
                         (when new-env (setf env new-env))
                         code-obj))))
     (let ((last-obj (last1 body-objs)))
       (merge-obs body-objs
                  :type (code-type last-obj)
                  :current-line (current-line last-obj)
                  :to-block (merge-lines-into-block-list body-objs)
                  :multi-vals (multi-vals (last1 body-objs)))))
   env))

(v-defspecial :progn1 (&rest body)
  :args-valid t
  :return
  (let ((tmp (free-name 'progn-var)))
    (expand->varjo->glsl `(let ((,tmp ,(first body))) ,@(rest body) ,tmp) env)))

(v-defspecial :multiple-value-bind (vars value-form &rest body)
  :args-valid t
  :return
  (let ((new-env (clone-environment env)))
    (unless (every #'symbolp vars)
      (error "multiple-value-bind: all vars must symbols")) ;{TODO} proper error
    (setf (v-multi-val-base new-env) (safe-glsl-name-string (free-name 'a)))
    (let* ((val-obj (varjo->glsl value-form new-env))
           (mvals (multi-vals val-obj))
           (mval-types (mapcar #'v-type mvals))
           (glsl-names (mapcar #'v-glsl-name mvals)))
      (unless (= (length vars) (length mvals))
        (error "Length mismatch between values form and value-bind:~%~s~%~s"
               vars mvals))
      (let ((lvars (mapcar #'(lambda (x y) (list (list x (type->type-spec y))))
                           vars mval-types)))
        (varjo->glsl `(%clone-env-block
                       (%env-multi-var-declare ,lvars t ,glsl-names)
                       ,val-obj
                       ,@body)
                     env)))))

(v-defspecial :return (form)
  :args-valid t
  :return
  (let ((new-env (clone-environment env)))
    (setf (v-multi-val-base new-env) "return") ;;{TODO} something from the banned list
    (let* ((obj (varjo->glsl form new-env))
           (mvals (multi-vals obj))
           (mval-types (mapcar #'v-type mvals))
           (result
            (if mvals
                (merge-obs
                 obj :type 'v-void
                 :current-line (format nil "return ~a" (v-glsl-name (first mvals)))
                 :returns mval-types)
                (merge-obs
                 obj :type 'v-void
                 :current-line (format nil "return ~a" (current-line obj))
                 :returns (list (code-type obj))))))
      result)))

(v-defspecial :values (&rest values)
  :args-valid t
  :return
  (if (v-multi-val-base env)
      (let* ((new-env (clone-environment env))
             (objs (mapcar (lambda (x) (varjo->glsl x new-env)) values))
             (names (loop for i in values :collect (free-name 'v)))
             (base (v-multi-val-base env))
             (glsl-names (loop :for i :below (length values) :collect
                            (format nil "~a~a" base i)))
             (bindings (mapcar (lambda (x y) (list (list x) y))  names values))
             (result (varjo->glsl
                      `(%clone-env-block
                        (%env-multi-var-declare ,bindings :env-and-set ,glsl-names))
                      (clone-environment env))))
        (setf (multi-vals result)
              (loop :for o :in objs :for n in glsl-names :collect
                 (make-instance 'v-value :glsl-name n :type (code-type o))))
        result)
      (varjo->glsl `(progn1 ,@values) env)))


(v-defspecial :%clean-env-block (&rest body)
  :args-valid t
  :return (let ((new-env (clean-environment env)))
            (values (varjo->glsl `(progn ,@body) new-env))))

(v-defspecial :%clone-env-block (&rest body)
  :args-valid t
  :return (let ((new-env (clone-environment env)))
            (values (varjo->glsl `(progn ,@body) new-env))))

;;[TODO] this should have a and &optional for place
;;[TODO] could this take a form and infer the type? yes...it could
;;       should destructively modify the env
(v-defspecial :%make-var (name-string type)
  :args-valid t
  :return (make-instance 'code :type (set-place-t type)
                         :current-line name-string))


(v-defspecial :%typify (form &optional qualifiers)
  :args-valid t
  :return
  (let ((code (varjo->glsl form env)))
    (merge-obs code :type (code-type code)
               :current-line (prefix-type-declaration code qualifiers))))

;;[TODO] Make this less ugly, if we can merge environments we can do this easily
(v-defspecial :%env-multi-var-declare (forms &optional include-type-declarations
                                       arg-glsl-names)
  ;; This is the single ugliest thing in varjo (hopefully!)
  ;; it implements declarations of multiple values without letting
  ;; them share the environment.
  :args-valid t
  :return
  (labels ((validate-var-types (var-name type-spec code-obj)
             (when (and (null type-spec) (null code-obj))
               (error "Could not establish the type of the variable: ~s" var-name))
             (when (and code-obj type-spec (not (v-type-eq (code-type code-obj) type-spec)))
               (error "Type specified does not match the type of the form~%~s~%~s"
                      (code-type code-obj) type-spec))
             t))
    ;; compile vals
    (let* ((forms (remove nil forms))
           (env (clone-environment env))
           (var-specs (loop :for f :in forms :collect (listify (first f))))
           (c-objs (loop :for f in forms :collect
                      (when (> (length f) 1) (varjo->glsl (second f) env))))
           (glsl-names (or arg-glsl-names
                           (loop :for (name) :in var-specs
                              :do (when (> (count name var-specs :key #'first) 1)
                                    (error 'duplicate-name name))
                              :collect (safe-glsl-name-string (free-name name env)))))
           (decl-objs (loop :for (name type-spec qualifiers) :in var-specs
                         :for glsl-name :in glsl-names
                         :for code-obj :in c-objs :do
                         (validate-var-types name type-spec code-obj)
                         :collect
                         (let* ((type-spec (when type-spec (type-spec->type type-spec)))
                                (code (if code-obj
                                          `(setf (%make-var ,glsl-name ,(or type-spec (code-type code-obj))) ,code-obj)
                                          `(%make-var ,glsl-name ,type-spec))))
                           (if (eq include-type-declarations :env-and-set)
                               (varjo->glsl code env)
                               (varjo->glsl `(%typify ,code) env))))))
      ;;add-vars to env - this is destrucitvely modifying env
      (loop :for (name type-spec qualifiers) :in var-specs
         :for glsl-name :in glsl-names :for code-obj :in c-objs :do
         (let ((type-spec (when type-spec (type-spec->type type-spec))))
           (add-var name
                    (make-instance 'v-value :glsl-name glsl-name
                                   :type (set-place-t (or type-spec
                                                          (code-type code-obj))))
                    env t)))
      (values (if include-type-declarations
                  (merge-obs decl-objs
                             :type (type-spec->type 'v-none)
                             :current-line nil
                             :to-block (append (mapcan #'to-block decl-objs)
                                               (mapcar #'(lambda (x) (current-line (end-line x)))
                                                       decl-objs))
                             :to-top (mapcan #'to-top decl-objs))
                  (make-instance 'code :type 'v-none))
              env))))

;; [TODO] is block the best term? is it a block in the code-obj sense?
(v-defmacro :let (bindings &body body)
  `(%clone-env-block
    (%env-multi-var-declare ,bindings t)
    ,@body))

(v-defmacro :let* (bindings &rest body)
  (let* ((bindings (reverse bindings))
         (result `(let (,(first bindings)) ,@body)))
    (loop :for binding :in (rest bindings) :do
       (setf result `(let (,binding) ,result)))
    result))

(v-defspecial :%make-function (name raw-args &rest body)
  :args-valid t
  :return
  (let* ((mainp (eq name :main))
         (args (mapcar #'list raw-args))
         (arg-glsl-names (loop :for (name) :in raw-args :collect
                            (safe-glsl-name-string (free-name name))))
         (body-obj (varjo->glsl `(,(if mainp 'progn '%clean-env-block)
                                   (%env-multi-var-declare ,args nil ,arg-glsl-names)
                                   ,@body) env))
         (glsl-name (safe-glsl-name-string (if mainp name (free-name name))))
         (primary-return (first (returns body-obj)))
         (multi-return-vars (rest (returns body-obj)))
         (type (if mainp (type-spec->type 'v-void) primary-return)))

    (unless (or mainp primary-return) (error 'no-function-returns :name name))
    (add-function
     name (func-spec->function
           (v-make-f-spec name
                          (gen-function-transform glsl-name raw-args
                                                  multi-return-vars)
                          raw-args (mapcar #'second raw-args)
                          type :glsl-name glsl-name
                          :multi-return-vars multi-return-vars) env) env t)

    (let* ((arg-pairs (loop :for (ignored type) :in raw-args
                         :for name :in arg-glsl-names :collect
                         `(,(v-glsl-string (type-spec->type type)) ,name)))
           (out-arg-pairs (loop :for type :in multi-return-vars :for i :from 1
                             :for name = (fmt "return~a" i) :collect
                             `(,(v-glsl-string type) ,name)))
           (sigs (if mainp
                     (signatures body-obj)
                     (cons (gen-function-signature glsl-name arg-pairs
                                                   out-arg-pairs type)
                           (signatures body-obj))))
           (top (cons-end (gen-function-body-string
                           glsl-name arg-pairs out-arg-pairs type body-obj)
                          (to-top body-obj))))

      (values (merge-obs body-obj
                         :type (type-spec->type 'v-none)
                         :current-line nil
                         :signatures sigs
                         :to-top top
                         :to-block nil
                         :returns nil
                         :out-vars (out-vars body-obj)
                         :multi-vals nil)
              env))))

(v-defmacro :labels (definitions &body body)
  `(%clone-env-block
    ,@(loop :for d :in definitions :collect `(%make-function ,@d))
    ,@body))

;; [TODO] what if type of form is not value
(v-defspecial :out (name-and-qualifiers form)
  :args-valid t
  :return
  (let* ((form-obj (varjo->glsl form env))
         (out-var-name (if (consp name-and-qualifiers)
                           (first name-and-qualifiers)
                           name-and-qualifiers))
         (qualifiers (when (consp name-and-qualifiers)
                       (rest name-and-qualifiers))))
    (if (assoc out-var-name *glsl-variables*)
        (error 'out-var-name-taken out-var-name)
        (end-line
         (merge-obs
          form-obj :type 'v-none
          :current-line (gen-out-var-assignment-string out-var-name form-obj)
          :to-block (to-block form-obj)
          :out-vars (cons `(,out-var-name
                            ,qualifiers
                            ,(make-instance 'v-value :type (code-type form-obj)))
                          (out-vars form-obj))) t))))

(v-defspecial :assert (kind form error-message)
  :args-valid t
  :return
  (case kind
    ((:valid-variable '(:valid-variable))
     (and (symbolp form)
          (handler-case (varjo->glsl form env)
            (varjo-error () (error error-message)))))
    (otherwise (error "Unknown assert kind ~s" kind))))

;; note that just like in lisp this only fails if false. 0 does not fail.
(v-defspecial :if (test-form then-form &optional else-form)
  :args-valid t
  :return
  (let* ((test-obj (varjo->glsl test-form env))
         (then-obj (end-line (varjo->glsl then-form env)))
         (else-obj (when else-form (end-line (varjo->glsl else-form env))))
         (arg-objs (remove-if #'null (list test-obj then-obj else-obj))))
    (when (not (v-typep (code-type test-obj) 'v-bool))
      (setf test-obj (varjo->glsl t env))
      (setf else-obj nil))
    (if (v-typep (code-type test-obj) 'v-bool)
        (merge-obs arg-objs :type :none :current-line nil
                   :to-block (list (gen-if-string test-obj then-obj else-obj)))
        (error "The result of the test must be a bool.~%~s"
               (code-type test-obj)))))

(v-defspecial :while (test &rest body)
  :args-valid t
  :return
  (let* ((test-obj (varjo->glsl test env))
         (body-obj (varjo->glsl `(progn ,@body) env)))
    (if (v-typep (code-type test-obj) 'v-bool)
        (merge-obs (list body-obj test-obj)
                   :type 'v-none :current-line nil
                   :to-block (list (gen-while-string test-obj body-obj)))
        (error 'loop-will-never-halt :test-code test :test-obj test-obj))))


;; [TODO] check keys
(v-defspecial :switch (test-form &rest clauses)
  :args-valid t
  :return
  (let* ((test-obj (varjo->glsl test-form env))
         (keys (mapcar #'first clauses))
         (clause-body-objs (mapcar #'(lambda (x) (varjo->glsl
                                                  `(progn ,(second x)) env))
                           clauses)))
    (if (and (v-typep (code-type test-obj) 'v-i-ui)
             (loop :for key :in keys :always
                (or (eq key 'default) (integerp key))))
        (merge-obs clause-body-objs :type 'v-none
                   :current-line nil
                   :to-block (list (gen-switch-string test-obj keys
                                                      clause-body-objs)))
        (error 'switch-type-error test-obj keys))))

(v-defmacro :s~ (&rest args) `(swizzle ,@args))
(v-defspecial :swizzle (vec-form components)
  :args-valid t
  :return
  (let* ((vec-obj (varjo->glsl vec-form env))
         (allowed (subseq (list #\x #\y #\z #\w) 0
                          (first (v-dimensions (code-type vec-obj)))))
         (comp-string (if (keywordp components)
                          (string-downcase (symbol-name components))
                          (error 'swizzle-keyword :item components)))
         (new-len (length comp-string)))
    (if (and (>= new-len 2) (<= new-len 4)
             (v-typep (code-type vec-obj) 'v-vector)
             (loop :for c :across comp-string
                :always (find c allowed)))
        (merge-obs vec-obj :type (type-spec->type
                                  (p-symb 'varjo 'v-vec new-len))
                   :current-line (gen-swizzle-string vec-obj comp-string))
        (error "swizzle form invalid"))))


;;   (for (a 0) (< a 10) (++ a)
;;     (* a 2))
;; [TODO] double check implications of typify in compile-let-forms
(v-defspecial :for (var-form condition update &rest body)
  :args-valid t
  :return
  (if (consp (first var-form))
      (error 'for-loop-only-one-var)
      (multiple-value-bind (code new-env)
          (varjo->glsl `(%env-multi-var-declare (,var-form) t) env)
        (let* ((var-string (subseq (first (to-block code)) 0 (1- (length (first (to-block code))))))
               (decl-obj (varjo->glsl (second var-form) new-env))
               (condition-obj (varjo->glsl condition new-env))
               (update-obj (varjo->glsl update new-env))
               (body-obj (end-line (varjo->glsl `(progn ,@body) new-env))))
          (unless (typep (code-type decl-obj) 'v-i-ui)
            (error 'invalid-for-loop-type decl-obj))
          (if (and (null (to-block condition-obj)) (null (to-block update-obj)))
              (merge-obs
               body-obj :type 'v-none :current-line nil
               :to-block `(,(gen-for-loop-string var-string condition-obj
                                                 update-obj body-obj)))
              (error 'for-loop-simple-expression))))))


(v-defspecial :the (type-name form)
  :args-valid t
  :return
  (let ((compiled (varjo->glsl form env))
        (declared-type (type-spec->type type-name)))
    (if (stemcellp (code-type compiled))
        (copy-code compiled
                   :type declared-type
                   :stemcells (let ((stemcell (first (stemcells compiled))))
                                `((,(first stemcell)
                                    ,(second stemcell)
                                    ,type-name))))
        (unless (v-typep compiled declared-type)
          (error "Incorrect declaration that ~a was of type ~a"
                 compiled type-name))))) ;{TODO} proper error here
