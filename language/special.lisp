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
;; (v-defun test-1 ((a v-int) (b v-float))
;;   :special 
;;   :return (make-instance 'code :current-line "booyah!" 
;;                          :type (type-spec->type 'v-int)))

;; example for case 2
;; (v-defun test-2 (name args &rest body)
;;   :special
;;   :args-valid t
;;   :return (progn (format nil "name:~s args:~s body:~s" name args body)
;;                  (make-instance 'code :current-line "booyah!" 
;;                                 :type (type-spec->type 'v-int))))

;;[TODO] make it handle multiple assignements like cl version
(v-defun :setf ((place v-type) (val v-type))
  :special
  :return 
  (cond ((not (v-placep (code-type place)))
         (error 'non-place-assign :place place :val val))
        ((not (v-type-eq (code-type place) (code-type val)))
         (error 'setf-type-match :code-obj-a place :code-obj-b val))
        (t (merge-obs (list place val) :type (code-type place)
                      :current-line (gen-assignment-string place val)))))

;; what about types and such used in other values forms, cant guarentee that
;; they will be used and thus cant be sure they will be included in code obj 
;; later
(v-defun :values (&rest forms)
  :special
  :args-valid t
  :return
  (let* ((body-objs (loop :for code :in forms :collect (varjo->glsl code env)))
         (first-obj (first body-objs)))
    (setf (values-code first-obj) (rest body-objs)
          (signatures first-obj) (mapcar #'signatures body-objs)
          (to-block first-obj) (mapcar #'to-block body-objs)
          (to-top first-obj) (mapcar #'to-top body-objs)
          (out-vars first-obj) (mapcar #'out-vars body-objs)
          (used-types first-obj) (mapcar #'used-types body-objs)
          (stemcells first-obj) (mapcar #'stemcells body-objs)
          (invariant first-obj) (mapcar #'invariant body-objs)
          (returns first-obj) (mapcar #'returns body-objs)
          (used-external-functions first-obj) (mapcar #'used-external-functions
                                                      body-objs))
    (loop :for obj :in (values-code first-obj) :do
       (setf (signatures first-obj) nil (to-block first-obj) nil
             (to-top first-obj) nil (out-vars first-obj) nil
             (used-types first-obj) nil (stemcells first-obj) nil
             (invariant first-obj) nil (returns first-obj) nil
             (used-external-functions first-obj) nil))
    first-obj))

(v-defun :multiple-value-bind (vars form &body body)
  :special
  :args-valid t
  :return
  (let* ((form-obj (varjo->glsl form env)) 
         (objs (cons form-obj (values-code form-obj))) ;;remove values-code?
         (vals (if (< (length objs) (length vars))
                   (append objs (loop :for i :below (- (length objs) (length vars))
                                   :collect nil))
                   objs)))
    (varjo->glsl `(let ,(loop :for val :in vals :for var :in vars  )))))

(v-defun :progn (&rest body)
  ;; this is super important as it is the only function that implements 
  ;; imperitive coding. It does this my passing the env from one form
  ;; to the next.
  :special
  :args-valid t
  :return
  (values
   (let ((body-objs (loop :for code :in body :collect 
                       (multiple-value-bind (code-obj new-env)
                           (varjo->glsl code env)
                         (when new-env (setf env new-env))
                         code-obj))))
     (let ((last-obj (car (last body-objs)))
           (objs (subseq body-objs 0 (1- (length body-objs)))))
       (merge-obs body-objs
                  :type (code-type last-obj)
                  :current-line (current-line last-obj)
                  :to-block 
                  (remove #'null
                          (append (loop :for i :in objs
                                     :for j :in (mapcar #'end-line objs)
                                     :append (remove nil (to-block i)) 
                                     :append (listify (current-line j)));this should work
                                  (to-block last-obj))))))
   env))

(v-defun :%clean-env-block (&body body)
  :special
  :args-valid t
  :return (let ((new-env (clean-environment env)))
            (values (varjo->glsl `(progn ,@body) new-env))))

(v-defun :%clone-env-block (&body body)
  :special
  :args-valid t
  :return (let ((new-env (clone-environment env)))
            (values (varjo->glsl `(progn ,@body) new-env))))

;;[TODO] this should have a and &optional for place
;;[TODO] could this take a form and infer the type? yes...it could
;;       should destructively modify the env
(v-defun :%make-var (name-string type)
  :special
  :args-valid t
  :return (make-instance 'code :type (set-place-t type) 
                         :current-line name-string))


(v-defun :%typify (form &optional qualifiers)
  :special
  :args-valid t
  :return
  (let* ((code (varjo->glsl form env)))
    (merge-obs code :type (code-type code)
               :current-line (prefix-type-declaration code qualifiers))))

;;[TODO] Make this less ugly, if we can merge environments we can do this easily
(v-defun :%env-multi-var-declare (forms &optional include-type-declarations 
                                       arg-glsl-names)
  ;; This is the single ugliest thing in varjo (hopefully!)
  ;; it implements declarations of multiple values without letting
  ;; them share the environment.
  :special
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
                           (varjo->glsl `(%typify ,code) env)))))
      ;;add-vars to env - this is destrucitvely modifying env
      (loop :for (name type-spec qualifiers) :in var-specs 
         :for glsl-name :in glsl-names :for code-obj :in c-objs :do
         (let ((type-spec (when type-spec (type-spec->type type-spec))))
           (add-var name
                    (make-instance 'v-value :glsl-name glsl-name
                                   :type (or type-spec (code-type code-obj)))
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

(v-defun :%make-function (name raw-args &rest body)
  :special
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
         (returns (returns body-obj))        
         (type (if mainp (type-spec->type 'v-void) (first returns))))
    (unless (or mainp returns) (error 'no-function-returns :name name))
    (unless (loop :for r :in returns :always (v-type-eq r (first returns)))
      (error 'return-type-mismatch :name name :types type :returns returns))
    (let ((arg-pairs (loop :for (ignored type) :in raw-args
                        :for name :in arg-glsl-names :collect
                        `(,(v-glsl-string (type-spec->type type)) ,name))))
      (add-function name (func-spec->function 
                          (v-make-f-spec (gen-function-transform glsl-name
                                                                 raw-args)
                                         raw-args (mapcar #'second raw-args)
                                         type :glsl-name glsl-name) env) 
                    env t)
      (values (merge-obs 
               body-obj
               :type (type-spec->type 'v-none)
               :current-line nil
               :signatures (if mainp (signatures body-obj)
                               (cons (gen-function-signature glsl-name arg-pairs type)
                                     (signatures body-obj)))
               :to-top (cons-end (gen-function-body-string glsl-name arg-pairs type body-obj)
                                 (to-top body-obj))
               :to-block nil
               :returns nil
               :out-vars (out-vars body-obj))
              env))))

(v-defun :return (form)
  :special
  :args-valid t
  :return  
  (let ((obj (varjo->glsl form env)))
    (unless (null (values-code obj)) (error 'values-return))
    (merge-obs obj :type 'v-void
               :current-line (format nil "return ~a" (current-line obj))
               :returns (list (code-type obj)))))

(v-defmacro :labels (definitions &body body)
  `(%clone-env-block
    ,@(loop :for d :in definitions :collect `(%make-function ,@d))
    ,@body))

;; [TODO] what if tpye of form is not value
(v-defun :out (name-and-qualifiers form)
  :special
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

;; note that just like in lisp this only fails if false. 0 does not fail.
(v-defun :if (test-form then-form &optional else-form)
  :special
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

(v-defun :while (test &rest body)
  :special
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
(v-defun :switch (test-form &rest clauses)
  :special
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
(v-defun :swizzle (vec-form components)
  :special 
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
             (loop :for c :being :the :elements :of comp-string 
                :always (find c allowed)))
        (merge-obs vec-obj :type (type-spec->type 
                                  (p-symb 'varjo 'v-vec new-len))
                   :current-line (gen-swizzle-string vec-obj comp-string))
        (error "swizzle form invalid"))))


;;   (for (a 0) (< a 10) (++ a) 
;;     (* a 2))
;; [TODO] double check implications of typify in compile-let-forms
(v-defun :for (var-form condition update &rest body)
  :special 
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

