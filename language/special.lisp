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
;;                          :type (make-instance 'v-int)))

;; example for case 2
;; (v-defun test-2 (name args &rest body)
;;   :special
;;   :args-valid t
;;   :return (progn (format nil "name:~s args:~s body:~s" name args body)
;;                  (make-instance 'code :current-line "booyah!" 
;;                                 :type (make-instance 'v-int))))

;;[TODO] make it handle multiple assignements like cl version
(v-defun setf ((place v-type) (val v-type))
  :special
  :return 
  (if (v-placep (code-type place))
      (merge-obs (list place val) :type (code-type place)
                 :current-line (gen-assignment-string place val))
      (error 'non-place-assign :place place :val val)))

(v-defun progn (&rest body)
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
                                     :append (to-block i) 
                                     :collect (current-line j))
                                  (to-block last-obj))))))
   env))

(v-defun %new-env-block (&body body)
  :special
  :args-valid t
  :return (let ((new-env (clone-environment env)))
            (values (varjo->glsl `(progn ,@body) new-env))))

(v-defun %clean-env-block (&body body)
  :special
  :args-valid t
  :return (let ((new-env (clone-environment env)))
            (values (varjo->glsl `(progn ,@body) new-env))))

;;[TODO] this should have a and &optional for place
(v-defun %make-var (name type)
  :special
  :args-valid t
  :return (make-instance 'code :type (set-place-t type) 
                         :current-line (lisp-name->glsl-name name)))


(v-defun %typify (form &optional qualifiers)
  :special
  :args-valid t
  :return
  (let* ((code (varjo->glsl form env)))
    (merge-obs code :type (code-type code)
               :current-line (prefix-type-declaration code qualifiers))))

;;[TODO] Make this less ugly, if we can merge environments we can do this easily
(v-defun %env-multi-var-declare (forms &optional include-type-declarations)
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
           (glsl-names (loop :for (name) :in var-specs
                          :do (when (> (count name var-specs :key #'first) 1)
                                (error 'duplicate-name name))
                          :collect (free-name name env)))
           (decl-objs (loop :for (name type-spec qualifiers) :in var-specs
                         :for code-obj :in c-objs :do
                         (validate-var-types name type-spec code-obj)
                         :collect
                         (let* ((type-spec (when type-spec (type-spec->type type-spec)))
                                (code (if code-obj
                                          `(setf (%make-var ,name ,(or type-spec (code-type code-obj))) ,code-obj)
                                          `(%make-var ,name ,type-spec))))
                           (varjo->glsl `(%typify ,code) env)))))
      ;;add-vars to env - this is destrucitvely modifying env
      (loop :for (name type-spec qualifiers) :in var-specs 
         :for glsl-name :in glsl-names :for code-obj :in c-objs :do
         (let ((type-spec (when type-spec (type-spec->type type-spec))))
           (add-var glsl-name
                    (make-instance 'v-value :type (or type-spec (code-type code-obj)))
                    env t)))
      (values (if include-type-declarations                  
                  (merge-obs decl-objs
                             :type (make-instance 'v-none)
                             :current-line ""
                             :to-block (append (mapcan #'to-block decl-objs)
                                               (mapcar (lambda (x) (end-line x))
                                                       decl-objs))
                             :to-top (mapcan #'to-top decl-objs))
                  (make-instance 'code :type 'v-none))
              env))))

;; [TODO] is block the best term? is it a block in the code-obj sense?
(v-defmacro let (bindings &body body)
  `(%new-env-block
    (%env-multi-var-declare ,bindings t)
    ,@body))

(v-defmacro let* (bindings &rest body)
  (let* ((bindings (reverse bindings))
         (result `(let (,(first bindings)) ,@body)))
    (loop :for binding :in (rest bindings) :do
       (setf result `(let (,binding) ,result)))
    result))

;;[TODO] move error
;;[TODO] ignoreing the arg-obs seems dumb, surely we need that data 
;;       for something
(v-defun %make-function (name raw-args &rest body)
  :special
  :args-valid t
  :return
  (let* ((args (mapcar #'list raw-args)) ;;so we can just use let
         (body-obj (varjo->glsl `(%clean-env-block 
                                  (%env-multi-var-declare ,args nil)
                                  ,@body) env))
         (name (if (eq name :main) :main (free-name name env)))
         (returns (returns body-obj))        
         (type (if (eq name :main) '(:void nil nil) (first returns))))
    (unless returns (error 'no-function-returns :name name))
    (unless (loop :for r :in returns :always (v-type-eq r (first returns)))
      (error 'return-type-mismatch name type returns))
    (let ((arg-pairs (loop :for (name type) :in raw-args :collect
                        `(,(v-glsl-string (type-spec->type type)) ,name))))
      (add-function 
       name (func-spec->function 
             (v-make-f-spec (gen-function-transform name raw-args) raw-args
                            (mapcar #'second raw-args) type)) env t)
      (values (make-instance 
               'code :type type
               :current-line nil
               :signatures (cons (gen-function-signature name arg-pairs type)
                                 (signatures body-obj))
               :to-top (cons-end (gen-function-body-string name arg-pairs type body-obj)
                                 (to-top body-obj))
               :out-vars (out-vars body-obj))
              env))))

(v-defun break ()
  :special
  :args-valid t
  :return (break))

(v-defun return (form)
  :special
  :args-valid t
  :return
  (let ((obj (varjo->glsl form env)))
    (merge-obs obj :type 'v-void
               :current-line (format nil "return ~a" (current-line obj))
               :returns (list (code-type obj)))))

(v-defmacro labels (definitions &body body)
  `(%new-env-block
    ,@(loop :for d :in definitions :collect `(%make-function ,@d))
    ,@body))

;; [TODO] what if tpye of form is not value
(v-defun out (name-and-qualifiers form)
  :special
  :args-valid t
  :return
  (let* ((form-obj (varjo->glsl form env))
         (out-var-name (if (consp name-and-qualifiers) 
                           (first name-and-qualifiers)
                           name-and-qualifiers))
         (qualifiers (when (consp name-and-qualifiers)
                       (rest name-and-qualifiers)))
         (glsl-name (free-name out-var-name env)))
    (if (assoc out-var-name *glsl-variables*)
        (error 'out-var-name-taken out-var-name)
        (make-instance 
         'code :type 'v-none
         :current-line (gen-out-var-assignment-string glsl-name form-obj)
         :to-block (to-block form-obj)
         :out-vars (cons `(,out-var-name 
                           ,qualifiers
                           ,(make-instance 'v-value :type (code-type form-obj)))
                         (out-vars form-obj))))))

;; note that just like in lisp this only fails if false. 0 does not fail.
(v-defun if (test-form then-form &optional else-form)
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

(v-defun while (test &rest body)
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
(v-defun switch (test-form &rest clauses)
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
                   :current-line ""
                   :to-block (list (gen-switch-string test-obj keys
                                                      clause-body-objs)))
        (error 'switch-type-error test-obj keys))))

;; (vdefspecial swizzle (vec-form components)
;;   (let* ((vec-ob (varjo->glsl vec-form))
;;          (vec-type (code-type vec-ob)))
;;     (if (type-vec-core-type vec-type)
;;         (let* ((comp (string-downcase (string (if (listp components)
;;                                                   (cadr components)
;;                                                   components))))
;;                (len (length comp)))
;;           (if (<= len 4)
;;               (merge-obs (list vec-ob)	
;;                          :type (set-place-t (change-vec-length
;;                                              vec-type len))
;;                          :current-line (format nil "~a.~a"
;;                                                (current-line vec-ob)
;;                                                comp))
;;               (error "Varjo: Invlaid length of components for swizzle")))
;;         (error "Varjo: Trying to swizzle a non vector: ~a" vec-type))))

;; ;; [TODO] double check implications of typify in compile-let-forms
;; (vdefspecial for (var-form condition update &rest body)
;;   "(for (a 0) (< a 10) (++ a)
;;      (* a 2))"
;;   (if 
;;    (consp (first var-form))
;;    (error "for can only iterate over one variable")
;;    (destructuring-bind (form-objs new-vars)
;;        (compile-let-forms (list var-form) t)
;;      (let* ((form-obj (first form-objs))
;;             (*glsl-variables* (append new-vars *glsl-variables*))
;;             (con-ob (varjo->glsl condition))
;;             (up-ob (varjo->glsl update))
;;             (prog-ob (end-line (indent-ob (apply-special 'progn body)))))
;;        (if (and (null (to-block con-ob)) (null (to-block up-ob)))
           
;;            (merge-obs (list prog-ob form-obj)
;;                       :type :none
;;                       :current-line nil
;;                       :to-block 
;;                       (list
;;                        (fmt "~{~a~%~}for (~a;~a;~a) {~%~{~a~%~}    ~a~%}"
;;                             (to-block form-obj)
;;                             (current-line form-obj)
;;                             (current-line con-ob)
;;                             (current-line up-ob)
;;                             (to-block prog-ob)
;;                             (current-line prog-ob))))
;;            (error "Varjo: Only simple expressions are allowed in the condition and update slots of a for loop"))))))


;; (vdefspecial %make-array (type length &optional contents)
;;   (let* ((literal-length (typep length 'code))
;;          (length (varjo->glsl length))
;;          (contents (mapcar #'varjo->glsl contents)))
;;     (merge-obs 
;;      (cons length contents)
;;      :type (flesh-out-type 
;;             `(,type ,(if literal-length
;;                          (parse-integer (current-line length))
;;                          t)))
;;      :current-line (format nil "~a[~a]{~{~a~^,~}}" 
;;                            type
;;                            (current-line length) 
;;                            (mapcar #'current-line contents)))))

;; (vdefspecial %init-vec-or-mat (type &rest args)
;;   (labels ((type-size (arg-type) 
;;              (let ((arg-type (type-principle arg-type)))
;;                (if (type-aggregate-p arg-type)
;;                    (type-component-count arg-type)
;;                    (if (eq arg-type (type-component-type type))
;;                        1
;;                        (error "Varjo: ~a is not of suitable type to be a component of ~a" 
;;                               arg-type type))))))
;;     (let* ((target-type (flesh-out-type type))
;;            (target-length (type-component-count target-type))
;;            (arg-objs (mapcar #'varjo->glsl args))
;;            (types (mapcar #'code-type arg-objs))
;;            (lengths (mapcar #'type-size types)))
;;       (if (eq target-length (apply #'+ lengths))
;;           (merge-obs arg-objs
;;                      :type target-type
;;                      :current-line 
;;                      (format nil "~a(~{~a~^,~^ ~})"
;;                              (varjo-type->glsl-type target-type)
;;                              (mapcar #'current-line arg-objs)))
;;           (error "The lengths of the types provided~%(~{~a~^,~^ ~})~%do not add up to the length of ~a" types target-type)))))


;; ;; [TODO] first argument should always be the environment
;; ;;        or maybe that is implicitly available
;; ;; [TODO] work out if we need to care about &optional &rest etc

;; (vdefspecial ? (test-form then-form &optional else-form)
;;   (let* ((test (varjo->glsl test-form))
;;          (t-obj (varjo->glsl then-form))
;;          (nil-obj (varjo->glsl else-form))
;;          (arg-objs (remove-if #'null (list test t-obj nil-obj))))
;;     (if (glsl-typep test '(:bool nil))
;;         (if (equal (code-type nil-obj) (code-type t-obj))
;;             (merge-obs 
;;              arg-objs
;;              :type (code-type nil-obj)
;;              :current-line (format nil "(~a ? ~a : ~a)"
;;                                    (current-line test)
;;                                    (current-line t-obj)
;;                                    (current-line nil-obj)))
;;             (error "Verjo: Both potential outputs must be of the same type"))
;;         (error "The result of the test must be a bool.~%~a"
;;                (code-type test)))))
