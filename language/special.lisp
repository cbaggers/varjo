;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(vdefspecial + (&rest args)    
  (let* ((arg-objs (mapcar #'varjo->glsl args))
         (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
        (merge-obs arg-objs
                   :type (apply #'superior-type types)
                   :current-line (format nil "(~{~a~^ ~^+~^ ~})"
                                         (mapcar #'current-line 
                                                 arg-objs)))
        (error "The types of object passed to + are not compatible~%~{~s~^ ~}" types))))

(vdefspecial %- (&rest args)    
  (let* ((arg-objs (mapcar #'varjo->glsl args))
         (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
        (merge-obs arg-objs
                   :type (apply #'superior-type types)
                   :current-line (format nil "(~{~a~^ ~^-~^ ~})"
                                         (mapcar #'current-line 
                                                 arg-objs)))
        (error "The types of object passed to - are not compatible~%~{~s~^ ~}" types))))

(vdefspecial / (&rest args)    
  (let* ((arg-objs (mapcar #'varjo->glsl args))
         (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
        (merge-obs arg-objs
                   :type (apply #'superior-type types)
                   :current-line (format nil "(~{~a~^ ~^/~^ ~})"
                                         (mapcar #'current-line 
                                                 arg-objs)))
        (error "The types of object passed to / are not compatible~%~{~s~^ ~}" types))))

(vdefspecial ? (test-form then-form &optional else-form)
  (let* ((test (varjo->glsl test-form))
         (t-obj (varjo->glsl then-form))
         (nil-obj (varjo->glsl else-form))
         (arg-objs (remove-if #'null (list test t-obj nil-obj))))
    (if (glsl-typep test '(:bool nil))
        (if (equal (code-type nil-obj) (code-type t-obj))
            (merge-obs 
             arg-objs
             :type (code-type nil-obj)
             :current-line (format nil "(~a ? ~a : ~a)"
                                   (current-line test)
                                   (current-line t-obj)
                                   (current-line nil-obj)))
            (error "Verjo: Both potential outputs must be of the same type"))
        (error "The result of the test must be a bool.~%~a"
               (code-type test)))))


;; [TODO] double check implications of typify in compile-let-forms
(vdefspecial for (var-form condition update &rest body)
  "(for (a 0) (< a 10) (++ a)
     (* a 2))"
  (if 
   (consp (first var-form))
   (error "for can only iterate over one variable")
   (destructuring-bind (form-objs new-vars)
       (compile-let-forms (list var-form) t)
     (let* ((form-obj (first form-objs))
            (*glsl-variables* (append new-vars *glsl-variables*))
            (con-ob (varjo->glsl condition))
            (up-ob (varjo->glsl update))
            (prog-ob (end-line (indent-ob (apply-special 'progn body)))))
       (if (and (null (to-block con-ob)) (null (to-block up-ob)))
           
           (merge-obs (list prog-ob form-obj)
                      :type :none
                      :current-line nil
                      :to-block 
                      (list
                       (fmt "~{~a~%~}for (~a;~a;~a) {~%~{~a~%~}    ~a~%}"
                            (to-block form-obj)
                            (current-line form-obj)
                            (current-line con-ob)
                            (current-line up-ob)
                            (to-block prog-ob)
                            (current-line prog-ob))))
           (error "Varjo: Only simple expressions are allowed in the condition and update slots of a for loop"))))))

(vdefspecial if (test-form then-form &optional else-form)  
  (let* ((test (varjo->glsl test-form))
         (t-obj (end-line (indent-ob (varjo->glsl then-form))))
         (nil-obj (when else-form (end-line (indent-ob (varjo->glsl else-form)))))
         (arg-objs (remove-if #'null (list test t-obj nil-obj))))
    (if (glsl-typep test '(:bool nil))
        (merge-obs 
         arg-objs
         :type :none
         :current-line nil
         :to-block 
         (list (if nil-obj
                   (format nil "~a~&if (~a) {~{~%~a~}~%    ~a~%} else {~{~%~a~}~%    ~a~%}"
                           (or (to-block test) "") 
                           (current-line test)
                           (or (to-block t-obj) nil) 
                           (current-line t-obj)
                           (or (to-block nil-obj) nil) 
                           (current-line nil-obj))
                   (format nil "~a~&if (~a) {~{~%~a~}~%    ~a~%}"
                           (or (to-block test) "") 
                           (current-line test)
                           (or (to-block t-obj) nil)
                           (current-line t-obj)))))
        (error "The result of the test must be a bool.~%~s"
               (code-type test)))))

(vdefspecial %init-vec-or-mat (type &rest args)
  (labels ((type-size (arg-type) 
             (let ((arg-type (type-principle arg-type)))
               (if (type-aggregate-p arg-type)
                   (type-component-count arg-type)
                   (if (eq arg-type (type-component-type type))
                       1
                       (error "Varjo: ~a is not of suitable type to be a component of ~a" 
                              arg-type type))))))
    (let* ((target-type (flesh-out-type type))
           (target-length (type-component-count target-type))
           (arg-objs (mapcar #'varjo->glsl args))
           (types (mapcar #'code-type arg-objs))
           (lengths (mapcar #'type-size types)))
      (if (eq target-length (apply #'+ lengths))
          (merge-obs arg-objs
                     :type target-type
                     :current-line 
                     (format nil "~a(~{~a~^,~^ ~})"
                             (varjo-type->glsl-type target-type)
                             (mapcar #'current-line arg-objs)))
          (error "The lengths of the types provided~%(~{~a~^,~^ ~})~%do not add up to the length of ~a" types target-type)))))

;; [TODO] Pretty sure this has a bug where if you use an in-built
;;        type with upper and lower case, this will just write 
;;        lower-case
(vdefspecial labels (func-specs &rest body)
  (let ((func-objs) (processed nil) (todo func-specs) (count 0))
    (loop :until (or (not todo) (>= count (length todo))) :do
       (let* ((*glsl-functions* (acons-many processed *glsl-functions*))
              (spec (first todo))
              (obj (handler-case (varjo->glsl (cons '%make-function spec))
                     (missing-function-error ()
                       (progn (setf todo `(,@(rest todo) ,(first todo)))
                              (incf count)
                              nil)))))
         (when obj
           (setf count 0)
           (push obj func-objs)
           (pop todo)
           (push (list (first spec) 
                       (vlambda :in-args (second spec)
                                :output-type (code-type obj)
                                :transform 
                                (format nil "~a(~{~a~^,~^ ~})"
                                        (safe-gl-name '-f (first spec))
                                        (loop for i below (length (second spec))
                                           :collect "~a"))))
                 processed))))
    (if todo
        (error "Functions unresolvable in labels block~{~%~s~}" todo)
        (let ((*glsl-functions* (acons-many processed *glsl-functions*)))
          (let ((prog-obj (apply-special 'progn body)))
            (merge-obs (append func-objs (list prog-obj))
                       :type (code-type prog-obj)
                       :current-line (current-line prog-obj)))))))

(vdefspecial let (form-code &rest body-code)
  ;; check for name clashes between forms
  ;; create init forms, for each one 
  (destructuring-bind (form-objs new-vars)
      (compile-let-forms form-code)
    (let* ((*glsl-variables* (append new-vars *glsl-variables*))
           (prog-ob (apply-special 'progn body-code)))
      (merge-obs (cons prog-ob form-objs)
                 :type (code-type prog-ob)
                 :current-line (current-line prog-ob)
                 :to-block (append 
                            (mapcan #'to-block form-objs)
                            (mapcar (lambda (x) 
                                      (current-line (end-line x)))
                                    form-objs)
                            (to-block prog-ob))
                 :to-top (append (mapcan #'to-top form-objs)
                                 (to-top prog-ob))))))

(vdefspecial %make-array (type length &optional contents)
  (let* ((literal-length (typep length 'code))
         (length (varjo->glsl length))
         (contents (mapcar #'varjo->glsl contents)))
    (merge-obs 
     (cons length contents)
     :type (flesh-out-type 
            `(,type ,(if literal-length
                         (parse-integer (current-line length))
                         t)))
     :current-line (format nil "~a[~a]{~{~a~^,~}}" 
                           type
                           (current-line length) 
                           (mapcar #'current-line contents)))))

;;[TODO] arg names arent always safe (try anything with a hyphon)
(vdefspecial %make-function (name args &rest body)
  (let ((name (if (eq name :main) :main (symb '-f name))))
    (destructuring-bind (form-objs new-vars)
        (compile-let-forms (mapcar #'list args) nil nil)
      (declare (ignore form-objs))
      (let* ((*glsl-variables* (append new-vars *glsl-variables*)) 
             (body-obj (indent-ob (apply-special 'progn body)))
             (name (if (eq name :main) :main name))
             (returns (returns body-obj))
             (type (if (eq name :main) '(:void nil nil) 
                       (first returns))))
        (let ((name (safe-gl-name name)))
          (if (or (not returns) (loop for r in returns always (equal r (first returns))))
              (make-instance 
               'code :type type
               :current-line nil
               :to-top (append 
                        (to-top body-obj)
                        (list (format 
                               nil "~a ~a(~(~{~{~a ~a~}~^,~^ ~}~)) {~%~{~a~%~}~@[    ~a~%~]}~%"
                               (varjo-type->glsl-type type)
                               name 
                               (mapcar #'reverse args)
                               (to-block body-obj) 
                               (current-line (end-line body-obj)))))

               :out-vars (out-vars body-obj))
              (error "Some of the return statements in function '~a' return different types~%~a~%~a" name type returns)))))))

(vdefspecial %make-var (name type)
  (make-instance 'code :type (set-place-t type)
                 :current-line (string name)))

(vdefspecial %negate (form)  
  (let* ((arg-obj (varjo->glsl form)))
    (merge-obs arg-obj
               :current-line (format nil "-~a"
                                     (current-line arg-obj)))))

(vdefspecial out (name-and-qualifiers form)
  (let ((arg-obj (varjo->glsl form))
        (out-var-name (if (consp name-and-qualifiers)
                          (first name-and-qualifiers)
                          name-and-qualifiers))
        (qualifiers (when (consp name-and-qualifiers)
                      (rest name-and-qualifiers))))
    (if (assoc out-var-name *glsl-variables*)
        (error "The variable name '~a' is already taken and so cannot be used~%for an out variable" out-var-name)
        (make-instance 'code
                       :type :void
                       :current-line (fmt "~a = ~a;" 
                                          (safe-gl-name out-var-name)
                                          (current-line arg-obj))
                       :to-block (to-block arg-obj)
                       :out-vars `((,out-var-name
                                    ,(code-type arg-obj)
                                    ,(safe-gl-name out-var-name) 
                                    ,@qualifiers))))))

(vdefspecial progn (&rest body)
  (let ((arg-objs (mapcar #'varjo->glsl body)))
    (cond 
      ((eq 0 (length arg-objs)) (make-none-ob))
      ((eq 1 (length arg-objs))
       (let ((ob (first arg-objs)))
         (merge-obs ob :current-line (current-line ob))))
      (t (let ((last-arg (car (last arg-objs)))
               (args (subseq arg-objs 0 (- (length arg-objs) 1))))
           (merge-obs arg-objs
                      :type (code-type last-arg)
                      :current-line (current-line last-arg)
                      :to-block 
                      (remove #'null
                              (append (loop for i in args
                                         for j in (mapcar #'end-line args)
                                         append (to-block i) 
                                         collect (current-line j))
                                      (to-block last-arg)))))))))

;; [TODO] why does this need semicolon?
(vdefspecial return (&optional (form '(%void)))
  (let ((ob (varjo->glsl form)))
    (if (eq :none (code-type ob))
        ob
        (merge-obs ob
                   :current-line (format nil "return ~a;" 
                                         (current-line ob))
                   :type :none
                   :returns (list (code-type ob))))))

(vdefspecial switch (test-form &rest clauses)    
  (let* ((test (varjo->glsl test-form))
         (keys (mapcar #'first clauses))
         (arg-objs (mapcar #'(lambda (x) (varjo->glsl (second x)))
                           clauses))
         (format-clauses 
          (loop :for key :in keys
             :for obj :in arg-objs
             :append
             (cond ((eq key 'otherwise) 
                    (list "default" nil "jam"))
                   ((glsl-typep key '(:int nil))
                    (list (current-line key)
                          (or (to-block obj) nil) 
                          (current-line obj)))))))
    (if (glsl-typep test '(:int nil))
        (merge-obs 
         arg-objs
         :type :none
         :current-line ""
         :to-block 
         (list 
          (format nil "~a~%switch (~a) {~{~%case ~a:~%~{~a~^~%~}~a;~%break;~}}"
                  (or (to-block test) "") 
                  (current-line test)
                  format-clauses)))
        (error "The result of the test must be an int.~%~s"
               (code-type test)))))

(vdefspecial %%typify (form)
  (let* ((arg (varjo->glsl form))
         (type (code-type arg)))
    (merge-obs arg :current-line 
               (format nil "<~a ~a>" type (current-line arg)))))

(vdefspecial %typify (form)
  (let* ((arg (varjo->glsl form))
         (type (code-type arg)))
    (merge-obs arg :current-line 
               (format nil "~a ~a" (varjo-type->glsl-type type)
                       (current-line arg)))))

(vdefspecial %in-typify (form &optional (qualifiers nil))
  (let* ((arg (varjo->glsl form))
         (type (code-type arg)))
    (merge-obs arg :current-line 
               (format nil "~a ~{~a ~}~a~@[[~a]~]" 
                       (varjo-type->glsl-type type)
                       qualifiers
                       (current-line arg)
                       (when (second type)
                         (if (numberp (second type))
                             (second type)
                             ""))))))

(vdefspecial while (test &rest body)
  (let* ((test-ob (varjo->glsl test))
         (prog-ob (end-line (apply-special 'progn body))))
    (merge-obs (list prog-ob test-ob)
               :type :none
               :current-line nil
               :to-block 
               (list
                (format nil "~{~a~%~}while (~a) {~%~{~a~%~}~a;~%}"
                        (to-block test-ob)
                        (current-line test-ob)
                        (to-block prog-ob)
                        (current-line prog-ob))))))

(vdefspecial swizzle (vec-form components)
  (let* ((vec-ob (varjo->glsl vec-form))
         (vec-type (code-type vec-ob)))
    (if (type-vec-core-type vec-type)
        (let* ((comp (string-downcase (string (if (listp components)
                                                  (cadr components)
                                                  components))))
               (len (length comp)))
          (if (<= len 4)
              (merge-obs (list vec-ob)	
                         :type (set-place-t (change-vec-length
                                             vec-type len))
                         :current-line (format nil "~a.~a"
                                               (current-line vec-ob)
                                               comp))
              (error "Varjo: Invlaid length of components for swizzle")))
        (error "Varjo: Trying to swizzle a non vector: ~a" vec-type))))
