(in-package :varjo)

(defconstant +order-bias+ 0.0001)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun v-make-f-spec (transform args arg-types return-spec 
                      &key place glsl-spec-matching glsl-name)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (declare (ignore args))
    (list transform arg-types return-spec context place glsl-spec-matching 
          glsl-name)))
;;
;; {IMPORTANT NOTE} IF YOU CHANGE ONE-^^^^, CHANGE THE OTHER-vvvvv
;;
;;[TODO] split each case into a different macro and use this as core
;;[TODO] use make-func-spec so we have only one place where the spec
;;       is defined, this will lower the number of errors once we start
;;       editting things in the future
;;[TODO] This is the ugliest part of varjo now....sort it out!
(defmacro v-defun (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (subst '&rest '&body 
                      (if context-pos (subseq args 0 context-pos) args)
                      :test #'symbol-name-equal))
         (arg-names (lambda-list-get-names args)))
    (cond ((stringp (first body))
           (destructuring-bind (transform arg-types return-spec 
                                          &key place glsl-spec-matching glsl-name) body
             `(progn (add-function ',name '(,transform ,arg-types ,return-spec
                                            ,context ,place ,glsl-spec-matching 
                                            ,glsl-name)
                                   *global-env*)
                     ',name)))
          ((eq (first body) :special)
           (destructuring-bind (&key context place args-valid return)
               (rest body)
             (if (eq args-valid t)
                 `(progn 
                        (add-function ',name (list :special 
                                                   t
                                                   (lambda ,(cons 'env args) 
                                                     (declare (ignorable env ,@arg-names)) 
                                                     ,return)
                                                   ,context ,place nil nil)
                                      *global-env*)
                        ',name)
                 (if args-valid
                     `(progn 
                        (add-function ',name (list :special 
                                                   (lambda ,(cons 'env args)
                                                     (declare (ignorable env ,@arg-names))
                                                     (let ((res ,args-valid)) 
                                                       (when res (list res 0))))
                                                   (lambda ,(cons 'env args) 
                                                     (declare (ignorable env ,@arg-names)) 
                                                     ,return)                                               
                                                   ,context ,place nil nil)
                                      *global-env*)
                        ',name)
                     `(progn
                        (add-function ',name (list :special 
                                                   ',(mapcar #'second args)
                                                   (lambda ,(cons 'env (mapcar #'first args))
                                                     (declare (ignorable env ,@arg-names))
                                                     ,return)
                                                   ,context ,place nil nil)
                                      *global-env*)
                        ',name)))))
          (t `(progn (setf (gethash ',name (v-external-functions *global-env*))
                         '(,args ,@(rest body)))
                   ',name)))))
;; [TODO] ^^^^- we use setf rather than add for extended functions...this seems 
;;              dumb as it means we can have generic externals which are chosen
;;              based on usage. This also means some level of checking to see 
;;              if the arg spec matches.

;;------------------------------------------------------------

;;[TODO] Where should this live?
(defun get-stemcells (arg-objs final-types)
  (loop :for o :in arg-objs :for f :in final-types
     :if (typep (code-type o) 'v-stemcell) :collect `(,o ,f)))

;;[TODO] catch cannot-compiler errors only here
(defun try-compile-arg (arg env)
  (handler-case (varjo->glsl arg env)
    (varjo-error (e) (make-instance 'code :type (make-instance 'v-error :payload e)))))

;;[TODO] stemcells...how do we handle them?
(defun special-arg-matchp (func arg-code arg-objs arg-types any-errors env)
  (let ((method (v-argument-spec func))
        (env (clone-environment env)))
    (if (listp method)
        (when (not any-errors) (basic-arg-matchp func arg-types arg-objs))
        (if (eq method t)
            (list t func arg-code nil)
            (handler-case (list 0 func (apply method (cons env arg-code)) nil) 
              (varjo-error () nil))))))

(defun glsl-arg-matchp (func arg-types arg-objs)
  (let* ((spec-types (v-argument-spec func))
         (spec-generics (positions-if #'v-spec-typep spec-types))
         (g-dim (when spec-generics 
                  (v-dimensions (nth (first spec-generics) arg-types)))))    
    (when (and (eql (length arg-objs) (length spec-types))
               (or (null g-dim)
                   (loop :for i :in spec-generics :always 
                      (equal (v-dimensions (nth i arg-types)) g-dim))))
      (if (loop :for a :in arg-types :for s :in spec-types :always (v-typep a s))
          (list 0 func arg-objs (get-stemcells arg-objs spec-types))
          (let ((cast-types (loop :for a :in arg-types :for s :in spec-types 
                             :collect (v-casts-to a s))))
            (when (not (some #'null cast-types))
              (list 1 func (loop :for obj :in arg-objs :for type :in cast-types
                              :collect (copy-code obj :type type))
                    (get-stemcells arg-objs cast-types))))))))

;; [TODO] should this always copy the arg-objs?
(defun basic-arg-matchp (func arg-types arg-objs)
  (let ((spec-types (v-argument-spec func)))
    (when (eql (length arg-objs) (length spec-types))
      (if (loop :for a :in arg-types :for s :in spec-types :always (v-typep a s))
          (list 0 func arg-objs (get-stemcells arg-objs spec-types))
          (let ((cast-types (loop :for a :in arg-types :for s :in spec-types 
                               :collect (v-casts-to a s))))
            (when (not (some #'null cast-types))
              (list 1 func (loop :for obj :in arg-objs :for type :in cast-types
                              :collect (copy-code obj :type type))
                    (get-stemcells arg-objs cast-types))))))))

(defun find-functions-for-args (func-name args-code env &aux matches)
  (let (arg-objs arg-types any-errors (potentials (get-function func-name env)))
    (if potentials
        (loop :for func :in potentials :for bias-count :from 0 :do
           (when (and (not arg-objs) (func-need-arguments-compiledp func))
             (setf arg-objs (loop :for i :in args-code :collect 
                               (try-compile-arg i env)))
             (setf arg-types (mapcar #'code-type arg-objs))
             (setf any-errors (some #'v-errorp arg-types)))           
           (let ((match (if (v-special-functionp func) 
                            (special-arg-matchp func args-code arg-objs
                                                arg-types any-errors env)
                            (when (not any-errors)
                              (if (v-glsl-spec-matchingp func)
                                  (glsl-arg-matchp func arg-types arg-objs)
                                  (basic-arg-matchp func arg-types arg-objs))))))
             (if (eq (first match) t)
                 (return (list match))
                 (when match
                   (when (numberp (first match)) 
                     (setf (first match) (+ (first match)
                                            (* bias-count +order-bias+))))
                   (push match matches))))
           :finally (return (or matches (func-find-failure func-name arg-objs))))
        (error 'could-not-find-function :name func-name))))

;; if there were no candidates then pass errors back
(defun func-find-failure (func-name arg-objs)
  (loop :for arg-obj :in arg-objs
     :if (typep (code-type arg-obj) 'v-error) 
     :return `((t ,(code-type arg-obj) nil)) 
     :finally (return
                `((t ,(make-instance 'v-error :payload
                                         (make-instance 'no-valid-function
                                                        :name func-name
                                                        :types (mapcar #'code-type
                                                                       arg-objs)))
                         nil)))))

(defun find-function-for-args (func-name args-code env)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the 
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  (let* ((functions (find-functions-for-args func-name args-code env)))
    (destructuring-bind (score function arg-objs stemcells)
        (if (> (length functions) 1) 
            (first (sort functions #'< :key #'first))
            (first functions))
      (declare (ignore score))
      (error "You were working here, some of the returned arg-objs are stemcell type")
      (list function arg-objs stemcells))))

(defun glsl-resolve-func-type (func args)
  "nil - superior type
   number - type of nth arg
   function - call the function
   (:element n) - element type of nth arg
   list - type spec"
  (let ((spec (v-return-spec func))
        (arg-types (mapcar #'code-type args)))
    (cond ((null spec) (apply #'find-mutual-cast-type arg-types))
          ((typep spec 'v-type) spec)
          ((numberp spec) (nth spec arg-types))
          ((functionp spec) (apply spec args))
          ((and (listp spec) (eq (first spec) :element))
           (v-element-type (nth (second spec) arg-types)))
          ((or (symbolp spec) (listp spec)) (type-spec->type spec))
          (t (error 'invalid-function-return-spec :func func :spec spec)))))

;;[TODO] Maybe the error should be caught and returned, 
;;       in case this is a bad walk
(defun glsl-resolve-special-func-type (func args env)
  (let ((env (clone-environment env)))
    (multiple-value-bind (code-obj new-env)
        (handler-case (apply (v-return-spec func) (cons env args))
          (varjo-error (e) (invoke-debugger e)))
      (values code-obj (or new-env env)))))

;;------------------------------------------------------------
