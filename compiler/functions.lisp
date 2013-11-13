(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun v-make-f-spec (transform args arg-types return-spec 
                      &key place glsl-spec-matching)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (declare (ignore args))
    `(,transform ,arg-types ,return-spec ,context ,place ,glsl-spec-matching)))
;;
;; {IMPORTANT NOTE} IF YOU CHANGE ONE-^^^^, CHANGE THE OTHER-vvvvv
;;
;;[TODO] split each case into a different macro and use this as core
;;[TODO] use make-func-spec so we have only one place where the spec
;;       is defined, this will lower the number of errors once we start
;;       editting things in the future
(defmacro v-defun (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (cond ((stringp (first body))
           (destructuring-bind (transform arg-types return-spec 
                                          &key place glsl-spec-matching) body
             `(progn (add-function ',name '(,transform ,arg-types ,return-spec
                                            ,context ,place ,glsl-spec-matching)
                                   *global-env*)
                     ',name)))
          ((eq (first body) :special)
           (destructuring-bind (&key context place args-valid return)
               (rest body)
             (if args-valid
                 `(progn 
                    (add-function ',name (list :special 
                                               (lambda ,args
                                                 (declare (ignorable ,@args))
                                                 (let ((res ,args-valid)) 
                                                   (when res (list res 0))))
                                               (lambda ,args ,return)
                                               ,context ,place nil)
                                  *global-env*)
                    ',name)
                 `(progn
                    (add-function ',name (list :special 
                                               ',(mapcar #'second args)
                                               (lambda ,(mapcar #'first args)
                                                 (declare (ignorable ,@(mapcar #'first args)))
                                                 ,return)
                                               ,context ,place nil)
                                  *global-env*)
                    ',name))))
          (t `(progn (setf (gethash ',name (v-external-functions *global-env*))
                         '(,args ,@(rest body)))
                   ',name)))))
;; [TODO] ^^^^- we use setf rather than add for extended functions...this seems 
;;              dumb as it means we can have generic externals which are chosen
;;              based on usage. This also means some level of checking to see 
;;              if the arg spec matches.

;;------------------------------------------------------------

;;[TODO] catch cannot-compiler errors only here
(defun try-compile-arg (arg env)
  (handler-case (varjo->glsl arg env)
    (error () (make-instance 'code :type (make-instance 'v-error)))))

(defun special-arg-matchp (func arg-code arg-objs arg-types any-errors)
  (let ((method (v-argument-spec func)))
    (if (listp method)
        (when (not any-errors) (basic-arg-matchp func arg-types arg-objs))
        (handler-case (list 0 func (apply method arg-code)) 
          (error () nil)))))

(defun glsl-arg-matchp (func arg-types arg-objs)
  (let* ((spec-types (v-argument-spec func))
         (spec-generics (positions-if #'v-spec-typep spec-types))
         (g-dim (when spec-generics 
                  (v-dimensions (nth (first spec-generics) arg-types)))))
    (when (or (null g-dim)
              (loop :for i :in spec-generics :always 
                 (equal (v-dimensions (nth i arg-types)) g-dim)))
      (if (loop :for a :in arg-types :for s :in spec-types :always (v-typep a s))
          (list 0 func arg-objs)
          (let ((cast-types (loop :for a :in arg-types :for s :in spec-types 
                             :collect (v-casts-to a s))))
            (when (not (some #'null cast-types))
              (list 1 func (loop :for obj :in arg-objs :for type :in cast-types
                              :collect (copy-code obj :type type)))))))))

;; [TODO] should this always copy the arg-objs?
(defun basic-arg-matchp (func arg-types arg-objs)
  (let ((spec-types (v-argument-spec func)))
    (if (loop :for a :in arg-types :for s :in spec-types :always (v-typep a s))
        (list 0 func arg-objs)
        (let ((cast-types (loop :for a :in arg-types :for s :in spec-types 
                             :collect (v-casts-to a s))))
          (when (not (some #'null cast-types))
            (list 1 func (loop :for obj :in arg-objs :for type :in cast-types
                            :collect (copy-code obj :type type))))))))

(defun find-functions-for-args (func-name args-code arg-objs env)
  (let* ((arg-types (mapcar #'code-type arg-objs))
         (any-errors (some #'v-errorp arg-types)))
    (loop :for func :in (get-function func-name env) 
       :for candidate =
       (if (v-special-functionp func) 
           (special-arg-matchp func args-code arg-objs arg-types any-errors)
           (when (not any-errors)
             (if (v-glsl-spec-matchingp func)
                 (glsl-arg-matchp func arg-types arg-objs)
                 (basic-arg-matchp func arg-types arg-objs))))
       :if candidate :collect candidate)))

(defun find-function-for-args (func-name args-code env)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the 
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  (let* ((arg-objs (loop :for i :in args-code :collect (try-compile-arg i env)))
         (functions (find-functions-for-args func-name args-code arg-objs env)))
    (if functions
        (destructuring-bind (score function arg-objs)
            (first (sort functions #'< :key #'first))
          (declare (ignore score))
          (list function arg-objs))
        (make-instance 'deferred-error :error-name 'no-valid-function 
                       :error-args `(:name ,func-name :types
                                           ,(loop :for obj :in arg-objs
                                               :collect (code-type obj)))))))

(defun glsl-resolve-func-type (func args)
  "nil - superior type
   number - type of nth arg
   function - call the function
   (:element n) - element type of nth arg
   list - type spec"
  (let ((spec (v-return-spec func))
        (arg-types (mapcar #'code-type args)))
    (cond ((null spec) (find-mutual-cast-type arg-types))
          ((numberp spec) (nth spec arg-types))
          ((functionp spec) (apply spec args))
          ((and (listp spec) (eq (first spec) :element))
           (v-element-type (nth (second spec) arg-types)))
          ((or (symbolp spec) (listp spec)) (type-spec->type spec))
          (t (error 'invalid-function-return-spec :func func :spec spec)))))

(defun glsl-resolve-special-func-type (func args)
  (handler-case (apply (v-return-spec func) args)
    (error () (error 'problem-with-the-compiler))))

;;------------------------------------------------------------
