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
           (destructuring-bind (&key context place args-valid return) body
             `(progn (add-function ',name (list :special
                                                (lambda ,args 
                                                  (let ((res ,args-valid)) 
                                                    (when res (list res 0))))
                                                (lambda ,args ,return)
                                                ,context ,place)
                                   *global-env*)
                     ',name)))
          (t `(progn (setf (gethash ',name (v-external-functions *global-env*))
                         '(,args ,@(rest body)))
                   ',name)))))
;; [TODO] ^^^^- we use setf rather than add for extended functions...this seems 
;;              dumb as it means we can have generic externals which are chosen
;;              based on usage. This also means some level of checking to see 
;;              if the arg spec matches.

;;------------------------------------------------------------

;; (and (eql arg-len (length (v-argument-spec func)))
;;             (and (loop :for arg :in args 
;;                     :for arg-type :in (v-argument-spec func)
;;                     :always (v-casts-to-p h(code-type arg) arg-type))
;;                  (multiple-type-equivilent )))

;;[TODO] Need to handle fake-structs
;;[TODO] if type specifiers are same in in-args then the args passed in must 

;;       be compatible (maybe only if :spec-match is true
;;[TODO] for special funcs this can involve walking the rest of a tree, we dont 
;;       want to do this twice
;;[TODO] We need to prioritise matches that dont require casting
;;[TODO] We need to collect the new type (in case of casting) and the number of
;;       casts[?] or some priority of results

(defun try-compile-arg (arg env)
  (handler-case (varjo->glsl arg env)
    (error () (make-instance 'code :type (make-instance 'v-error)))))

(defun special-arg-match (arg-code arg-objs arg-types any-errors)
  nil)

(defun glsl-arg-match (func arg-types)
  nil)

(defmacro if-it (test then else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun basic-arg-match (func arg-types arg-objs)
  (let ((spec-types (v-argument-spec func)))
    (if (loop :for a :in arg-types :for s in spec-types :always (v-typep a s))
        (list 0 func arg-objs))))

(defun find-functions-for-args (func-name args-code env)
  (let* ((arg-objs (loop :for i :in args-code :collect (try-compile-arg i env)))
         (arg-types (mapcar #'code-type arg-objs))
         (any-errors (some #'v-errorp arg-types)))
    (loop :for func :in (get-function func-name env) 
       :for candidate =
       (if (v-special-functionp func) 
           (special-arg-match args-code arg-objs arg-types any-errors)
           (when (not any-errors)
             (if (v-glsl-spec-matchingp func)
                 (glsl-arg-match func arg-types)
                 (basic-arg-match func arg-types))))
       :if candidate :collect candidate)))

(defun find-function-for-args (func-name args env)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the 
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  (let ((functions (find-functions-for-args func-name args env)))
    (destructuring-bind (score function arg-objs score)
        (first (sort functions #'< :key #'second))
      (declare (ignore score))
      (list function arg-objs))))

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

;;------------------------------------------------------------
