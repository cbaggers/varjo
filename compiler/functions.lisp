(in-package :varjo)

(defconstant +order-bias+ 0.0001)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun v-make-f-spec (name transform context arg-types return-spec
                      &key place glsl-spec-matching glsl-name
                        multi-return-vars)
  (list transform arg-types return-spec context place glsl-spec-matching
        glsl-name multi-return-vars name))

(defmacro v-defun (name args &body body)
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    (declare (ignore in-args))
    (when uniforms (error 'uniform-in-sfunc :func-name name))
    (let* ((template (first body)))
      (unless (stringp (first body))
        (error 'invalid-v-defun-template :func-name name :template template))
      (destructuring-bind (transform arg-types return-spec
                                     &key place glsl-spec-matching glsl-name) body
        `(progn (add-function
                 ',name
                 (v-make-f-spec ',name ,transform ',context ',arg-types ',return-spec
                                :place ',place :glsl-name ',glsl-name
                                :glsl-spec-matching ',glsl-spec-matching)
                 *global-env*)
                ',name)))))

;;[TODO] This is pretty ugly. Let's split this up...or at least document it :)
;;{TODO} :return should just be the last form
(defmacro v-defspecial (name args &body body)
  (destructuring-bind (in-args uniforms context rest optional)
      (split-arguments args '(&uniform &context &rest &optional))
    (declare (ignore context)) ;; ignored as provided from body
    (when uniforms (error 'uniform-in-sfunc :func-name name))
    (let ((arg-names (lambda-list-get-names
                      (concatenate 'list in-args
                                   (when rest (cons '&rest rest))
                                   (when rest (cons '&optional optional))))))
      (destructuring-bind (&key context place args-valid return) body
        (cond
          ((eq args-valid t)
           `(progn
              (add-function ',name
                            (v-make-f-spec
                             ',name
                             :special
                             ',context
                             t
                             (lambda ,(cons 'env args)
                               (declare (ignorable env ,@arg-names))
                               ,return)
                             :place ',place)
                            *global-env*)
              ',name))
          (args-valid
           `(progn
              (add-function ',name
                            (v-make-f-spec
                             ',name
                             :special
                             ',context
                             (lambda ,(cons 'env args)
                               (declare (ignorable env ,@arg-names))
                               (let ((res ,args-valid))
                                 (when res (list res 0))))
                             (lambda ,(cons 'env args)
                               (declare (ignorable env ,@arg-names))
                               ,return)
                             :place ',place)
                            *global-env*)
              ',name))
          (t `(progn
                (add-function ',name
                              (v-make-f-spec
                               ',name
                               :special
                               ',context
                               ',(mapcar #'second args)
                               (lambda ,(cons 'env (mapcar #'first args))
                                 (declare (ignorable env ,@arg-names))
                                 ,return)
                               :place ',place)
                              *global-env*)
                ',name)))))))

;;------------------------------------------------------------

(defclass func-match ()
  ((score :initarg :score :reader score)
   (func :initarg :func :reader func)
   (arguments :initarg :arguments :reader arguments)))

;;[TODO] catch cannot-compiler errors only here
(defun try-compile-arg (arg env)
  (handler-case (varjo->glsl arg env)
    (varjo-error (e) (make-instance 'code :type (make-instance 'v-error :payload e)))))


(defun special-arg-matchp (func arg-code arg-objs arg-types any-errors env)
  (let ((method (v-argument-spec func))
        (env (clone-environment env)))
    (if (listp method)
        (when (not any-errors) (basic-arg-matchp func arg-types arg-objs env))
        (if (eq method t)
            (make-instance 'func-match :score t :func func :arguments arg-code)
            (handler-case (make-instance
                           'func-match :score 0 :func func
                           :arguments (apply method (cons env arg-code)))
              (varjo-error () nil))))))

(defun glsl-arg-matchp (func arg-types arg-objs env)
  (let* ((spec-types (v-argument-spec func))
         (spec-generics (positions-if #'v-spec-typep spec-types))
         (g-dim (when spec-generics
                  (when (v-typep (nth (first spec-generics) arg-types) 'v-array
                                 env)
                    (v-dimensions (nth (first spec-generics) arg-types))))))
    (when (and (eql (length arg-objs) (length spec-types))
               (or (null g-dim)
                   (loop :for i :in spec-generics :always
                      (equal (v-dimensions (nth i arg-types)) g-dim))))
      (if (loop :for a :in arg-types :for s :in spec-types :always
             (v-typep a s env))
          (make-instance 'func-match :score 0 :func func
                         :arguments (mapcar #'copy-code arg-objs))
          (let ((cast-types (loop :for a :in arg-types :for s :in spec-types
                               :collect (v-casts-to a s env))))
            (when (not (some #'null cast-types))
              (make-instance 'func-match :score 1 :func func
                             :arguments (loop :for obj :in arg-objs :for type
                                           :in cast-types :collect
                                           (copy-code obj :type type)))))))))

;; [TODO] should this always copy the arg-objs?
(defun basic-arg-matchp (func arg-types arg-objs env)
  (let ((spec-types (v-argument-spec func)))
    (when (eql (length arg-objs) (length spec-types))
      (if (loop :for a :in arg-types :for s :in spec-types :always (v-typep a s env))
          (make-instance 'func-match :score 0 :func func
                         :arguments (mapcar #'copy-code arg-objs))
          (let ((cast-types (loop :for a :in arg-types :for s :in spec-types
                               :collect (v-casts-to a s env))))
            (when (not (some #'null cast-types))
              (make-instance
               'func-match :score 1 :func func
               :arguments (loop :for obj :in arg-objs
                             :for type :in cast-types
                             :collect (copy-code obj :type type)))))))))

(defun match-function-to-args (args-code compiled-args env candidate)
  (let* ((arg-types (mapcar #'code-type compiled-args))
         (any-errors (some #'v-errorp arg-types)))
    (if (v-special-functionp candidate)
        (special-arg-matchp candidate args-code compiled-args arg-types
                            any-errors env)
        (when (not any-errors)
          (if (v-glsl-spec-matchingp candidate)
              (glsl-arg-matchp candidate arg-types compiled-args env)
              (basic-arg-matchp candidate arg-types compiled-args env))))))


(defun find-functions-for-args (func-name args-code env)
  (let* ((candidates (or (get-function-by-name func-name env)
                         (error 'could-not-find-function :name func-name)))
         (compiled-args (when (some #'func-need-arguments-compiledp candidates)
                          (mapcar (rcurry #'try-compile-arg env) args-code)))
         (match-fn (curry #'match-function-to-args args-code compiled-args env))
         (matches (remove nil (mapcar match-fn candidates)))
         (instant-win (find-if #'(lambda (x) (eq t (score x))) matches)))

    (or (when instant-win (list instant-win))
        (mapcar (lambda (x y)
                  (when (numberp (score x))
                    (make-instance
                     'func-match
                     :score (+ (score x) (* y +order-bias+))
                     :func (func x) :arguments (arguments x))))
                matches
                (iota (length matches)))
        (func-find-failure func-name compiled-args))))

;; if there were no candidates then pass errors back
(defun func-find-failure (func-name arg-objs)
  (loop :for arg-obj :in arg-objs
     :if (typep (code-type arg-obj) 'v-error)

     :return `(,(make-instance 'func-match :score t :func (code-type arg-obj)
                               :arguments nil))
     :finally (return
                `(,(make-instance
                    'func-match
                    :score t
                    :func (make-instance 'v-error :payload
                                         (make-instance 'no-valid-function
                                                        :name func-name
                                                        :types (mapcar #'code-type
                                                                       arg-objs)))
                    :arguments nil)))))

(defun find-function-for-args (func-name args-code env)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  (let* ((functions (find-functions-for-args func-name args-code env))
         (function
          (if (and (> (length functions) 1)
                   (some (lambda (x)
                           (some (lambda (x) (stemcellp (code-type x)))
                                 (arguments x)))
                         functions))
              (error 'multi-func-stemcells functions)
              (first (sort functions #'< :key #'score)))))
    (list (func function) (arguments function))))

(defun resolve-func-type (func args env)
  "nil - superior type
   number - type of nth arg
   function - call the function
   (:element n) - element type of nth arg
   list - type spec"
  (let ((spec (v-return-spec func))
        (arg-types (mapcar #'code-type args)))
    (cond ((null spec) (apply #'find-mutual-cast-type arg-types))
          ((typep spec 'v-t-type) spec)
          ((numberp spec) (nth spec arg-types))
          ((functionp spec) (apply spec args))
          ((and (listp spec) (eq (first spec) :element))
           (v-element-type (nth (second spec) arg-types)))
          ((or (symbolp spec) (listp spec)) (type-spec->type spec :env env))
          (t (error 'invalid-function-return-spec :func func :spec spec)))))

;;------------------------------------------------------------
