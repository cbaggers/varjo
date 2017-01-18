(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defmethod v-special-functionp ((func v-function))
  (eq :special (v-glsl-string func)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmacro v-defun (name args &body body)
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    (declare (ignore in-args))
    (when uniforms (error 'uniform-in-sfunc :func-name name))
    (let* ((template (first body)))
      (unless (stringp (first body))
        (error 'invalid-v-defun-template :func-name name :template template))
      (destructuring-bind (transform arg-types return-spec
                                     &key v-place-index glsl-name) body
        `(progn (add-function-from-spec
                 (make-function-obj
                  ',name ,transform ',context ',arg-types '(,return-spec)
                  :v-place-index ',v-place-index :glsl-name ',glsl-name
                  :flow-ids (%gl-flow-id!)
                  :in-arg-flow-ids
                  ,(cons 'list (n-of '(%gl-flow-id!) (length args))))
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
                                   (when rest (cons '&optional optional)))))
          (func-name (symb :vs- name)))
      (destructuring-bind (&key context v-place-index args-valid return) body
        (cond
          ((eq args-valid t)
           `(progn
              (defun ,func-name ,(cons 'env args)
                (declare (ignorable env ,@arg-names))
                ,return)
              (add-function-from-spec
               (make-function-obj ',name :special ',context t (list #',func-name)
                                  :v-place-index ',v-place-index)
               *global-env*)
              ',name))
          (t `(progn
                (defun ,func-name ,(cons 'env (mapcar #'first args))
                  (declare (ignorable env ,@arg-names))
                  ,return)
                (add-function-from-spec
                 (make-function-obj ',name :special ',context
                                    ',(mapcar #'second args) (list #',func-name)
                                    :v-place-index ',v-place-index)
                 *global-env*)
                ',name)))))))

;;------------------------------------------------------------

(defun special-arg-matchp (func arg-code arg-objs arg-types any-errors env)
  (let ((arg-spec (v-argument-spec func))
        (env (fresh-environment env)))
    (cond
      ((listp arg-spec)
       (when (not any-errors) (basic-arg-matchp func arg-types arg-objs env)))
      ((eq arg-spec t)
       (make-instance 'func-match :func func :arguments arg-code
                      :score t :secondary-score 0))
      (t (error 'invalid-special-function-arg-spec
                :name (name func)
                :spec arg-spec)))))

(defmethod cast-code (src-obj cast-to-type)
  (let* ((src-type (code-type src-obj))
         (dest-type (set-flow-id cast-to-type (flow-ids src-type))))
    (if (v-type-eq src-type cast-to-type)
        (copy-code src-obj :type dest-type)
        (copy-code src-obj :current-line (cast-string cast-to-type src-obj)
                   :type dest-type))))

(defmethod cast-code (obj (cast-to-type v-function-type))
  (let ((new-type (make-instance
                   'v-function-type
                   :arg-spec (v-argument-spec cast-to-type)
                   :return-spec (v-return-spec cast-to-type)
                   :ctv (ctv (v-type-of obj)))))
    (if (v-type-eq (code-type obj) new-type)
        (copy-code obj :type new-type)
        (copy-code
         obj
         :current-line (cast-string new-type obj)
         :type new-type))))

;; [TODO] should this always copy the arg-objs?
(defun basic-arg-matchp (func arg-types arg-objs env)
  (let ((spec-types (v-argument-spec func)))
    (labels ((calc-secondary-score (types)
               ;; the lambda below sums all numbers or returns nil
               ;; if nil found
               (reduce (lambda (a x)
                         (when (and a x)
                           (+ a x)))
                       (mapcar λ(get-type-distance _ _1 nil)
                               spec-types
                               types)
                       :initial-value 0)))
      ;;
      (when (eql (length arg-objs) (length spec-types))
        (let* ((perfect-matches (mapcar λ(v-typep _ _1 env) arg-types spec-types))
               (score (- (length arg-objs)
                         (length (remove nil perfect-matches)))))
          (if (= score 0)
              (let ((secondary-score (calc-secondary-score arg-types)))
                (when secondary-score
                  (make-instance
                   'func-match
                   :func func
                   :arguments (mapcar #'copy-code arg-objs)
                   :score score
                   :secondary-score secondary-score)))
              (let ((cast-types (loop :for a :in arg-types :for s :in spec-types
                                   :collect (v-casts-to a s env))))
                (when (not (some #'null cast-types))
                  (let ((secondary-score (calc-secondary-score cast-types)))
                    (when secondary-score
                      (make-instance
                       'func-match
                       :func func
                       :arguments (mapcar #'cast-code arg-objs cast-types)
                       :score score
                       :secondary-score secondary-score)))))))))))

(defun match-function-to-args (args-code compiled-args env candidate)
  (let* ((arg-types (mapcar #'code-type compiled-args))
         (any-errors (some #'v-errorp arg-types)))
    (if (v-special-functionp candidate)
        (special-arg-matchp candidate args-code compiled-args arg-types
                            any-errors env)
        (when (not any-errors)
          (basic-arg-matchp candidate arg-types compiled-args env)))))


(defun try-compile-arg (arg env &optional (wrap-errors-p t))
  ;; This let is important
  ;; By setting :multi-val-base to nil you stop 'values forms
  ;; deeper in the code seeing that the can return. This is how
  ;; the CL values logic works (values cant pass through function calls)
  ;; however the 'values-safe special form allow you to get around that
  ;; for one function call by setting multi-val-safe to true.
  (let* ((mval-base (when (v-multi-val-safe env)
                      (v-multi-val-base env)))
         ;; we dont have to set :multi-val-safe explicitly here
         ;; as it will be nil regardless, but I like it as documentation
         (env (fresh-environment env :multi-val-base mval-base
                                 :multi-val-safe nil)))
    (handler-case (compile-form arg env)
      (varjo-error (e)
        (if wrap-errors-p
            (make-code-obj
             (make-instance 'v-error :payload e) ""
             :node-tree (ast-node! :error nil (gen-none-type)
                                   env env))
            (error e))))))

(defun try-compile-args (args-code env)
  ;; {TODO} Why don't we care about the env here?
  (mapcar (rcurry #'try-compile-arg env) args-code))

(defun find-functions-in-set-for-args (func-set args-code env &optional name)
  (let* ((func-name (or name (%func-name-from-set func-set)))
         (candidates (functions func-set))
         (compiled-args (when (some #'func-need-arguments-compiledp candidates)
                          (try-compile-args args-code env)))
         (match-fn (curry #'match-function-to-args args-code compiled-args env))
         (matches (remove nil (mapcar match-fn candidates)))
         (instant-win (find-if #'(lambda (x) (eq t (score x))) matches)))
    ;;
    (or (when instant-win (list instant-win))
        (progn
          (assert (every λ(numberp (score _)) matches))
          matches)
        (func-find-failure func-name (mapcar #'code-type compiled-args)))))

(defun find-function-in-set-for-args (func-set args-code env &optional name)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  (labels ((check-for-stemcell-issue (matches func-name)
             (when (and (> (length matches) 1)
                        (some (lambda (x)
                                (some (lambda (x) (stemcellp (code-type x)))
                                      (arguments x)))
                              matches))
               (error 'multi-func-stemcells :func-name func-name)))
           (dual-sort (matches)
             (let* ((primary-sorted (sort matches #'< :key #'score))
                    (best-primary (score (first primary-sorted)))
                    (primary-set (remove best-primary primary-sorted
                                         :key #'score
                                         :test-not #'=)))
               (sort primary-set #'< :key #'secondary-score))))
    (let* ((func-name (or name (%func-name-from-set func-set)))
           (matches (find-functions-in-set-for-args
                     func-set
                     args-code env))
           (function (if (= (length matches) 1)
                         (first matches)
                         (progn
                           (check-for-stemcell-issue matches func-name)
                           (first (dual-sort matches))))))
      (list (func function) (arguments function)))))

(defun find-function-for-args (func-name args-code env)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  (let* ((candidates (let ((c (get-func-set-by-name func-name env)))
                       (if c
                           (functions c)
                           (error 'could-not-find-function :name func-name)))))
    (find-function-in-set-for-args
     (make-instance 'v-function-set :functions candidates)
     args-code env)))

(defun %func-name-from-set (func-set)
  (let* ((names (mapcar #'name (functions func-set)))
         (names (remove-duplicates names)))
    (if (= 1 (length names))
        (first names)
        names)))

;; if there were no candidates then pass errors back
(defun func-find-failure (func-name arg-types)
  (assert func-name)
  (loop :for arg-type :in arg-types
     :if (typep arg-type 'v-error)

     :return `(,(make-instance 'func-match :score t :func arg-type
                               :arguments nil))
     :finally (return
                `(,(make-instance
                    'func-match
                    :score t
                    :func (make-instance 'v-error :payload
                                         (make-instance 'no-valid-function
                                                        :name func-name
                                                        :types arg-types))
                    :arguments nil)))))

(defun resolve-func-type (func args env)
  "nil - superior type
   number - type of nth arg
   function - call the function
   (:element n) - element type of nth arg
   list - type spec"
  (declare (ignore env))
  (let* ((spec (first (v-return-spec func)))
         (arg-types (mapcar #'code-type args))
         (result
          (cond ((null spec) (apply #'find-mutual-cast-type arg-types))
                ((typep spec 'v-type) spec)
                ((numberp spec) (nth spec arg-types))
                ((functionp spec) (apply spec args))
                ((and (listp spec) (eq (first spec) :element))
                 (v-element-type (nth (second spec) arg-types)))
                ((or (symbolp spec) (listp spec)) (type-spec->type spec))
                (t (error 'invalid-function-return-spec :func func :spec spec)))))
    (assert (typep result 'v-type))
    result))

;;------------------------------------------------------------


(defun basic-exact-type-matchp (func arg-types env)
  (let ((spec-types (v-argument-spec func)))
    (when (eql (length arg-types) (length spec-types))
      (loop :for a :in arg-types :for s :in spec-types
         :always (v-type-eq a s env)))))

(defun special-exact-type-matchp (func arg-types env)
  (let ((arg-spec (v-argument-spec func))
        (env (fresh-environment env)))
    (cond
      ((listp arg-spec) (basic-exact-type-matchp func arg-types env))
      ((eq arg-spec t) t)
      (t (error 'invalid-special-function-arg-spec
                :name (name func)
                :spec arg-spec)))))

(defun exact-match-function-to-types (arg-types env candidate)
  (if (v-special-functionp candidate)
      (special-exact-type-matchp candidate arg-types env)
      (basic-exact-type-matchp candidate arg-types env)))

(defun find-function-for-types (func-name arg-types env)
  (find-if (curry #'exact-match-function-to-types arg-types env)
           (or (functions (get-func-set-by-name func-name env))
               (error 'could-not-find-function :name func-name))))

;;------------------------------------------------------------

(defun find-function-by-literal (func-name env)
  (destructuring-bind (name &rest arg-types) func-name
    (let ((arg-types (mapcar (lambda (x) (type-spec->type x))
                             arg-types)))
      (or (if arg-types
              (find-function-for-types name arg-types env)
              (get-func-set-by-name name env))
          (error "No function yada {TODO} ~a" name)))))
