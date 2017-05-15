(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defmethod v-special-functionp ((func v-function))
  (eq :special (v-glsl-string func)))

(defmethod v-special-functionp ((func v-function-set))
  (some #'v-special-functionp (functions func)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmacro v-defun (name args &body body)
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    (declare (ignore context))
    `(progn
       (varjo:add-external-function ',name ',in-args ',uniforms ',body)
       ',name)))

(defmacro v-def-glsl-template-fun (name args transform arg-types return-spec
                                   &key v-place-index glsl-name)
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    (declare (ignore in-args))
    (when uniforms (error 'uniform-in-sfunc :func-name name))
    (unless (or (stringp transform) (null transform))
      (error 'invalid-v-defun-template :func-name name :template transform))
    (let ((arg-types (if (listp arg-types)
                         (mapcar #'type-spec->type arg-types)
                         arg-types)))
      `(progn (add-form-binding
               (make-function-obj
                ',name ,transform ',context ',arg-types
                ,(make-template-return-spec-generator return-spec)
                :v-place-index ',v-place-index :glsl-name ',glsl-name
                :flow-ids (%gl-flow-id!)
                :in-arg-flow-ids
                (list ,@(n-of '(%gl-flow-id!) (length args))))
               *global-env*)
              ',name))))

(defun element-spec-p (spec)
  (and (listp spec) (eq (first spec) :element)))

(defun make-template-return-spec-generator (x)
  (cond
    ((or (eq x :void) (eq x 'v-void)) (make-type-set))
    ((functionp x) x)
    ((null x) (vector (make-instance 'ret-gen-superior-type)))
    ((numberp x) (vector (make-instance 'ret-gen-nth-arg-type
                                        :arg-num x)))
    ((element-spec-p x) (vector (make-instance 'ret-gen-element-of-nth-arg-type
                                               :arg-num (second x))))
    ((typep x 'v-type) (make-type-set x))
    ((type-specp x) (make-type-set (type-spec->type x)))
    ;; {TODO} proper error
    (t (error "Varjo: Invalid return-type specifier in template-function ~a"
              x))))

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
          (func-name (symb :vs- name))
          (env (symb :env))
          (this (symb :this)))
      (destructuring-bind (&key context v-place-index args-valid return) body
        (cond
          ((eq args-valid t)
           `(progn
              (defun ,func-name ,(append (list env this) args)
                (declare (ignorable ,env ,this ,@arg-names))
                ,return)
              (add-form-binding
               (make-function-obj ',name :special ',context t
                                  #',func-name
                                  :v-place-index ',v-place-index)
               *global-env*)
              ',name))
          (t `(progn
                (defun ,func-name ,(append (list env this)
                                           (mapcar #'first args))
                  (declare (ignorable ,env ,this ,@arg-names))
                  ,return)
                (add-form-binding
                 (make-function-obj ',name :special ',context
                                    ',(mapcar λ(type-spec->type (second _))
                                              args)
                                    #',func-name
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

(defun cast-code (src-obj cast-to-type env)
  (cast-code-inner (primary-type src-obj) src-obj cast-to-type env))

(defmethod cast-code-inner (varjo-type src-obj cast-to-type env)
  (declare (ignore varjo-type env))
  (let* ((src-type (primary-type src-obj))
         (dest-type (set-flow-id cast-to-type (flow-ids src-type))))
    (if (v-type-eq src-type cast-to-type)
        (copy-compiled src-obj :type-set (make-type-set dest-type))
        (copy-compiled src-obj :current-line (cast-string cast-to-type src-obj)
                       :type-set (make-type-set dest-type)))))

(defmethod cast-code-inner
    (varjo-type src-obj (cast-to-type v-function-type) env)
  (declare (ignore varjo-type env))
  (let ((new-type (make-instance
                   'v-function-type
                   :arg-spec (v-argument-spec cast-to-type)
                   :return-spec (v-return-spec cast-to-type)
                   :ctv (ctv (v-type-of src-obj))
                   :flow-ids (flow-ids src-obj))))
    (if (v-type-eq (primary-type src-obj) new-type)
        (copy-compiled src-obj :type-set (make-type-set new-type))
        (copy-compiled
         src-obj
         :current-line (cast-string new-type src-obj)
         :type-set (make-type-set new-type)))))

;; {TODO} proper error
(defmethod cast-code-inner
    ((varjo-type v-any-one-of) src-obj (cast-to-type v-function-type) env)
  (let ((funcs (remove-if-not (lambda (fn) (v-casts-to fn cast-to-type env))
                              (v-types varjo-type))))
    (if funcs
        (copy-compiled src-obj
                       :type-set (make-type-set (gen-any-one-of-type funcs)))
        (error "Varjo: compiler bug: Had already decided it was possible to cast ~a to ~a
however failed to do so when asked."
               src-obj cast-to-type))))

(defmethod cast-code-inner ((varjo-type v-any-one-of)
                            src-obj
                            (cast-to-type v-any-one-of)
                            env)
  ;; in cases where v-casts-to has been used to calculate a new
  ;; v-any-one-of type then the type has the flow-ids of the ctvs
  ;; this means we don't want to use the flow-ids in the src-obj
  (declare (ignore varjo-type env))
  (assert (flow-ids cast-to-type))
  (let* ((dest-type cast-to-type))
    (copy-compiled src-obj :type-set (make-type-set dest-type))))

;; [TODO] should this always copy the arg-objs?
(defun basic-arg-matchp (func arg-types arg-objs env
                         &key (allow-casting t))
  (let ((spec-types (v-argument-spec func)))
    (labels ((calc-secondary-score (types)
               ;; the lambda below sums all numbers or returns nil
               ;; if nil found
               (or (reduce (lambda (a x)
                             (when (and a x)
                               (+ a x)))
                           (mapcar λ(get-type-distance _ _1 nil)
                                   spec-types
                                   types)
                           :initial-value 0)
                   most-positive-fixnum)))
      ;;
      (when (eql (length arg-types) (length spec-types))
        (let* ((perfect-matches (mapcar λ(v-typep _ _1 env)
                                        arg-types
                                        spec-types))
               (score (- (length arg-types)
                         (length (remove nil perfect-matches)))))

          (if (= score 0)
              ;; if all the types match
              (let ((secondary-score (calc-secondary-score arg-types)))
                (make-instance
                 'func-match
                 :func func
                 :arguments (mapcar #'copy-compiled arg-objs)
                 :score score
                 :secondary-score secondary-score))
              (when allow-casting
                (let ((cast-types
                       (loop :for a :in arg-types :for s :in spec-types :collect
                          (v-casts-to a s env))))
                  (when (not (some #'null cast-types))
                    ;; when all the types can be cast
                    (let ((secondary-score (calc-secondary-score cast-types)))
                      (make-instance
                       'func-match
                       :func func
                       :arguments (mapcar (lambda (a c) (cast-code a c env))
                                          arg-objs cast-types)
                       :score score
                       :secondary-score secondary-score)))))))))))

(defun match-function-to-args (args-code compiled-args env candidate)
  (let* ((arg-types (mapcar #'primary-type compiled-args))
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
            (make-compiled
             :type-set (make-type-set (make-instance 'v-error :payload e))
             :current-line ""
             :node-tree (ast-node! :error nil (make-type-set)
                                   env env))
            (error e))))))

(defun try-compile-args (args-code env)
  ;; {TODO} Why don't we care about the env here?
  (mapcar (rcurry #'try-compile-arg env) args-code))

(defmethod func-need-arguments-compiledp ((func v-function))
  (not (and (v-special-functionp func)
            (eq (v-argument-spec func) t))))

(defun find-functions-in-set-for-args (func-set args-code env &optional name code)
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
        (func-find-failure func-name
                           code
                           (mapcar #'primary-type compiled-args)))))

(defun find-function-in-set-for-args (func-set args-code env &optional name code)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  ;; optional code is used for errors
  (labels ((check-for-stemcell-issue (matches func-name)
             (when (and (> (length matches) 1)
                        (some (lambda (x)
                                (some (lambda (x) (stemcellp (primary-type x)))
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
                     args-code env nil code))
           (function (if (= (length matches) 1)
                         (first matches)
                         (progn
                           (check-for-stemcell-issue matches func-name)
                           (first (dual-sort matches))))))
      (list (func function) (arguments function)))))

(defun %func-name-from-set (func-set)
  (let* ((names (mapcar #'name (functions func-set)))
         (names (remove-duplicates names)))
    (if (= 1 (length names))
        (first names)
        names)))

;; if there were no candidates then pass errors back
(defun func-find-failure (func-name form arg-types)
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
                                                        :types arg-types
                                                        :form form))
                    :arguments nil)))))

(defun resolve-func-set (func compiled-args)
  (let* ((arg-types (map 'list #'primary-type compiled-args))
         (new-flow-ids (funcall
                        (if (multi-return-function-p func)
                            #'calc-mfunction-return-ids-given-args
                            #'calc-regular-function-return-ids-given-args)
                        func compiled-args)))
    (labels ((force-flow-id (type flow-id)
               ;; This is one of the few cases where we want to set a flow id
               ;; regardless of the current state
               (if (flow-ids type)
                   (replace-flow-id type flow-id)
                   (set-flow-id type flow-id)))

             (resolve-return-type (spec new-flow-id)
               (typecase spec
                 (v-type
                  (force-flow-id spec new-flow-id))

                 (ret-gen-superior-type
                  (or (force-flow-id (apply #'find-mutual-cast-type arg-types)
                                     new-flow-id)
                      (error 'unable-to-resolve-func-type
                             :func-name (name func) :args compiled-args)))

                 (ret-gen-nth-arg-type
                  (force-flow-id (nth (arg-num spec) arg-types)
                                 new-flow-id))

                 (ret-gen-element-of-nth-arg-type
                  (force-flow-id (v-element-type (nth (arg-num spec)
                                                      arg-types))
                                 new-flow-id))

                 (t (error 'invalid-function-return-spec
                           :func func
                           :spec spec)))))

      (make-type-set* (map 'list #'resolve-return-type
                           (v-return-spec func)
                           new-flow-ids)))))

;;------------------------------------------------------------

(defun function-arg-specs-match-p (func-a func-b env)
  (exact-match-function-to-types
   (v-argument-spec func-a) env func-b))

(defun basic-exact-type-matchp (func arg-types env)
  (let ((match (basic-arg-matchp func arg-types nil env :allow-casting nil)))
    (and match (= 0 (score match) (secondary-score match)))))

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

;;------------------------------------------------------------

;; {TODO} proper error
(defmethod find-form-binding-by-literal ((name symbol) env)
  (assert (not (eq name 'declare)) ()
          'treating-declare-as-func :decl '(function declare))
  (get-form-binding name env))

(defmethod find-form-binding-by-literal ((func-name list) env)
  ;;
  (destructuring-bind (name &rest arg-types) func-name
    (assert (not (eq name 'declare)) ()
            'treating-declare-as-func :decl func-name)
    (let ((arg-types (mapcar (lambda (x) (type-spec->type x))
                             arg-types))
          (binding (get-form-binding name env)))
      ;;
      (etypecase binding
        ;; When we have types we should try to match exactly
        (v-function-set
         (if arg-types
             (or (find-if λ(exact-match-function-to-types arg-types env _)
                          (functions binding))
                 (error 'could-not-find-function :name func-name))
             binding))
        ;; otherwise return the binding
        (v-regular-macro binding)
        ;;
        (null (error 'could-not-find-function :name func-name))))))
