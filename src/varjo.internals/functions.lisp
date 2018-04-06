(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defmethod v-special-functionp ((func v-function))
  (eq :special (v-glsl-string func)))

(defun user-function-p (f)
  (typep f 'v-user-function))

(defun multi-return-function-p (func)
  (let ((rset (v-return-spec func)))
    (and (vectorp rset) (> (length rset) 1))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmacro define-vari-function (name args &body body)
  (destructuring-bind (in-args uniforms context shared)
      (split-arguments args '(&uniform &context &shared))
    (declare (ignore context))
    (assert (not shared) () 'shared-in-function :name name)
    `(progn
       (add-external-function ',name ',in-args ',uniforms ',body)
       ',name)))

(defmacro v-defun (name args &body body)
  `(define-vari-function ,name ,args ,@body))

(defun template-args-valid (args rest types)
  (let ((args (append args (when rest (cons '&rest rest)))))
    (and (= (length args) (length types))
         (eql (position-if #'&rest-p args)
              (position-if #'&rest-p types)))))

(defmacro v-def-glsl-template-fun (name args transform arg-types return-spec
                                   &key v-place-index glsl-name pure)
  (destructuring-bind (in-args rest uniforms context shared)
      (split-arguments args '(&rest &uniform &context &shared))
    (assert (not shared) () 'shared-in-function :name name)
    (assert (template-args-valid in-args rest arg-types) (args arg-types)
            'v-def-template-arg-mismatch :args args :types arg-types)
    (when uniforms (error 'uniform-in-sfunc :func-name name))
    (unless (or (stringp transform) (null transform))
      (error 'invalid-v-defun-template :func-name name :template transform))
    (let ((arg-types (if (listp arg-types)
                         (mapcar #'arg-form->type arg-types)
                         arg-types)))
      `(progn (add-global-form-binding
               (make-function-obj
                ',name ,transform ',context ',arg-types
                ,(make-template-return-spec-generator return-spec)
                :v-place-index ',v-place-index :glsl-name ',glsl-name
                :flow-ids (%gl-flow-id!)
                :in-arg-flow-ids
                (list ,@(n-of '(%gl-flow-id!) (length args)))
                :pure ,pure))
              ',name))))

(defmacro define-glsl-template-function (name args return-spec transform &key pure)
  (let ((arg-names (mapcar #'first args))
        (arg-types (mapcar #'second args)))
    `(v-def-glsl-template-fun ,name ,arg-names ,transform ,arg-types
                              ,return-spec
                              :pure ,pure)))

#+nil
(define-glsl-template-fun foo ((x :int)) :int
  "foo(~a)"
  :pure t)

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

(defmacro v-defspecial (name args &body body)
  `(define-vari-special-operator ,name ,args ,@body))

(defun extract-lambda-list-names (llist)
  (multiple-value-bind (required optional rest keys other aux)
      (alexandria:parse-ordinary-lambda-list llist)
    (declare (ignore other))
    (remove nil
            (append required
                    (mapcar #'first optional)
                    (mapcar #'third optional)
                    (list rest)
                    (mapcar #'first keys)
                    (mapcar #'third keys)
                    (mapcar #'first aux)))))

;;[TODO] This is pretty ugly. Let's split this up...or at least document it :)
;;{TODO} :return should just be the last form
(defmacro define-vari-special-operator (name args &body body)
  (destructuring-bind (in-args uniforms context rest optional shared)
      (split-arguments args '(&uniform &context &rest &optional &shared))
    (declare (ignore context)) ;; ignored as provided from body
    (assert (not shared) () 'shared-in-function :name name)
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
              (defun ,func-name (,env ,this &rest sargs)
                (declare (ignorable ,env ,this))
                (handler-case
                    (destructuring-bind ,args sargs
                      (declare (ignore ,@(extract-lambda-list-names args))))
                  (error () (error 'invalid-arguments-for-special-op
                                   :name ',name
                                   :args sargs)))
                (destructuring-bind ,args sargs
                  (declare (ignorable ,@arg-names))
                  ,return))
              (add-global-form-binding
               (make-function-obj ',name :special ',context t
                                  #',func-name
                                  :v-place-index ',v-place-index))
              ',name))
          (t `(progn
                (defun ,func-name ,(append (list env this)
                                           (mapcar #'first args))
                  (declare (ignorable ,env ,this ,@arg-names))
                  ,return)
                (add-global-form-binding
                 (make-function-obj ',name :special ',context
                                    ',(mapcar λ(type-spec->type (second _))
                                              args)
                                    #',func-name
                                    :v-place-index ',v-place-index))
                ',name)))))))

;;------------------------------------------------------------

(defun special-arg-matchp (func arg-code arg-objs arg-types any-errors)
  (let ((arg-spec (v-argument-spec func)))
    (cond
      ((listp arg-spec)
       (when (not any-errors) (basic-arg-matchp func arg-types arg-objs)))
      ((eq arg-spec t)
       (make-instance 'func-match :func func :arguments arg-code
                      :score t :secondary-score 0 :tertiary-score 0))
      (t (error 'invalid-special-function-arg-spec
                :name (name func)
                :spec arg-spec)))))

;; {TODO} proper error
(defmethod cast-code-inner ((varjo-type v-any-one-of)
                            src-obj
                            (cast-to-type v-function-type))
  (let ((funcs (remove-if-not (lambda (fn) (v-casts-to fn cast-to-type))
                              (v-types varjo-type))))
    (if funcs
        (copy-compiled src-obj
                       :type-set (make-type-set (gen-any-one-of-type funcs)))
        (error "Varjo: compiler bug: Had already decided it was possible to cast ~a to ~a
however failed to do so when asked."
               src-obj cast-to-type))))

(defmethod cast-code-inner ((varjo-type v-any-one-of)
                            src-obj
                            (cast-to-type v-any-one-of))
  ;; in cases where v-casts-to has been used to calculate a new
  ;; v-any-one-of type then the type has the flow-ids of the ctvs
  ;; this means we don't want to use the flow-ids in the src-obj
  (declare (ignore varjo-type))
  (assert (flow-ids cast-to-type))
  (let* ((dest-type cast-to-type))
    (copy-compiled src-obj :type-set (make-type-set dest-type))))

(defun expand-argument-spec (func arg-types)
  (let* ((spec (v-argument-spec func))
         (rest-pos (&rest-pos func)))
    (if (and rest-pos (>= (length arg-types) rest-pos))
        (let* ((rest (subseq spec (1+ rest-pos)))
               (type (first rest))
               (len (length (subseq arg-types rest-pos))))
          ;;
          (append (subseq spec 0 rest-pos)
                  (loop :for i :below len :collect
                     (copy-type type))))
        spec)))

(defun func-args-satisfy-p (func arg-type-requirements)
  (let ((func-arg-types (if (&rest-pos func)
                        (expand-argument-spec func arg-type-requirements)
                        (v-argument-spec func)))
        (req-type-no-block-structs
         (loop :for type :in arg-type-requirements :collect
            (if (typep type 'v-block-struct)
                (v-element-type type)
                type))))
    (when (eql (length arg-type-requirements) (length func-arg-types))
      (let* ((perfect-matches 0))
        (loop :for req :in req-type-no-block-structs
           :for arg :in func-arg-types :do
           (when (if (typep req 'v-trait)
                     (get-trait-implementation req arg :errorp nil)
                     (v-typep arg req))
             (incf perfect-matches)))
        (let ((score (- (length arg-type-requirements) perfect-matches)))
          (= score 0))))))

;; [TODO] should this always copy the arg-objs?
(defun basic-arg-matchp (func arg-types arg-objs
                         &key (allow-casting t))
  (let ((func-arg-types (if (&rest-pos func)
                        (expand-argument-spec func arg-types)
                        (v-argument-spec func)))
        (arg-types-no-block-structs
         (loop :for type :in arg-types :collect
            (if (typep type 'v-block-struct)
                (v-element-type type)
                type))))
    (labels ((calc-secondary-score (types)
               ;; the lambda below sums all numbers or returns nil
               ;; if nil found
               (or (reduce (lambda (a x)
                             (when (and a x)
                               (+ a x)))
                           (mapcar λ(get-type-distance _ _1 nil)
                                   func-arg-types
                                   types)
                           :initial-value 0)
                   most-positive-fixnum)))
      ;;
      (when (eql (length arg-types) (length func-arg-types))
        (let* ((perfect-matches
                (loop :for val :in arg-types-no-block-structs
                   :for arg :in func-arg-types
                   :count (if (typep arg 'v-trait)
                              (if (typep val 'v-trait)
                                  (v-type-eq arg val)
                                  (get-trait-implementation arg val :errorp nil))
                              (v-typep val arg))))
               (score (- (length arg-types) perfect-matches)))

          (if (= score 0)
              ;; if all the types match
              (let ((secondary-score (calc-secondary-score
                                      arg-types-no-block-structs)))
                (make-instance
                 'func-match
                 :func func
                 :arguments (mapcar #'copy-compiled arg-objs)
                 :score score
                 :secondary-score secondary-score
                 :tertiary-score 0))
              (when allow-casting
                (let ((cast-types
                       (loop :for a :in arg-types :for s :in func-arg-types :collect
                          (v-casts-to a s))))
                  (when (not (some #'null cast-types))
                    ;; when all the types can be cast
                    (let ((secondary-score (calc-secondary-score cast-types))
                          (tertiary-score (reduce #'+ (mapcar #'tertiary-score
                                                              func-arg-types))))
                      (make-instance
                       'func-match
                       :func func
                       :arguments (mapcar (lambda (a c) (cast-code a c))
                                          arg-objs cast-types)
                       :score score
                       :secondary-score secondary-score
                       :tertiary-score tertiary-score)))))))))))

(defun match-function-to-args (args-code compiled-args candidate)
  ;; could be here that we unbox the block-structs
  ;; hmm I dont think weve ever tried passing ephemerals to non user
  ;; defined funcs before. ah it's ok, this code is used for user funcs too
  (let* ((arg-types (mapcar #'primary-type compiled-args))
         (any-errors (some #'v-errorp arg-types)))
    (if (v-special-functionp candidate)
        (special-arg-matchp candidate args-code compiled-args arg-types
                            any-errors)
        (when (not any-errors)
          (basic-arg-matchp candidate arg-types compiled-args)))))


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
             :current-line "")
            (error e))))))

(defun try-compile-args (args-code env)
  ;; {TODO} Why don't we care about the env here?
  (mapcar (rcurry #'try-compile-arg env) args-code))

(defmethod func-need-arguments-compiledp ((func v-function))
  (not (and (v-special-functionp func)
            (eq (v-argument-spec func) t))))

(defun find-functions-in-set-for-args (func-set args-code env &optional name code)
  (let* ((candidates (functions func-set))
         (compiled-args (when (some #'func-need-arguments-compiledp candidates)
                          (try-compile-args args-code env)))
         (match-fn (curry #'match-function-to-args args-code compiled-args))
         (matches (remove nil (mapcar match-fn candidates)))
         (instant-win (find-if #'(lambda (x) (eq t (score x))) matches)))
    ;;
    (or (when instant-win (list instant-win))
        (progn
          (assert (every λ(numberp (score _)) matches))
          matches)
        (func-find-failure code (mapcar #'primary-type compiled-args)
                           func-set name))))

(defun find-function-in-set-for-args (func-set args-code env &optional name code)
  "Find the function that best matches the name and arg spec given
   the current environment. This process simply involves finding the
   functions and then sorting them by their appropriateness score,
   the lower the better. We then take the first one and return that
   as the function to use."
  ;; optional code is used for errors
  (labels ((check-for-stemcell-issue (matches)
             (when (and (> (length matches) 1)
                        (some (lambda (x)
                                (some (lambda (x) (stemcellp (primary-type x)))
                                      (arguments x)))
                              matches))
               (error 'multi-func-stemcells
                      :func-name (or name (%func-name-from-set func-set)))))
           (dual-sort (matches)
             (let* ((primary-sorted (sort matches #'< :key #'score))
                    (best-primary (score (first primary-sorted)))
                    (primary-set (remove best-primary primary-sorted
                                         :key #'score
                                         :test-not #'=)))
               (sort primary-set #'< :key #'secondary-score)))
           (pick-using-scores (matches)
             (check-for-stemcell-issue matches)
             (let* ((2nd-matches (dual-sort matches))
                    (best-2d-score (secondary-score (first 2nd-matches)))
                    (2nd-candidates (remove-if-not
                                     (lambda (x)
                                       (= (secondary-score x) best-2d-score))
                                     2nd-matches))
                    (3rd (sort 2nd-matches #'> :key #'tertiary-score)))
               (cond
                 ((= (length 2nd-candidates) 1)
                  (first 2nd-candidates))
                 ((= (tertiary-score (first 3rd))
                     (tertiary-score (first 2nd-candidates)))
                  (first 2nd-candidates))
                 (t (first 3rd))))))
    (let* ((matches (find-functions-in-set-for-args
                     func-set
                     args-code env nil code))
           (function (if (= (length matches) 1)
                         (first matches)
                         (pick-using-scores matches ))))
      (list (func function) (arguments function)))))

(defun %func-name-from-set (func-set)
  (let* ((names (mapcar #'name (functions func-set)))
         (names (remove-duplicates names)))
    (if (= 1 (length names))
        (first names)
        names)))

;; if there were no candidates then pass errors back
(defun func-find-failure (form arg-types func-set func-name)
  (loop :for arg-type :in arg-types
     :if (typep arg-type 'v-error)

     :return `(,(make-instance 'func-match :score t :func arg-type
                               :arguments nil))
     :finally (return
                (let ((func-name
                       (or func-name (%func-name-from-set func-set))))
                  `(,(make-instance
                      'func-match
                      :score t
                      :func (make-instance 'v-error :payload
                                           (make-instance 'no-valid-function
                                                          :name func-name
                                                          :types arg-types
                                                          :form form))
                      :arguments nil))))))

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

(defun function-signatures-equal (func-a func-b)
  (and (eq (name func-a) (name func-b))
       (let ((spec-a (v-special-functionp func-a))
             (spec-b (v-special-functionp func-b)))
         (if (or spec-a spec-b)
             (and (and spec-a spec-b)
                  (special-exact-type-matchp
                   func-a (v-argument-spec func-b) ))
             (function-arg-specs-match-p func-a func-b)))))

(defun function-arg-specs-match-p (func-a func-b)
  (let* ((a-spec (v-argument-spec func-a))
         (b-spec (v-argument-spec func-b))
         (&rest-pos-a (&rest-pos a-spec))
         (&rest-pos-b (&rest-pos b-spec)))
    (when (eql &rest-pos-a &rest-pos-b)
      (let ((a-spec (if &rest-pos-b
                         (expand-argument-spec
                          func-a
                          (remove-if #'&rest-p b-spec))
                         a-spec)))
        (exact-match-function-to-types a-spec func-b)))))

(defun basic-exact-type-matchp (func arg-types)
  (let ((match (basic-arg-matchp func arg-types nil :allow-casting nil)))
    (and match (= 0 (score match) (secondary-score match)))))

(defun special-exact-type-matchp (func arg-types)
  (let ((arg-spec (v-argument-spec func)))
    (cond
      ((listp arg-spec) (basic-exact-type-matchp func arg-types))
      ((eq arg-spec t) t)
      (t (error 'invalid-special-function-arg-spec
                :name (name func)
                :spec arg-spec)))))

(defun exact-match-function-to-types (arg-types candidate)
  (if (v-special-functionp candidate)
      (special-exact-type-matchp candidate arg-types)
      (basic-exact-type-matchp candidate arg-types)))

;;------------------------------------------------------------

(defmethod find-global-form-binding-by-literal
    ((name symbol) &optional include-external-functions)
  (assert (not (eq name 'declare)) ()
          'treating-declare-as-func :decl '(function declare))
  (get-global-form-binding name include-external-functions))

(defmethod find-form-binding-by-literal ((name symbol) env)
  (assert (not (eq name 'declare)) ()
          'treating-declare-as-func :decl '(function declare))
  (get-form-binding name env))

(defmethod find-global-form-binding-by-literal
    ((func-name list) &optional include-external-functions)
  ;;
  (destructuring-bind (name &rest arg-types) func-name
    (assert (not (eq name 'declare)) ()
            'treating-declare-as-func :decl func-name)
    (assert (not (find-if #'&rest-p arg-types))
            () 'cannot-take-reference-to-&rest-func :func-name func-name)
    (let ((arg-types (mapcar (lambda (x) (type-spec->type x))
                             arg-types))
          (binding (get-global-form-binding name include-external-functions)))
      ;;
      (etypecase binding
        ;; When we have types we should try to match exactly
        (v-function-set
         (if arg-types
             (or (%post-process-found-literal-func
                  (find-if λ(exact-match-function-to-types arg-types _)
                           (functions binding))
                  arg-types)
                 (error 'could-not-find-function :name func-name))
             binding))
        ;; otherwise return the binding
        (v-regular-macro binding)
        ;;
        (null (error 'could-not-find-function :name func-name))))))

(defmethod find-form-binding-by-literal ((func-name list) env)
  ;;
  (destructuring-bind (name &rest arg-types) func-name
    (assert (not (eq name 'declare)) ()
            'treating-declare-as-func :decl func-name)
    (assert (not (find-if #'&rest-p arg-types))
            () 'cannot-take-reference-to-&rest-func :func-name func-name)
    (let ((arg-types (mapcar (lambda (x) (type-spec->type x))
                             arg-types))
          (binding (get-form-binding name env)))
      ;;
      (etypecase binding
        ;; When we have types we should try to match exactly
        (v-function-set
         (if arg-types
             (or (%post-process-found-literal-func
                  (find-if λ(exact-match-function-to-types arg-types _)
                           (functions binding))
                  arg-types)
                 (error 'could-not-find-function :name func-name))
             binding))
        ;; otherwise return the binding
        (v-regular-macro binding)
        ;;
        (null (error 'could-not-find-function :name func-name))))))

(defun %post-process-found-literal-func (match arg-types)
  (if (and match (find-if #'&rest-p (v-argument-spec match)))
      (synthesize-exact-func-from-&rest-func match arg-types)
      match))

(defun synthesize-exact-func-from-&rest-func (match arg-types)
  (labels ((rest-pos (func)
             (position-if #'&rest-p (v-argument-spec func))))
    (let* ((lines (loop :for nil :in arg-types :collect "~a"))
           (rest-pos (rest-pos match))
           (arg-glsl (append (subseq lines 0 rest-pos)
                             (list (subseq lines rest-pos))))
           (transform (apply #'format nil (v-glsl-string match) arg-glsl)))
      (make-function-obj (name match)
                         transform
                         (v-versions match)
                         arg-types
                         (v-return-spec match)
                         :v-place-index (v-place-index match)
                         :glsl-name (glsl-name match)
                         :implicit-args (implicit-args match)
                         :in-out-args (in-out-args match)
                         :flow-ids (flow-ids match)
                         :in-arg-flow-ids (in-arg-flow-ids match)
                         :pure (pure-p match)))))

;;----------------------------------------------------------------------

(defun %calc-flow-id-given-args (in-arg-flow-ids return-flow-id arg-code-objs
                                 &optional (multi-return-position 0))
  (let ((p (positions-if (lambda (x) (id~= return-flow-id x))
                         in-arg-flow-ids)))
    (if p
        (reduce #'flow-id!
                (mapcar (lambda (i) (flow-ids (elt arg-code-objs i)))
                        p))
        (flow-id+meta! :return-pos multi-return-position))))

(defun calc-regular-function-return-ids-given-args (func arg-code-objs)
  (unless (function-return-spec-doesnt-need-flow-ids (v-return-spec func))
    (list (%calc-flow-id-given-args (in-arg-flow-ids func)
                                    (flow-ids func)
                                    arg-code-objs))))

(defun calc-mfunction-return-ids-given-args (func arg-code-objs)
  (let* ((func-name (name func))
         (all-return-types (type-set-to-type-list (v-return-spec func)))
         (flow-ids (map 'list #'flow-ids all-return-types)))
    (if (some #'type-doesnt-need-flow-id all-return-types)
        (error 'invalid-flow-id-multi-return :func-name func-name
               :return-type all-return-types)
        (mapcar #'(lambda (flow-id position)
                    (%calc-flow-id-given-args (in-arg-flow-ids func)
                                              flow-id
                                              arg-code-objs
                                              position))
                flow-ids
                (iota (length flow-ids))))))

;;----------------------------------------------------------------------

(defun add-glsl-funcs (env)
  (assert (typep env 'base-environment))
  (let ((funcs *global-env-form-bindings*)
        (binding-set (make-binding-hash-set)))
    (with-slots (form-bindings) env
      (loop :for key :in (hash-table-keys funcs) :do
         (loop :for func :in (gethash key funcs) :do
            (push-to-binding-set key func binding-set)))
      (setf form-bindings binding-set)
      env)))

;;----------------------------------------------------------------------
