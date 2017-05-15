(in-package :varjo)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------

(defmethod get-flow-id-for-stem-cell (stem-cell-symbol (e environment))
  (with-slots (stemcell->flow-id) (get-base-env e)
    (or (assocr stem-cell-symbol stemcell->flow-id)
        (let ((flow-id (flow-id!)))
          (push (cons stem-cell-symbol flow-id) stemcell->flow-id)
          flow-id))))

(defmethod get-stemcell-name-for-flow-id (id (e environment))
  (with-slots (stemcell->flow-id) (get-base-env e)
    (car (rassoc id stemcell->flow-id))))

(defmethod compiled-functions ((e environment) (key external-function))
  (gethash key (slot-value (get-base-env e) 'compiled-functions)))

(defmethod (setf compiled-functions) (value (e environment) key)
  ;; WARNING: MUTATES ENV
  (setf (gethash key (slot-value (get-base-env e) 'compiled-functions))
        value))

(defmethod all-cached-compiled-functions ((e environment))
  (hash-table-values (slot-value (get-base-env e) 'compiled-functions)))

(defmethod signatures ((e environment))
  ;; {TODO} we shouldnt have to remove-duplicates here
  ;;        find out why this is happening
  (remove-duplicates
   (mappend #'signatures
            (hash-table-values
             (slot-value (get-base-env e) 'compiled-functions)))
   :test #'equal))

(defmethod used-external-functions ((e environment))
  (let (functions)
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (when (typep k 'external-function)
         (push k functions)))
     (slot-value (get-base-env e) 'compiled-functions))
    functions))

(defun get-base-env (env)
  (let ((parent (v-parent-env env)))
    (if (not (eq parent *global-env*))
        (get-base-env parent)
        env)))

(defmethod allows-stemcellsp ((e environment))
  (allows-stemcellsp (get-base-env e)))

(defmethod map-environments (func (e environment))
  (cons (funcall func e)
        (let ((parent (v-parent-env e)))
          (when (not (eq parent *global-env*))
            (map-environments func parent)))))

;;-------------------------------------------------------------------------

(defmethod metadata-for-flow-id ((metadata-kind symbol)
                                 (flow-id flow-identifier)
                                 (env environment))
  (assert (= 1 (length (ids flow-id))) (flow-id)
          "Cannot declare metadata for multiple values at once: ~a" flow-id)
  (let ((key (slot-value (first (ids flow-id)) 'val))
        (env (get-base-env env)))
    (assocr metadata-kind (gethash key (slot-value env 'value-metadata)))))

(defmethod metadata-for-flow-id (metadata-kind flow-id (env expansion-env))
  (metadata-for-flow-id metadata-kind flow-id (slot-value env 'env)))

(defmethod (setf metadata-for-flow-id)
    ((data standard-value-metadata) (flow-id flow-identifier) (env environment))
  ;; WARNING: MUTATES ENV
  (assert (= 1 (length (ids flow-id))) (flow-id)
          "Cannot declare metadata for multiple values at once: ~a" flow-id)
  (let* ((key (slot-value (first (ids flow-id)) 'val))
         (env (get-base-env env))
         (metadata-kind (type-of data))
         (current-metadata (gethash key (slot-value env 'value-metadata)))
         (current-for-kind (assocr metadata-kind current-metadata))
         (new-meta (combine-metadata current-for-kind data)))
    (assert (or (null new-meta) (typep new-meta metadata-kind)) ()
            'metadata-combine-invalid-type :expected metadata-kind
            :found (type-of new-meta))
    (setf (gethash key (slot-value env 'value-metadata))
          (cons (cons metadata-kind new-meta) current-metadata))))

(defmethod metadata-for-scope ((metadata-kind symbol)
                               (env environment))
  (let ((func-scope (v-function-scope env)))
    (labels ((walk-envs (env)
               (or (gethash metadata-kind (slot-value env 'local-metadata))
                   (when (= (v-function-scope (v-parent-env env)) func-scope)
                     (walk-envs (v-parent-env env))))))
      (walk-envs env))))

(defmethod metadata-for-scope (metadata-kind (env expansion-env))
  (metadata-for-scope metadata-kind (slot-value env 'env)))

(defmethod (setf metadata-for-scope)
    ((data standard-scope-metadata) (env environment))
  ;; WARNING: MUTATES ENV
  (let* ((metadata-kind (type-of data))
         (current (gethash metadata-kind (slot-value env 'local-metadata))))
    (assert (null current))
    (setf (gethash metadata-kind (slot-value env 'local-metadata))
          data)))

;;-------------------------------------------------------------------------

;; WARNING:: This is mutated in translate.lisp & structs.lisp
(defmethod expanded-input-variables ((env environment))
  (expanded-input-variables (get-base-env env)))

;; WARNING:: This is mutated in translate.lisp & structs.lisp
(defmethod v-uniforms ((env environment))
  (v-uniforms (get-base-env env)))

;; WARNING: This is mutated in names.lisp
(defmethod v-name-map ((env environment))
  (slot-value (get-base-env env) 'name-map))

(defmethod stage ((env environment))
  (stage (get-base-env env)))

(defmethod primitive-in ((env environment))
  (primitive-in (stage env)))

(defmethod initialize-instance :after ((env environment) &rest initargs)
  (declare (ignore initargs))
  (unless (every λ(and (symbolp (first _))
                       (every λ(typep _ '(or v-value v-symbol-macro))
                              (rest _)))
                 (v-symbol-bindings env))
    (error 'invalid-env-vars :vars (v-symbol-bindings env))))

(defun %make-base-environment (stage &key stemcells-allowed)
  (make-instance 'base-environment
                 :stage stage
                 :context (context stage)
                 :stemcells-allowed stemcells-allowed))

;;-------------------------------------------------------------------------
;; global env

(defmethod v-form-bindings ((env (eql :-genv-)))
  (declare (ignore env))
  *global-env-form-bindings*)

;;-------------------------------------------------------------------------

(defun a-get (name list)
  (assocr name list))

(defun a-get1 (name list)
  (first (assocr name list)))

(defmacro a-add (name value list-place)
  `(acons ,name
          (cons ,value (assocr ,name ,list-place))
          ,list-place))


(defmacro a-set (name value list-place)
  (let ((g-list-place (gensym "list-place")))
    `(let ((,g-list-place (remove ,name ,list-place :key #'first)))
       (acons ,name (list ,value) ,g-list-place))))

(defmacro a-remove-all (name list-place)
  `(remove ,name ,list-place :key #'first))

;;-------------------------------------------------------------------------

(defun remove-main-method-flag-from-env (env)
  (assert (typep env 'environment))
  (make-instance 'environment
                 :symbol-bindings (v-symbol-bindings env)
                 :form-bindings (v-form-bindings env)
                 :macros nil
                 :context (remove :main (copy-list (v-context env)))
                 :function-scope (v-function-scope env)
                 :parent-env (v-parent-env env)
                 :allowed-outer-vars (v-allowed-outer-vars env)
                 :ext-func-compile-chain (ext-func-compile-chain env)))

(defun fresh-environment (env &key context function-scope
                                form-bindings macros
                                symbol-bindings
                                (multi-val-base nil set-mvb)
                                multi-val-safe
                                (allowed-outer-vars nil set-aov)
                                (ext-func-compile-chain nil set-fbc))
  (assert (typep env 'environment))
  (make-instance 'environment
                 :symbol-bindings symbol-bindings
                 :form-bindings form-bindings
                 :macros macros
                 :context (or context (copy-list (v-context env)))
                 :multi-val-base (if set-mvb
                                     multi-val-base
                                     (v-multi-val-base env))
                 :multi-val-safe multi-val-safe
                 :function-scope (or function-scope (v-function-scope env))
                 :parent-env env
                 :allowed-outer-vars (if set-aov
                                         allowed-outer-vars
                                         (v-allowed-outer-vars env))
                 :ext-func-compile-chain (if set-fbc
                                              ext-func-compile-chain
                                              (ext-func-compile-chain env))))

(defmacro with-fresh-env-scope ((name starting-env
                                      &key context function-scope
                                      form-bindings macros
                                      symbol-bindings
                                      (multi-val-base nil set-mvb)
                                      multi-val-safe
                                      (allowed-outer-vars nil set-aov))
                                &body body)
  (let ((s (gensym "starting-env"))
        (r (gensym "result"))
        (e (gensym "final-env")))
    `(let* ((,s ,starting-env)
            (,name (fresh-environment
                    ,s :context ,context
                    :function-scope ,function-scope
                    :form-bindings ,form-bindings
                    :macros ,macros
                    :symbol-bindings ,symbol-bindings
                    ,@(when set-aov `(:allowed-outer-vars ,allowed-outer-vars))
                    ,@(when set-mvb `(:multi-val-base ,multi-val-base))
                    :multi-val-safe ,multi-val-safe)))
       (vbind (,r ,e) (progn ,@body)
         (assert ,e (,e) 'with-fresh-env-scope-missing-env)
         (values ,r (env-prune (env-depth ,s) ,e))))))

(defun env-replace-parent (env new-parent
                           &key (symbol-bindings nil symbol-bindings-set))
  (assert (typep env 'environment))
  (assert (typep new-parent 'environment))
  (make-instance 'environment
                 :symbol-bindings (if symbol-bindings-set
                                      symbol-bindings
                                      (v-symbol-bindings env))
                 :form-bindings (v-form-bindings env)
                 :macros (v-macros env)
                 :context (copy-list (v-context env))
                 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
                 :parent-env new-parent
                 :allowed-outer-vars (v-allowed-outer-vars env)
                 :ext-func-compile-chain (ext-func-compile-chain env)))

(defun env-depth (env)
  (labels ((dist (e &optional (accum 0))
             (let ((p (v-parent-env e)))
               (if (eq p *global-env*) accum (dist p (1+ accum))))))
    (dist env)))

(defun env-prune* (to-depth &rest envs)
  "Remove the first 'to-depth' accestors counting from the base-environment.
For example calling env-prune on this environment..

    base-env -> env0 -> env1 -> env2 -> env3 -> env4 -> env5

.. with a to-depth of 3 will return:

    env2 -> env3 -> env4 -> env5
"
  (assert (every (lambda (x) (typep x 'environment)) envs))
  (labels ((%up (e count)
             (if (> count 0)
                 (%up (v-parent-env e) (1- count))
                 e))
           (up (e)
             (let ((c (- (env-depth e) to-depth)))
               (assert (>= c 0))
               (%up e c))))
    (mapcar #'up envs)))

(defun env-prune (to-depth env)
  (first (env-prune* to-depth env)))

(defun env-merge-history (env-a env-b)
  (assert (= (env-depth env-a) (env-depth env-b)))
  (labels ((walk-env-parents (a b)
             (if (eq a b)
                 a
                 (env-replace-parent
                  a (walk-env-parents (v-parent-env a) (v-parent-env b))
                  :symbol-bindings (merge-variable-histories a b)))))
    (walk-env-parents env-a env-b)))

(defun merge-variable-histories (env-a env-b)
  ;; we can be sure that both have the var names as assignment
  ;; can only affect flow id, not type
  (let* ((a (remove-if λ(typep _ 'v-symbol-macro)
                       (v-symbol-bindings env-a)))
         (v-names (mapcar #'first a)))
    (mapcar
     (lambda (n)
       (let ((va (get-symbol-binding n nil env-a))
             (vb (get-symbol-binding n nil env-b)))
         (if (eq va vb)
             `(,n ,va)
             `(,n
               ,(v-make-value
                 (replace-flow-id (v-type-of va)
                                  (flow-id! (flow-ids va) (flow-ids vb)))
                 env-a ;; this is ignored as function-scope is provided
                 :read-only (v-read-only va)
                 :function-scope (v-function-scope va)
                 :glsl-name (glsl-name va))))))
     v-names)))

(defun env-binding-names (env &key stop-at-base variables-only)
  "Walk up the environment tree and collect the names of all symbol-bindings
   If stop-at-base is true then this list will not include the global env.
   The order of the result may not reflect the depth of the scope. Do not
   rely on the order for any kind of information"
  (labels ((collect (e accum)
             (walk (v-parent-env e)
                   (remove-duplicates
                    (let ((bindings (mapcar #'first (v-symbol-bindings e))))
                      (append (if variables-only
                                  (remove-if λ(typep _ 'v-symbol-macro)
                                             bindings)
                                  bindings)
                              accum))
                    :test #'eq
                    :from-end t)))
           (walk (e accum)
             (if (or (and stop-at-base (typep e 'base-environment))
                     (eq e *global-env*))
                 accum
                 (collect e accum))))
    (walk env nil)))

(defun find-env-bindings (env-a env-b
                          &key (test #'eq) stop-at-base (variables-only t))
  "Look at every variable binding in both the supplied environments
   and return the names of the bindings that match"
  ;;
  (let ((names-a (env-binding-names env-a :stop-at-base stop-at-base))
        (names-b (env-binding-names env-a :stop-at-base stop-at-base)))
    (assert (equal names-a names-b))
    (labels ((macrop (x) (typep x 'v-symbol-macro))
             (v-eq (name)
               (let ((binding-a (get-symbol-binding name nil env-a))
                     (binding-b (get-symbol-binding name nil env-b)))
                 (if (and (or (macrop binding-a) (macrop binding-b))
                          variables-only)
                     (progn ;; {TODO} proper error
                       (assert (and (macrop binding-a) (macrop binding-b)) ()
                               "Varjo: Compiler Bug: Found a case in #'find-env-bindings where ~a was a symbol-macro in one env and not the other"
                               name))
                     (funcall test binding-a binding-b)))))
      (remove-if-not #'v-eq names-a))))

(defun merge-env (env new-env)
  (unless (= (v-function-scope env) (v-function-scope new-env))
    (error 'merge-env-func-scope-mismatch :env-a env :env-b new-env))
  (with-slots ((a-vars symbol-bindings) (a-funcs form-bindings)
               (a-macros macros)) env
    (with-slots ((b-vars symbol-bindings) (b-funcs form-bindings)
                 (b-macros macros)) new-env
      (fresh-environment
       env
       :symbol-bindings (%merge-env-lists a-vars b-vars)
       :form-bindings (%merge-env-lists a-funcs b-funcs)
       :macros (%merge-env-lists a-macros b-macros)))))

(defun %merge-env-lists (a b)
  (reduce #'%merge-env-lists-item b :initial-value a))

(defun %merge-env-lists-item (a item-to-insert)
  "if item is in A then append its entry to item in A"
  ;; find item in a
  (let* ((pre-exisiting-item (find (first item-to-insert) a :key #'first))
         (pre-existing-members (rest pre-exisiting-item)))
    (if pre-exisiting-item
        ;; dont insert any item that's already in there
        (let ((to-insert
               (remove-if (lambda (x) (not (member x pre-existing-members)))
                          (rest item-to-insert))))
          (cons (cons (first pre-exisiting-item)
                      (append to-insert pre-existing-members))
                (remove (first item-to-insert) a :key #'first))
          a)
        ;; not found in A so add it
        (cons item-to-insert a))))

;;-------------------------------------------------------------------------

(defun context-ok-given-restriction (context restriction)
  (loop :for item :in restriction :always
     (if (listp item)
         (find-if (lambda (_) (member _ context)) item)
         (find item context))))

(defun shadow-global-check (name)
  (when (find name *unshadowable-names*)
    (error 'cannot-not-shadow-core))
  t)

(defun get-version-from-context (env)
  (or (get-version-from-context-list (v-context env))
      (error 'no-version-in-context :env env)))

(defun get-version-from-context-list (list)
  (loop :for item :in list
     :when (find item *supported-versions*)
     :return item))

(defun get-stage-from-env (env)
  (get-version-from-context (v-context env)))

(defun get-primitive-type-from-context (context)
  (or (find-if λ(or (member _ *supported-draw-modes*)
                    (and (listp _)
                         (string= (first _) "PATCH")
                         (integerp (second _))))
               context)
      :triangles))

(defun get-stage-kind-from-context (context)
  (find-if λ(member _ *stage-names*) context))

;;-------------------------------------------------------------------------

(defmethod add-compiler-macro ((macro v-compiler-macro) (env (eql :-genv-)))
  (setf (gethash (name macro) *global-env-compiler-macros*)
        (cons macro (gethash (name macro) *global-env-compiler-macros*)))
  *global-env*)

;; {TODO} proper error
(defmethod add-compiler-macro (macro (env environment))
  (error "Varjo: Compiler Bug: Compiler macros can only be added to the global environment: ~a"
         (name macro)))

(defmethod get-compiler-macro (macro-name (env (eql :-genv-)))
  (gethash macro-name *global-env-compiler-macros*))

(defmethod get-compiler-macro (macro-name (env environment))
  (get-compiler-macro macro-name *global-env*))

;;-------------------------------------------------------------------------

;;[TODO] really no better way of doing this?
(defun vtype-existsp (type-name)
  (etypecase type-name
    (symbol
     (and type-name
          (find-class type-name nil)
          (values (subtypep type-name 'v-type))))
    (list (vtype-existsp (first type-name)))))

;;-------------------------------------------------------------------------

(defmethod add-symbol-binding (name (val v-value) (env (eql :-genv-)))
  (setf (gethash name *global-env-symbol-bindings*) val)
  *global-env*)

(defmethod add-symbol-binding (name (val uninitialized-value)
                               (env (eql :-genv-)))
  (error 'global-uninitialized-var :name name))

(defmethod add-symbol-binding (name (macro v-symbol-macro) (env (eql :-genv-)))
  (setf (gethash name *global-env-symbol-bindings*) macro)
  *global-env*)

(defmethod %add-symbol-binding (var-name (val v-value) (env base-environment))
  "Warning - Destructive: Used when we don't want to create a fresh environment.
   This is used when setting up the environment prior to starting the actual
   compilation"
  (setf (slot-value env 'symbol-bindings)
        (a-set var-name val (v-symbol-bindings env))))

(defmethod %add-symbol-binding (name (macro v-symbol-macro)
                                (env base-environment))
  "Warning - Destructive: Used when we don't want to create a fresh environment.
   This is used when setting up the environment prior to starting the actual
   compilation"
  (setf (slot-value env 'symbol-bindings)
        (a-set name macro (v-symbol-bindings env))))

(defmethod add-symbol-binding (name (val v-value) (env environment))
  (fresh-environment env :symbol-bindings (a-add name val
                                                 (v-symbol-bindings env))))

(defmethod add-symbol-binding (name (macro v-symbol-macro) (env environment))
  (fresh-environment env :symbol-bindings (a-add name macro
                                                 (v-symbol-bindings env))))

(defun binding-accesible-p (env binding &optional binding-name)
  (let ((from-higher-scope (binding-in-higher-scope-p binding env)))
    (when (or (not from-higher-scope)
              (or (eq t (v-allowed-outer-vars env))
                  (when binding-name
                    (find binding-name (v-allowed-outer-vars env)))))
      t)))

(defgeneric apply-scope-rules (binding-name binding env)
  (:method (binding-name binding env)
    (let ((from-higher-scope (binding-in-higher-scope-p binding env)))
      (when (or (not from-higher-scope)
                (or (eq t (v-allowed-outer-vars env))
                    (find binding-name (v-allowed-outer-vars env))))
        binding)))
  (:method (binding-name (binding uninitialized-value) env)
    (declare (ignore binding-name env))
    binding))

(defmethod binding-in-higher-scope-p ((binding v-value) env)
  (let ((val-scope (v-function-scope binding)))
    (and (> val-scope 0)
         (< val-scope (v-function-scope env)))))

(defmethod binding-in-higher-scope-p ((binding v-symbol-macro) env)
  (let ((val-scope (v-function-scope binding)))
    (and (> val-scope 0)
         (< val-scope (v-function-scope env)))))

(defmethod binding-in-higher-scope-p ((binding v-regular-macro) env)
  (let ((val-scope (v-function-scope binding)))
    (and (> val-scope 0)
         (< val-scope (v-function-scope env)))))

(defmethod binding-in-higher-scope-p ((name symbol) env)
  (let* ((binding (get-symbol-binding name nil env)))
    (assert binding (name) 'symbol-unidentified :sym name)
    (binding-in-higher-scope-p binding env)))

(defmethod get-symbol-binding (symbol respect-scope-rules (env (eql :-genv-)))
  (declare (ignore respect-scope-rules))
  (let ((s (gethash symbol *global-env-symbol-bindings*)))
    (when s
      (values s *global-env*))))

;; {TODO} does get-symbol-binding need to return the env?
(defmethod get-symbol-binding (symbol respect-scope-rules (env environment))
  (let ((s (first (a-get symbol (v-symbol-bindings env)))))
    (cond (s (if respect-scope-rules
                 (values (apply-scope-rules symbol s env) env)
                 (values s env)))
          (t (get-symbol-binding symbol
                                 respect-scope-rules
                                 (v-parent-env env))))))

(defmethod v-symbol-bindings ((env (eql :-genv-)))
  nil)

;;-------------------------------------------------------------------------
;; Adding bindings for functions & macros


;; Global Env
;;
(defmethod add-form-binding ((macro-obj v-regular-macro) (env (eql :-genv-)))
  (let ((macro-name (name macro-obj)))
    (setf (gethash macro-name *global-env-form-bindings*)
          (list macro-obj)))
  *global-env*)

(defmethod add-form-binding ((func-obj v-function) (env (eql :-genv-)))
  (let* ((func-name (name func-obj))
         (current-bindings (gethash func-name *global-env-form-bindings*)))
    (if (find-if λ(typep _ 'v-regular-macro) current-bindings)
        (setf (gethash func-name *global-env-form-bindings*)
              (list func-obj))
        (setf (gethash func-name *global-env-form-bindings*)
              (cons func-obj current-bindings))))
  *global-env*)

;; Standard Environment
;;
(defmethod add-form-binding ((compiled-func compiled-function-result)
                             (env environment))
  (let* ((func (function-obj compiled-func))
         (func-name (name func)))
    (when (shadow-global-check func-name)
      (assert (typep func 'v-function))
      (setf (compiled-functions env func) compiled-func)
      (add-form-binding func env))))

(defmethod add-form-binding ((func v-function) (env environment))
  (let ((name (name func)))
    (when (shadow-global-check name)
      (fresh-environment
       env :form-bindings (a-add name func (v-form-bindings env))))))

(defmethod add-form-binding ((macro v-regular-macro) (env environment))
  (let ((name (name macro)))
    (when (shadow-global-check name)
      (fresh-environment
       env :form-bindings (a-add name macro (v-form-bindings env))))))

;; Base Environment
(defmethod %add-function (func-name (func-spec v-function)
                          (env base-environment))
  (when (shadow-global-check func-name)
    (setf (slot-value env 'form-bindings)
          (a-add func-name func-spec (v-form-bindings env)))))

;;-------------------------------------------------------------------------

;; Global Environment
;;
(defmethod get-form-binding (name (env (eql :-genv-)))
  (let* ((bindings (gethash name *global-env-form-bindings*))
         (macro-count (count-if λ(typep _ 'v-regular-macro) bindings))
         (func-count (count-if λ(typep _ 'v-function) bindings)))
    (cond
      ;;
      ((> macro-count 0) ;; {TODO} proper errors
       (assert (= macro-count 1))
       (assert (= func-count 0) ()
               "Functions and macros bound under same name in global env")
       (first bindings))
      ;;
      ((> func-count 0)
       (make-function-set (gethash name *global-env-form-bindings*)))
      (t nil))))

;; Standard Environment
;;
(defmethod get-form-binding (name (env environment))
  ;;
  (let* ((bindings-at-this-level
          (append (a-get name (v-form-bindings env))
                  (when (typep env 'base-environment)
                    (get-external-function-by-name name env))))
         ;;
         (macro (find-if λ(typep _ 'v-regular-macro) bindings-at-this-level))
         (macro (when (valid-for-contextp macro env)
                  macro)))
    ;;
    (if bindings-at-this-level
        ;; it's either a macro from this level or a function set
        (or macro
            (let* ((bindings-above (get-form-binding name (v-parent-env env)))
                   (all-bindings
                    (append bindings-at-this-level
                            (when (typep bindings-above 'v-function-set)
                              (functions bindings-above))))
                   (valid (remove-if-not λ(valid-for-contextp _ env)
                                         all-bindings)))
              (make-function-set valid)))
        ;; nothing here? Check higher.
        (get-form-binding name (v-parent-env env)))))

;;-------------------------------------------------------------------------

(defun v-boundp (var-name env)
  (not (null (get-symbol-binding var-name t env))))

(defun v-fboundp (func-name env)
  (not (null (get-form-binding func-name env))))

;;-------------------------------------------------------------------------

(defmethod valid-for-contextp ((func v-function) (env environment))
  (let ((versions (v-versions func))
        (context (v-context env)))
    (%valid-for-contextp func versions context)))

(defmethod valid-for-contextp ((func v-function) (env (eql *global-env*)))
  (let ((versions (v-versions func))
        (context (v-context env)))
    (%valid-for-contextp func versions context)))

(defmethod valid-for-contextp ((func list) (env environment))
  (let ((versions (second func))
        (context (v-context env)))
    (%valid-for-contextp func versions context)))

(defmethod valid-for-contextp ((func external-function) env)
  (declare (ignore env))
  t)

(defmethod valid-for-contextp ((func v-regular-macro) env)
  (declare (ignore env))
  t)

(defun %valid-for-contextp (func versions context)
  (if versions
      (when (some λ(member _ context) versions)
        func)
      func))

(defmethod add-equivalent-name (existing-name new-name)
  (let ((current (get-form-binding existing-name *global-env*)))
    (if current
        (setf (gethash new-name *global-env-form-bindings*)
              (gethash existing-name *global-env-form-bindings*))
        (error 'could-not-find-any :name existing-name)))
  new-name)

;;-------------------------------------------------------------------------

(defun descendant-env-p (env ancestor)
  (cond
    ((eq env ancestor) t)
    ((eq env *global-env*) nil)
    (t (descendant-env-p (v-parent-env env) ancestor))))
