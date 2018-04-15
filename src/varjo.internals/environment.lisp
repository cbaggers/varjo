(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------

(declaim
 (ftype (function (environment) base-environment)
        get-base-env)
 (inline get-base-env))
(defun get-base-env (env)
  (declare (type environment env)
           (optimize (speed 3) (safety 1) (debug 1)))
  (the base-environment (slot-value env 'base-env)))

;;-------------------------------------------------------------------------

(defmethod get-flow-id-for-stem-cell (stem-cell-symbol (e environment))
  (with-slots (stemcell->flow-id) (get-base-env e)
    (or (gethash stem-cell-symbol stemcell->flow-id)
        (let ((flow-id (flow-id!)))
          (setf (gethash stem-cell-symbol stemcell->flow-id)
                flow-id)
          flow-id))))

(defmethod get-stemcell-name-for-flow-id (id (e environment))
  (block nil
    (with-slots (stemcell->flow-id) (get-base-env e)
      (maphash (lambda (k v)
                 (when (eq v id)
                   (return k)))
               stemcell->flow-id))))

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
            (remove-if (lambda (f) (= (call-count f) 0))
                       (hash-table-values
                        (slot-value (get-base-env e) 'compiled-functions))))
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

(defmethod allows-stemcellsp ((e environment))
  (allows-stemcellsp (get-base-env e)))

(defmethod map-environments (func (e environment))
  (cons (funcall func e)
        (let ((parent (v-parent-env e)))
          (when parent
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
  (let ((result (make-instance 'base-environment
                               :stage stage
                               :env-depth 0
                               :context (context stage)
                               :stemcells-allowed stemcells-allowed)))
    (setf (slot-value result 'base-env) result)
    result))

;;-------------------------------------------------------------------------
;; global env

(defgeneric v-global-form-bindings ()
  (:method ()
    *global-env-form-bindings*))

;;-------------------------------------------------------------------------
;;
;; A note on the lack of copy-environment
;;
;; Every now and again I decide it would simplify a bunch of these functions
;; if I have a copy-environment function, you might even be here after
;; searching for such a function (hi!). The reason we dont have it is that
;; two of environment's slots (local-metadata & multi-val-safe) have very
;; specific rules and conditions that mean it should not be trivial to
;; propagate them.
;;
;; 'No problem..' past me says 'I'll call it %copy-environment and only use
;; it in this file'. No. Dont.
;;
;; In order to make the %copy-environment function communicate the issue
;; clearly you end up putting those slots as mandatory parameters..and then
;; they have weird rules.. and then what have we gained? another function call
;; and no simplification.
;;
;; My advice is to:
;; - only make-instances of 'environment from here, expose functions to other
;;   parts of the system
;; - audit any new code thoroughly.


(defun remove-main-method-flag-from-env (env)
  (assert (typep env 'environment))
  (make-instance 'environment
                 :base-env (get-base-env env)
                 :symbol-bindings (v-symbol-bindings env)
                 :form-bindings (v-form-bindings env)
                 :context (remove :main (v-context env))
                 :function-scope (v-function-scope env)
                 :parent-env (v-parent-env env)
                 :env-depth (env-depth env)
                 :allowed-outer-vars (v-allowed-outer-vars env)
                 :previous-env-with-form-bindings (v-previous-env-with-form-bindings env)
                 :ext-func-compile-chain (ext-func-compile-chain env)))

(defun fresh-environment (env &key context function-scope
                                form-bindings
                                symbol-bindings
                                (multi-val-base nil set-mvb)
                                multi-val-safe
                                (allowed-outer-vars nil set-aov)
                                (ext-func-compile-chain nil set-fbc))
  (assert (typep env 'environment))
  (make-instance 'environment
                 :base-env (get-base-env env)
                 :symbol-bindings symbol-bindings
                 :form-bindings form-bindings
                 :context (or context (v-context env))
                 :multi-val-base (if set-mvb
                                     multi-val-base
                                     (v-multi-val-base env))
                 :multi-val-safe multi-val-safe
                 :function-scope (or function-scope (v-function-scope env))
                 :parent-env env
                 :env-depth (1+ (env-depth env))
                 :previous-env-with-form-bindings
                 (if (v-form-bindings env)
                     env
                     (v-previous-env-with-form-bindings env))
                 :allowed-outer-vars (if set-aov
                                         allowed-outer-vars
                                         (v-allowed-outer-vars env))
                 :ext-func-compile-chain (if set-fbc
                                              ext-func-compile-chain
                                              (ext-func-compile-chain env))))

(defmacro with-fresh-env-scope ((name starting-env
                                      &key context function-scope
                                      form-bindings
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
                    :symbol-bindings ,symbol-bindings
                    ,@(when set-aov `(:allowed-outer-vars ,allowed-outer-vars))
                    ,@(when set-mvb `(:multi-val-base ,multi-val-base))
                    :multi-val-safe ,multi-val-safe)))
       (vbind (,r ,e) (progn ,@body)
         (assert ,e (,e) 'with-fresh-env-scope-missing-env)
         (values ,r (env-prune (env-depth ,s) ,e))))))

(defun env-replace-symbol-bindings (env symbol-bindings)
  (assert (typep env 'environment))
  (make-instance 'environment
                 :base-env (get-base-env env)
                 :symbol-bindings symbol-bindings
                 :form-bindings (v-form-bindings env)
                 :context (v-context env)
                 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
                 :env-depth (env-depth env)
                 :parent-env (v-parent-env env)
                 :previous-env-with-form-bindings (v-previous-env-with-form-bindings env)
                 :allowed-outer-vars (v-allowed-outer-vars env)
                 :ext-func-compile-chain (ext-func-compile-chain env)))

(defun env-replace-parent (env new-parent
                           &key (symbol-bindings nil symbol-bindings-set))
  (assert (typep env 'environment))
  (assert (typep new-parent 'environment))
  (make-instance 'environment
                 :base-env (get-base-env env)
                 :symbol-bindings (if symbol-bindings-set
                                      symbol-bindings
                                      (v-symbol-bindings env))
                 :form-bindings (v-form-bindings env)
                 :context (v-context env)
                 :env-depth (env-depth env)
                 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
                 :previous-env-with-form-bindings (if (v-form-bindings new-parent)
                                                      new-parent
                                                      (v-previous-env-with-form-bindings new-parent))
                 :parent-env new-parent
                 :allowed-outer-vars (v-allowed-outer-vars env)
                 :ext-func-compile-chain (ext-func-compile-chain env)))

(defun env-add-ext-funct-to-chain (env func)
  (assert (typep env 'environment))
  (make-instance 'environment
                 :base-env (get-base-env env)
                 :symbol-bindings (v-symbol-bindings env)
                 :form-bindings (v-form-bindings env)
                 :context (v-context env)
                 :env-depth (env-depth env)
                 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
                 :previous-env-with-form-bindings (v-previous-env-with-form-bindings env)
                 :parent-env env
                 :allowed-outer-vars (v-allowed-outer-vars env)
                 :ext-func-compile-chain (cons func (ext-func-compile-chain env))))

(defun env-prune* (to-depth &rest envs)
  (env-prune-many to-depth envs))

(defun env-prune-many (to-depth envs)
  "Remove the first 'to-depth' ancestors counting from the base-environment.
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
             (if (and stop-at-base (typep e 'base-environment))
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
  (with-slots ((a-vars symbol-bindings)
               (a-funcs form-bindings)
               (a-macros macros))
      env
    (with-slots ((b-vars symbol-bindings)
                 (b-funcs form-bindings)
                 (b-macros macros))
        new-env
      (fresh-environment
       env
       :symbol-bindings (%merge-symbol-bindings a-vars b-vars)
       :form-bindings (%merge-form-bindings a-funcs b-funcs)))))

;;------------------------------------------------------------

(defun make-binding-hash-set (&optional set)
  (let ((set (or set (make-hash-table :test #'eq))))
    (assert (hash-table-p set)) ;; {TODO} remove before shipping
    set))

(declaim (inline get-from-binding-set)
         (ftype (function (symbol hash-table) list)
                get-from-binding-set))
(defun get-from-binding-set (name ht)
  (gethash name ht))

(defun push-to-binding-set (name thing ht)
  (let ((ht (or ht (make-binding-hash-set))))
    (setf (gethash name ht)
          (cons thing (gethash name ht)))
    ht))

(defun %merge-symbol-bindings (a b)
  (reduce #'%merge-bindings-item b :initial-value a))

(defun %merge-form-bindings (a b)
  (let ((result (make-hash-table :test #'eq)))
    (flet ((add (name items)
             "if item is in A then append its entry to item in A"
             ;; find item in a
             (let* ((pre-existing-members (gethash name a)))
               (if pre-existing-members
                   ;; dont insert any item that's already in there
                   (let ((new-items
                          (remove-duplicates
                           (append items pre-existing-members))))
                     (setf (gethash name result) new-items))
                   ;; not found in A so add it
                   (setf (gethash name result) items))))
           (copy (k v)
             (setf (gethash k result) v)))
      (when a (maphash #'copy a))
      (when b (maphash #'add b))
      result)))

(defun %merge-bindings-item (a item-to-insert)
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

;;-------------------------------------------------------------------------

(defmethod add-global-compiler-macro ((macro v-compiler-macro))
  (setf (gethash (name macro) *global-env-compiler-macros*)
        (cons macro (gethash (name macro) *global-env-compiler-macros*)))
  macro)

;; {TODO} proper error
(defmethod add-compiler-macro (macro (env environment))
  (error "Varjo: Compiler Bug: Compiler macros can only be added to the global environment: ~a"
         (name macro)))

(defmethod get-global-compiler-macro (macro-name)
  (gethash macro-name *global-env-compiler-macros*))

(defun remove-global-compiler-macro (binding)
  (check-type binding v-compiler-macro)
  (let ((name (name binding)))
    (assert (gethash name *global-env-compiler-macros*))
    (setf (gethash name *global-env-compiler-macros*)
          (delete binding (gethash name *global-env-compiler-macros*)))
    nil))

;;-------------------------------------------------------------------------

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

;; {TODO} does get-symbol-binding need to return the env?
;;        A: yup, it's used over in the iteration fixpoint code
(defmethod get-symbol-binding (symbol respect-scope-rules (env environment))
  (declare (inline a-get))
  (let ((s (a-get1 symbol (v-symbol-bindings env))))
    (cond (s (if respect-scope-rules
                 (values (apply-scope-rules symbol s env) env)
                 (values s env)))
          (t (when (v-parent-env env)
               (get-symbol-binding symbol
                                   respect-scope-rules
                                   (v-parent-env env)))))))

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

;;-------------------------------------------------------------------------
;; Adding bindings for functions & macros


;; Global Env
;;
(defmethod add-global-form-binding ((macro-obj v-regular-macro))
  (let ((macro-name (name macro-obj)))
    (setf (gethash macro-name *global-env-form-bindings*)
          (list macro-obj)))
  macro-obj)

(defmethod add-global-form-binding ((func-obj v-function))
  (let* ((func-name (name func-obj))
         (current-bindings (gethash func-name *global-env-form-bindings*)))
    (if (find-if λ(typep _ 'v-regular-macro) current-bindings)
        (setf (gethash func-name *global-env-form-bindings*)
              (list func-obj))
        (setf (gethash func-name *global-env-form-bindings*)
              (cons func-obj (remove-if
                              (lambda (x)
                                (function-signatures-equal func-obj x))
                              current-bindings)))))
  func-obj)

(defun remove-global-form-binding (binding)
  (check-type binding (or v-function v-regular-macro))
  (let ((name (name binding)))
    (assert (gethash name *global-env-form-bindings*))
    (setf (gethash name *global-env-form-bindings*)
          (delete binding (gethash name *global-env-form-bindings*)))
    nil))

;; Standard Environment
;;

(defmethod add-form-binding ((compiled-func compiled-function-result)
                             (env environment))
  (let* ((func (function-obj compiled-func)))
    (when (shadow-global-check (name func))
      (assert (typep func 'v-function))
      (setf (compiled-functions env func) compiled-func)
      (add-form-binding func env))))

(defmethod add-form-binding ((func v-function)
                             (env environment))
  (let ((name (name func)))
    (when (shadow-global-check name)
      (fresh-environment
       env :form-bindings (push-to-binding-set
                           name
                           func
                           nil)))))

(defmethod add-form-binding ((macro v-regular-macro)
                             (env environment))
  (let ((name (name macro)))
    (when (shadow-global-check name)
      (fresh-environment
       env :form-bindings (push-to-binding-set
                           name
                           macro
                           nil)))))

(defmethod add-form-bindings ((funcs list)
                              (env environment))
  ;; local mutation of env here
  (let ((fo (loop
               :for fn :in funcs
               :when (shadow-global-check (name fn))
               :collect fn)))
    (if fo
        (let ((new-env (fresh-environment env))
              (set nil))
          (with-slots (form-bindings) new-env
            (loop :for fn :in fo :do
               (let ((fn (if (typep fn 'compiled-function-result)
                             (let ((fo (function-obj fn)))
                               (setf (compiled-functions env fo) fn)
                               fo)
                             fn)))
                 (setf set (push-to-binding-set (name fn) fn set))))
            (setf form-bindings set))
          new-env)
        env)))

;; Base Environment
(defmethod add-form-binding (anything (env base-environment))
  (declare (ignore anything env))
  (error "BUG: add-form-binding called on base-environment"))

(defmethod %add-function (func-name (func-spec v-function)
                          (env base-environment))
  (when (shadow-global-check func-name)
    (with-slots (form-bindings) env
      (push-to-binding-set func-name func-spec form-bindings))))

;;-------------------------------------------------------------------------

;; Global Environment
;;
(defmethod get-global-form-binding (name &optional include-external-functions)
  (let* ((bindings (gethash name *global-env-form-bindings*))
         (bindings (if include-external-functions
                       (append (get-external-function-by-name name nil)
                               bindings)
                       bindings))
         (macro-count (count-if λ(typep _ 'v-regular-macro) bindings))
         (func-count (count-if λ(or (typep _ 'v-function)
                                    (typep _ 'external-function))
                               bindings)))
    (cond
      ;;
      ((> macro-count 0) ;; {TODO} proper errors
       (assert (= macro-count 1))
       (assert (= func-count 0) ()
               "Functions and macros bound under same name in global env")
       (first bindings))
      ;;
      ((> func-count 0)
       (make-function-set bindings))
      (t nil))))

;; Standard Environment
;;
(defmethod get-form-binding (name (env environment))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;;
  (let* ((prev-env-with-bindings (v-previous-env-with-form-bindings env))
         (bindings-at-this-level
          (append (let ((set (v-form-bindings env)))
                    (when set
                      (get-from-binding-set name set)))
                  (unless prev-env-with-bindings
                    (get-external-function-by-name name env))))
         ;;
         (macro (find-if λ(typep _ 'v-regular-macro)
                         (the list bindings-at-this-level)))
         (macro (if prev-env-with-bindings
                    macro
                    (when (valid-for-contextp macro env)
                      macro))))
    ;;
    (if bindings-at-this-level
        ;; it's either a macro from this level or a function set
        (or macro
            (let* ((bindings-above
                    (when prev-env-with-bindings
                      (get-form-binding name prev-env-with-bindings)))
                   (all-bindings
                    (append bindings-at-this-level
                            (when (typep bindings-above 'v-function-set)
                              (functions bindings-above))))
                   (valid (if prev-env-with-bindings
                              all-bindings
                              (remove-if-not λ(valid-for-contextp _ env)
                                             all-bindings))))
              (make-function-set valid)))
        ;; nothing here? Check higher.
        (when prev-env-with-bindings
          (get-form-binding name prev-env-with-bindings)))))

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
  (let ((current (get-global-form-binding existing-name)))
    (if current
        (setf (gethash new-name *global-env-form-bindings*)
              (gethash existing-name *global-env-form-bindings*))
        (error 'could-not-find-any :name existing-name)))
  new-name)

;;-------------------------------------------------------------------------

(defun descendant-env-p (env ancestor)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type (or null environment) env ancestor))
  (or (eq env ancestor)
      (unless (or (null env) (null ancestor))
        (descendant-env-p (v-parent-env env)
                          ancestor))))
