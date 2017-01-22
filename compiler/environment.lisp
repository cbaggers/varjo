(in-package :varjo)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------

(defmethod get-flow-id-for-stem-cell (stem-cell-symbol (e environment))
  (with-slots (stemcell->flow-id) (get-base-env e)
    (or (assocr stem-cell-symbol stemcell->flow-id)
        (let ((flow-id (flow-id!)))
          (push (cons stem-cell-symbol flow-id) stemcell->flow-id)
          flow-id))))

(defmethod compiled-functions ((e environment) (key external-function))
  (gethash key (slot-value (get-base-env e) 'compiled-functions)))

(defmethod all-cached-compiled-functions ((e environment))
  (hash-table-values (slot-value (get-base-env e) 'compiled-functions)))

(defmethod signatures ((e environment))
  (let (signatures)
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (push (signatures v) signatures))
     (slot-value (get-base-env e) 'compiled-functions))
    ;; {TODO} we shouldnt have to remove-duplicates here
    ;;        find out why this is happening
    (remove-duplicates (reduce #'append signatures)
                       :test #'equal)))

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

(defmethod metadata-for-flow-id ((flow-id flow-identifier) (env environment))
  (assert (= 1 (length (ids flow-id))) (flow-id)
          "Cannot declare metadata for multiple values at once: ~a" flow-id)
  (let ((key (slot-value (first (ids flow-id)) 'val))
        (env (get-base-env env)))
    (gethash key (slot-value env 'value-metadata))))

;; ugh
;;
(defmethod (setf metadata-for-flow-id)
    ((data standard-value-metadata) (flow-id flow-identifier) (env environment))
  (assert (= 1 (length (ids flow-id))) (flow-id)
          "Cannot declare metadata for multiple values at once: ~a" flow-id)
  (let* ((key (slot-value (first (ids flow-id)) 'val))
         (env (get-base-env env))
         (metadata-kind (type-of data))
         (current-metadata (gethash key (slot-value env 'value-metadata))))
    (assert (null (assoc metadata-kind current-metadata)) (current-metadata)
            "Varjo: ~a metadata already found for flow-id ~a.~%Metadata cannot be redefined"
            metadata-kind flow-id)
    (setf (gethash key (slot-value env 'value-metadata))
          (cons (cons (type-of data) data) current-metadata))))

(defmethod (setf compiled-functions)
    (value (e environment) key)
  (setf (gethash key (slot-value (get-base-env e) 'compiled-functions))
        value))

;; WARNING:: This is mutated in translate.lisp
(defmethod v-raw-in-args ((env environment))
  (v-raw-in-args (get-base-env env)))

;; WARNING:: This is mutated in translate.lisp
(defmethod v-raw-uniforms ((env environment))
  (v-raw-uniforms (get-base-env env)))

;; WARNING:: This is mutated in translate.lisp
(defmethod v-raw-context ((env environment))
  (v-raw-context (get-base-env env)))

;; WARNING:: This is mutated in translate.lisp & structs.lisp
(defmethod v-in-args ((env environment))
  (v-in-args (get-base-env env)))

;; WARNING:: This is mutated in translate.lisp & structs.lisp
(defmethod v-uniforms ((env environment))
  (v-uniforms (get-base-env env)))

;; WARNING: This is mutated in names.lisp
(defmethod v-name-map ((env environment))
  (slot-value (get-base-env env) 'name-map))

(defmethod initialize-instance :after ((env environment) &rest initargs)
  (declare (ignore initargs))
  (unless (every λ(and (symbolp (first _))
                       (every λ(typep _ '(or v-value v-symbol-macro))
                              (rest _)))
                 (v-symbol-bindings env))
    (error 'invalid-env-vars :vars (v-symbol-bindings env))))

(defun %make-base-environment (&key (third-party-metadata (make-hash-table))
                                 stemcells-allowed)
  (make-instance 'base-environment
                 :stemcells-allowed stemcells-allowed
                 :third-party-metadata third-party-metadata))

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
                 :compiler-macros nil
                 :context (remove :main (copy-list (v-context env)))
                 :function-scope (v-function-scope env)
                 :parent-env (v-parent-env env)
                 :allowed-outer-vars (v-allowed-outer-vars env)))

(defun fresh-environment (env &key context function-scope
                                form-bindings macros
                                compiler-macros symbol-bindings
                                (multi-val-base nil set-mvb)
                                multi-val-safe
                                (allowed-outer-vars nil set-aov))
  (assert (typep env 'environment))
  (make-instance 'environment
                 :symbol-bindings symbol-bindings
                 :form-bindings form-bindings
                 :macros macros
                 :compiler-macros compiler-macros
                 :context (or context (copy-list (v-context env)))
                 :multi-val-base (if set-mvb
                                     multi-val-base
                                     (v-multi-val-base env))
                 :multi-val-safe multi-val-safe
                 :function-scope (or function-scope (v-function-scope env))
                 :parent-env env
                 :allowed-outer-vars (if set-aov
                                         allowed-outer-vars
                                         (v-allowed-outer-vars env))))

(defmacro with-fresh-env-scope ((name starting-env
                                      &key context function-scope
                                      form-bindings macros
                                      compiler-macros symbol-bindings
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
                    :compiler-macros ,compiler-macros
                    :symbol-bindings ,symbol-bindings
                    ,@(when set-aov `(:allowed-outer-vars ,allowed-outer-vars))
                    ,@(when set-mvb `(:multi-val-base ,multi-val-base))
                    :multi-val-safe ,multi-val-safe)))
       (vbind (,r ,e) (progn ,@body)
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
                 :compiler-macros (v-compiler-macros env)
                 :context (copy-list (v-context env))
                 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
                 :parent-env new-parent
                 :allowed-outer-vars (v-allowed-outer-vars env)))

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
  (labels ((w (a b)
             (if (eq a b)
                 a
                 (env-replace-parent
                  a (w (v-parent-env a) (v-parent-env b))
                  :symbol-bindings (merge-variable-histories a b)))))
    (w env-a env-b)))

(defun merge-variable-histories (env-a env-b)
  ;; we can be sure that both have the var names as assignment
  ;; can only affect flow id, not type
  (warn "merge-variable-histories is incomplete: handle symbol-macros")
  (let* ((a (v-symbol-bindings env-a))
         (v-names (mapcar #'first a)))
    (mapcar
     (lambda (n)
       (let ((va (get-symbol-binding n nil env-a))
             (vb (get-symbol-binding n nil env-b)))
         (if (eq va vb)
             `(,n ,va)
             `(,n
               ,(v-make-value
                 (replace-flow-id (v-type va)
                                  (flow-id! (flow-ids va) (flow-ids vb)))
                 env-a ;; this is ignored as function-scope is provided
                 :read-only (v-read-only va)
                 :function-scope (v-function-scope va)
                 :glsl-name (v-glsl-name va))))))
     v-names)))

(defun env-binding-names (env &key stop-at-base)
  "Walk up the environment tree and collect the names of all symbol-bindings
   If stop-at-base is true then this list will not include the global env.
   The order of the result may not reflect the depth of the scope. Do not
   rely on the order for any kind of information"
  (labels ((w (e accum)
             (if (or (eq e *global-env*)
                     (and stop-at-base
                          (typep e 'base-environment)))
                 accum
                 (w (v-parent-env e)
                    (remove-duplicates
                     (append (mapcar #'first (v-symbol-bindings e))
                             accum)
                     :test #'eq
                     :from-end t)))))
    (w env nil)))

(defun find-env-bindings (env-a env-b &key (test #'eq) stop-at-base)
  "Look at every variable binding in both the supplied environments
   and return the bindings that match"
  (warn "find-env-bindings is incomplete: what about symbol-macros")
  (let ((n-a (env-binding-names env-a :stop-at-base stop-at-base))
        (n-b (env-binding-names env-a :stop-at-base stop-at-base)))
    (assert (equal n-a n-b))
    (labels ((v-eq (n)
               (funcall test (get-symbol-binding n nil env-a)
                        (get-symbol-binding n nil env-b))))
      (loop for n in n-a if (v-eq n) collect n))))

(defun merge-env (env new-env)
  (unless (= (v-function-scope env) (v-function-scope new-env))
    (error 'merge-env-func-scope-mismatch :env-a env :env-b new-env))
  (with-slots ((a-vars symbol-bindings) (a-funcs form-bindings) (a-macros macros)
               (a-cmacros compiler-macros)) env
    (with-slots ((b-vars symbol-bindings) (b-funcs form-bindings) (b-macros macros)
                 (b-cmacros compiler-macros)) new-env
      (fresh-environment
       env
       :symbol-bindings (%merge-env-lists a-vars b-vars)
       :form-bindings (%merge-env-lists a-funcs b-funcs)
       :macros (%merge-env-lists a-macros b-macros)
       :compiler-macros (%merge-env-lists a-cmacros b-cmacros)))))

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

(defun shadow-global-check (name &key (specials t) (c-macros t))
  (when (and c-macros (get-compiler-macro name *global-env*))
    (error 'cannot-not-shadow-core))
  (when specials
    (loop :for func :in (functions (get-func-set-by-name name *global-env*))
       :if (and specials (v-special-functionp func))
       :do (error 'cannot-not-shadow-core)))
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

(defun get-stage-from-context (context)
  (find-if (lambda (x) (member x *supported-stages*)) context))


(defun get-primitive-type-from-context (context)
  (or (find-if λ(member _ *supported-draw-modes*) context)
      :triangles))

;;{TODO} move errors to correct place
;; (defun get-primitive-length (prim-type)
;;   (let ((pos (position prim-type prims)))
;;     (if pos
;;         (1+ pos)
;;         (error "Varjo: Not a valid primitive type"))))

;;-------------------------------------------------------------------------

(defmethod add-compiler-macro (macro-name (macro function) (context list)
                               (env (eql :-genv-)))
  (setf (gethash macro-name *global-env-compiler-macros*) `(,macro ,context))
  *global-env*)

(defmethod add-compiler-macro (macro-name (macro function) (context list)
                               (env environment))
  (when (shadow-global-check macro-name :specials nil :c-macros t)
    (let ((c-macros
           (a-set macro-name `(,macro ,context) (v-compiler-macros env))))
      (fresh-environment env :compiler-macros c-macros))))

(defmethod get-compiler-macro (macro-name (env (eql :-genv-)))
  (gethash macro-name *global-env-compiler-macros*))

(defmethod %get-compiler-macro-spec (macro-name (env (eql :-genv-)))
  (get-compiler-macro macro-name env))

(defmethod %get-compiler-macro-spec (macro-name (env environment))
  (or (a-get1 macro-name (v-compiler-macros env))
      (%get-compiler-macro-spec macro-name (v-parent-env env))))

(defmethod get-compiler-macro (macro-name (env environment))
  (let ((spec (%get-compiler-macro-spec macro-name env)))
    (when (and spec (valid-for-contextp spec env))
      (first spec))))

;;-------------------------------------------------------------------------

;;[TODO] really no better way of doing this?
(defun vtype-existsp (type-name)
  (and type-name
       (find-class type-name nil)
       (handler-case (progn (typep (make-instance type-name) 'v-type)
                            t)
         (error () nil))))

;;-------------------------------------------------------------------------

(defmethod add-symbol-binding (name (val v-value) (env (eql :-genv-)))
  (setf (gethash name *global-env-symbol-bindings*) val)
  *global-env*)

(defmethod add-symbol-binding (name (macro v-symbol-macro) (env (eql :-genv-)))
  (setf (gethash name *global-env-symbol-bindings*) macro)
  *global-env*)

(defmethod %add-symbol-binding (var-name (val v-value) (env base-environment))
  "Warning - Destructive: Used when we don't want to create a fresh environment.
   This is used when setting up the environment prior to starting the actual
   compilation"
  (setf (slot-value env 'symbol-bindings)
        (a-add var-name val (v-symbol-bindings env))))

(defmethod %add-symbol-binding (name (macro v-symbol-macro)
                                (env base-environment))
  "Warning - Destructive: Used when we don't want to create a fresh environment.
   This is used when setting up the environment prior to starting the actual
   compilation"
  (setf (slot-value env 'symbol-bindings)
        (a-add name macro (v-symbol-bindings env))))

(defmethod add-symbol-binding (name (val v-value) (env environment))
  (fresh-environment env :symbol-bindings (a-add name val
                                                 (v-symbol-bindings env))))

(defmethod add-symbol-binding (name (macro v-symbol-macro) (env environment))
  (fresh-environment env :symbol-bindings (a-add name macro
                                                 (v-symbol-bindings env))))

(defun apply-scope-rules (binding-name binding env)
  (let ((from-higher-scope (binding-in-higher-scope-p binding env)))
    (when (or (not from-higher-scope)
              (or (eq t (v-allowed-outer-vars env))
                  (find binding-name (v-allowed-outer-vars env))))
      binding)))

(defmethod binding-in-higher-scope-p ((binding v-value) env)
  (let ((val-scope (v-function-scope binding)))
    (and (> val-scope 0)
         (< val-scope (v-function-scope env)))))

(defmethod binding-in-higher-scope-p ((binding v-symbol-macro) env)
  (let ((val-scope (v-function-scope binding)))
    (and (> val-scope 0)
         (< val-scope (v-function-scope env)))))

(defmethod binding-in-higher-scope-p ((name symbol) env)
  (let* ((binding (get-symbol-binding name nil env)))
    (assert binding (name) 'symbol-unidentified :sym name)
    (binding-in-higher-scope-p binding env )))

(defmethod get-symbol-binding (var-name respect-scope-rules (env (eql :-genv-)))
  (declare (ignore respect-scope-rules))
  (warn "env get-symbol-binding is incomplete: what about symbol-macros")
  (let ((s (gethash var-name *global-env-symbol-bindings*)))
    (cond (s (values s *global-env*))
          (t nil))))

;; {TODO} does get-symbol-binding need to return the env?
(defmethod get-symbol-binding (var-name respect-scope-rules (env environment))
  (warn "env get-symbol-binding is incomplete: what about symbol-macros")
  (let ((s (first (a-get var-name (v-symbol-bindings env)))))
    (cond (s (if respect-scope-rules
                 (values (apply-scope-rules var-name s env) env)
                 (values s env)))
          (t (get-symbol-binding var-name respect-scope-rules (v-parent-env env))))))

(defmethod v-boundp (var-name (env environment))
  (warn "v-boundp is incomplete: what about symbol-macros")
  (not (null (get-symbol-binding var-name env))))

;;-------------------------------------------------------------------------

(defmethod add-form-binding ((macro-obj v-regular-macro) (env (eql :-genv-)))
  (let ((macro-name (name macro-obj)))
    (setf (gethash macro-name *global-env-form-bindings*)
          (list macro-obj)))
  *global-env*)

(defmethod add-form-binding ((macro-obj v-regular-macro) (env environment))
  (error "IMPLEMENT ME!")
  (let ((macro-name (name macro-obj)))
    (when (shadow-global-check macro-name)
      (fresh-environment
       env :form-bindings (a-add macro-name macro-obj (v-form-bindings env))))))

;;-------------------------------------------------------------------------


(defmethod add-form-binding ((func-obj v-function) (env (eql :-genv-)))
  (let ((func-name (name func-obj)))
    (setf (gethash func-name *global-env-form-bindings*)
          (cons func-obj (gethash func-name *global-env-form-bindings*))))
  *global-env*)

(defmethod add-form-binding ((compiled-func compiled-function-result)
                         (env environment))
  (let* ((func (function-obj compiled-func))
         (func-name (name func)))
    (when (shadow-global-check func-name)
      (assert (typep func 'v-function))
      (setf (compiled-functions env func) compiled-func)
      (add-form-binding func env))))

(defmethod add-form-binding ((func-spec v-function) (env environment))
  (let ((func-name (name func-spec)))
    (when (shadow-global-check func-name)
      (fresh-environment env :form-bindings (a-add func-name func-spec (v-form-bindings env))))))

(defmethod %add-function (func-name (func-spec v-function)
                          (env base-environment))
  (when (shadow-global-check func-name)
    (setf (slot-value env 'form-bindings)
          (a-add func-name func-spec (v-form-bindings env)))))

(defmethod %get-functions-by-name (func-name (env (eql :-genv-)))
  (functions (get-func-set-by-name func-name env)))

(defmethod %get-functions-by-name (func-name (env environment))
  (append (a-get func-name (v-form-bindings env))
          (%get-functions-by-name func-name (v-parent-env env))))

(defmethod get-func-set-by-name (func-name (env (eql :-genv-)))
  (let ((funcs (sort-function-list (gethash func-name *global-env-form-bindings*))))
    (make-instance 'v-function-set :functions funcs)))

(defmethod get-func-set-by-name (func-name (env environment))
  (let ((funcs (sort-function-list
                (append
                 (loop :for func :in (%get-functions-by-name func-name env)
                    :if (and func (valid-for-contextp func env)) :collect func)
                 (get-external-function-by-name func-name env)))))
    (make-instance 'v-function-set :functions funcs)))

(defmethod special-raw-argp ((func v-function))
  (eq (v-argument-spec func) t))

(defmethod special-func-argp ((func v-function))
  (functionp (v-argument-spec func)))

(defmethod special-basic-argp ((func v-function))
  (listp (v-argument-spec func)))

(defmethod func-need-arguments-compiledp ((func v-function))
  (not (and (v-special-functionp func) (special-raw-argp func))))

(defun sort-function-list (func-list)
  (sort (copy-list func-list) #'< :key #'func-priority-score))

(defun func-priority-score (func)
  (if (v-special-functionp func)
      (cond ((special-raw-argp func) 0)
            ((special-func-argp func) 1)
            ((special-basic-argp func) 2))
      3))

(defmethod v-fboundp (func-name (env environment))
  (not (null (functions (get-func-set-by-name func-name env)))))

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

(defun %valid-for-contextp (func versions context)
  (if versions
      (when (some λ(member _ context) versions)
        func)
      func))

(defmethod add-equivalent-name (existing-name new-name)
  (warn "add-equivalent-name in incomplete: It ignores macros")
  (let ((f (get-func-set-by-name existing-name *global-env*))
        (c (get-compiler-macro existing-name *global-env*))
        (m nil;;(get-macro existing-name *global-env*)
          ))
    (cond
      ((or f c)
       (when f
         (setf (gethash new-name *global-env-form-bindings*)
               (gethash existing-name *global-env-form-bindings*)))
       (when c
         (setf (gethash new-name *global-env-compiler-macros*)
               (gethash existing-name *global-env-compiler-macros*))))
      (m (setf (gethash new-name *global-env-macros*)
               (gethash existing-name *global-env-macros*)))
      (t (error 'could-not-find-any :name existing-name))))
  new-name)

;;-------------------------------------------------------------------------

(defun wipe-global-environment ()
  (loop :for f :being :the :hash-key :of *global-env-form-bindings* :do
     (remhash f *global-env-form-bindings*))
  (loop :for f :being :the :hash-key :of *global-env-symbol-bindings* :do
     (remhash f *global-env-symbol-bindings*)))
