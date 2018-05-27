(in-package :varjo.internals)
(in-readtable :fn.reader)

;;============================================================
;; The mess of creation

(defmethod build-external-function ((func external-function) calling-env env)
  ;; If our external-function is in the list then we have recurred
  (assert (not (find func (ext-func-compile-chain calling-env))) ()
          'recursive-function-call-detected
          :func (format-external-func-for-error func))
  (with-slots (name in-args uniforms code glsl-versions) func
    (vbind (compiled-func maybe-def-code)
        (build-function name
                        (append in-args
                                (when uniforms `(&uniform ,@uniforms)))
                        code
                        nil
                        (env-add-ext-funct-to-chain env func)
                        :derived-from func)
      ;; Here we check that we haven't got any behaviour that, while legal for
      ;; main or local funcs, would be undesired in external functions
      (when maybe-def-code
        (assert (= (length (return-set maybe-def-code)) 0))
        ;; (assert (= (length (emit-set maybe-def-code)) 0))
        (assert (null (current-line maybe-def-code)))
        (assert (null (flow-ids maybe-def-code)))
        (assert (null (out-of-scope-args maybe-def-code)))
        (assert (null (place-tree maybe-def-code)))
        (assert (glsl-chunk-emptyp (to-block maybe-def-code)))
        (assert (emptyp (type-set maybe-def-code))))
      (values compiled-func maybe-def-code))))

(defun build-function (name args body allowed-implicit-args
                       env &key derived-from)
  ;;
  ;; Check that the args are correctly formatted, we could just let
  ;; type-spec->type take care of this, however this way we get to
  ;; give better error messages (and also impose extra limitations
  (unless (function-raw-args-validp args)
    (cond
      ((some #'&rest-p args)
       (error 'illegal-&rest-in-args
              :func-name name
              :arg-specs args))
      ((some #'&uniform-p args)
       (error 'illegal-&uniform-in-args
              :func-name name
              :arg-specs args))
      (t (error 'bad-make-function-args
                :func-name name
                :arg-specs (remove-if #'function-raw-arg-validp args)))))
  ;;
  ;; Parse the types
  (let* (;; parse the args seperately as this save us doing it twice
         (arg-types (mapcar λ(type-spec->type (second _)) args))
         (args-with-types (mapcar λ(dbind (name ig . rest) _
                                     (declare (ignore ig))
                                     `(,name ,_1 ,@rest))
                                  args
                                  arg-types)))

    (if (some λ(typep _ 'v-unrepresentable-value) arg-types)
        (make-new-function-with-unreps
         name args body derived-from env)
        (make-regular-function
         name args args-with-types body allowed-implicit-args
         derived-from env))))

(defun make-regular-function (name raw-args args body
                              allowed-implicit-args derived-from env)
  (vbind (body declarations) (extract-declares-and-doc-string body)
    (let* ((mainp (eq name :main))
           (func-env (make-func-env env mainp allowed-implicit-args))
           (in-arg-flow-ids (mapcar (lambda (_)
                                      (declare (ignore _))
                                      (flow-id!))
                                    args))
           (arg-glsl-names (loop :for (name) :in args :collect
                              (lisp-name->glsl-name name env)))
           (body-env (add-declarations-to-env
                      declarations
                      (reduce
                       (lambda (func-env tripple)
                         (dbind (arg glsl-name flow-ids) tripple
                           (dbind (arg-name type . invalid) arg
                             (assert (not invalid) ()
                                     'invalid-function-arg-format
                                     :arg `(,arg-name
                                            ,(type->type-spec type)
                                            ,@invalid)
                                     :name name)
                             (add-symbol-binding
                              arg-name
                              (v-make-value
                               (set-flow-id type flow-ids)
                               func-env
                               :glsl-name glsl-name)
                              func-env))))
                       (mapcar #'list args arg-glsl-names in-arg-flow-ids)
                       :initial-value (if mainp
                                          func-env
                                          (remove-main-method-flag-from-env
                                           func-env)))))
           (body-code `(return (progn ,@body)))
           (body-obj (compile-form body-code body-env))
           (implicit-args (extract-implicit-args name allowed-implicit-args
                                                 (normalize-out-of-scope-args
                                                  (out-of-scope-args body-obj))
                                                 func-env))
           (glsl-name (if mainp "main" (lisp-name->glsl-name name func-env)))
           (return-set (coerce (return-set body-obj) 'list)))
      (when (and (typep (stage env) 'compute-stage) mainp)
        (assert (null return-set) () 'compute-stage-must-be-void
                :returns return-set))
      (let* ((emit-set (emit-set body-obj))
             (multi-return-vars (when return-set (rest return-set)))
             (type (if mainp
                       (type-spec->type 'v-void)
                       (or (when return-set (first return-set))
                           (type-spec->type :void))))
             ;;
             (arg-pairs (unless mainp
                          (loop :for (nil type) :in args
                             :for name :in arg-glsl-names :collect
                             `(,(v-glsl-string type) ,name))))
             (out-arg-pairs (unless mainp
                              (loop :for mval :in multi-return-vars
                                 :for i :from 1
                                 :for name = (postfix-glsl-index
                                              *return-var-name-base* i)
                                 :for type = (v-glsl-string mval)
                                 :collect `(,type ,name))))
             (in-out-args
              ;; {TODO} handle multiple returns
              (when (and (typep type 'v-function-type)
                         (ctv type)
                         (implicit-args (ctv type)))
                (let ((closure (ctv type)))
                  (append (in-out-args closure)
                          (implicit-args closure)))))
             (return-for-glsl (if (ephemeral-p type)
                                  (type-spec->type :void)
                                  type))
             (strip-glsl
              (and (not mainp)
                   (pure-p body-obj)
                   (or (v-voidp type) (ephemeral-p type))
                   (null multi-return-vars)))
             (sigs (unless (or mainp strip-glsl)
                     (list (gen-function-signature glsl-name arg-pairs
                                                   out-arg-pairs
                                                   return-for-glsl
                                                   implicit-args
                                                   in-out-args))))
             (func-glsl-def (unless strip-glsl
                              (glsl-chunk-to-string
                               (gen-function-body-string
                                 glsl-name arg-pairs
                                 out-arg-pairs
                                 return-for-glsl body-obj
                                 implicit-args in-out-args))))
             (arg-types (mapcar #'second args))
             ;;
             ;; This is an interesting case, it is possible for the user to write a function
             ;; where they always discard. Whilst strage I don't think is illegal so I dont
             ;; want to throw an error here. We have to make a valid function though so it
             ;; pretty much has to be a void.
             (ret-set (or (return-set body-obj)
                          (make-type-set)))
             (func (make-user-function-obj name
                                           (unless strip-glsl
                                             (gen-function-transform
                                              glsl-name args
                                              multi-return-vars
                                              implicit-args))
                                           nil ;;{TODO} should be context
                                           arg-types
                                           (or (return-set body-obj)
                                               (make-type-set))
                                           :code (list raw-args body)
                                           :glsl-name glsl-name
                                           :implicit-args implicit-args
                                           :in-out-args in-out-args
                                           :flow-ids (unless (emptyp (return-set body-obj))
                                                       (flow-ids (elt (return-set body-obj) 0)))
                                           :in-arg-flow-ids in-arg-flow-ids
                                           :pure (pure-p body-obj)
                                           :derived-from derived-from))
             (tl-meta (hash-table-values (slot-value body-env 'local-metadata)))
             (code-obj (copy-compiled body-obj
                                      :type-set (make-type-set)
                                      :current-line nil
                                      :to-block nil
                                      :return-set nil
                                      :emit-set emit-set
                                      :place-tree nil
                                      :out-of-scope-args implicit-args
                                      :used-types nil))
             (inline-candidate (and (not implicit-args)
                                    (not strip-glsl)
                                    (not multi-return-vars)
                                    (= (length emit-set) 0)
                                    (or (null (to-block body-obj))
                                        (= (length (glsl-chunk-lines (to-block body-obj)))
                                           0)))))
        (let ((res (make-instance 'compiled-function-result
                                  :call-count (if mainp 1 0)
                                  :function-obj func
                                  :signatures sigs
                                  :used-types (append (used-types body-obj)
                                                      (coerce ret-set 'list)
                                                      (coerce emit-set 'list))
                                  :glsl-code func-glsl-def
                                  :stemcells (stemcells code-obj)
                                  :return-set ret-set
                                  :emit-set emit-set
                                  :top-level-scoped-metadata tl-meta
                                  :inline-candidate inline-candidate)))
          (setf (compiled-result func) res)
          (values res code-obj))))))

(defun make-new-function-with-unreps (name args body derived-from env)
  (assert (not (eq name :main)))
  (let* ((all-vars (env-binding-names env :stop-at-base t
                                      :variables-only t))
         (visible-vars (remove-if-not λ(get-symbol-binding _ t env)
                                      all-vars))
         (visible-var-pairs (mapcar λ(capture-var _ env) visible-vars))
         (arg-types (mapcar (lambda (x) (type-spec->type (second x))) args))
         (func (make-user-function-obj name nil nil arg-types #()
                                       :code (list args body)
                                       :captured-vars visible-var-pairs
                                       :derived-from derived-from)))
    (values (make-instance 'compiled-function-result
                           :function-obj func
                           :signatures nil
                           :used-types nil
                           :glsl-code nil
                           :stemcells nil
                           :return-set nil
                           :emit-set nil)
            (make-compiled :type-set (make-type-set)
                           :current-line nil
                           :place-tree nil))))

(defun capture-var (name env)
  (let ((val (get-symbol-binding name t env)))
    (assert (typep val 'v-value))
    (assert env ()
            "why ~a ~a" name env)
    (make-instance 'captured-var
                   :name name
                   :value val
                   :origin-env env)))

(defun function-raw-args-validp (raw-args)
  (every #'function-raw-arg-validp raw-args))

(defun function-raw-arg-validp (raw-arg)
  "Basic checks to validate the argument forms for the function"
  (and (listp raw-arg)
       (>= (length raw-arg) 2)
       (not (null (first raw-arg)))
       (symbolp (first raw-arg))
       (not (keywordp (first raw-arg)))
       (let ((type (try-type-spec->type
                    (resolve-name-from-alternative (second raw-arg))
                    nil)))
         (and type
              (if (typep type 'v-array)
                  (let ((dim (first (v-dimensions type))))
                    (not (equal dim '*)))
                  t)))))

(defun extract-implicit-args (name allowed-implicit-args
                              normalized-out-of-scope-args env)
  (let ((result (remove-if λ(= (v-function-scope _)
                               (v-function-scope env))
                           normalized-out-of-scope-args)))
    (if (or (eq allowed-implicit-args t)
            (and (listp allowed-implicit-args)
                 (every λ(member _ allowed-implicit-args)
                        allowed-implicit-args)))
        result
        (when result
          (error 'illegal-implicit-args :func-name name)))))

(defun make-func-env (env mainp allowed-implicit-args)
  (if mainp
      (fresh-environment env :function-scope (1+ (v-function-scope env))
                         :context (cons :main (v-context env))
                         :allowed-outer-vars allowed-implicit-args)
      (fresh-environment env :function-scope (1+ (v-function-scope env))
                         :allowed-outer-vars allowed-implicit-args)))
