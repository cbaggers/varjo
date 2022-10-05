(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------

(defun make-stage (kind in-args uniforms context code
                   &optional (stemcells-allowed t) primitive)
  (assert (listp context) ()
          "Varjo: The context argument to make-stage must be a list")
  (multiple-value-bind (version/s target-env extensions includes)
      (destructure-context context)
    (create-stage kind version/s
                  :input-variables in-args
                  :uniform-variables uniforms
                  :code code
                  :stemcells-allowed stemcells-allowed
                  :primitive primitive
                  :target-environment target-env
                  :extensions extensions
                  ;; actually includes don't make a lot of sense in Varjo, but who knows...
                  :includes includes)))

(defun create-stage (kind version/s
                     &key
                       input-variables
                       uniform-variables
                       shared-variables
                       code
                       (stemcells-allowed t)
                       primitive
                       (target-environment :opengl)
                       includes
                       extensions)
  (when (and (every #'check-arg-form input-variables)
             (every (lambda (arg)
                      (check-arg-form arg
                                      :uniformp t
                                      :target-environment target-environment))
                    uniform-variables)
             (check-for-dups input-variables
                             uniform-variables
                             shared-variables))
    (labels ((early-qualifier-checks (name vkind whole qualifiers)
               (let* ((is-ubo (find :ubo qualifiers))
                      (is-ssbo (find :ssbo qualifiers))
                      (layouts (intersection qualifiers '(:std-140
                                                          :std-430
                                                          :shared
                                                          :packed)))
                      (layout (first layouts)))
                 (assert (not (and is-ubo is-ssbo))
                         () 'uniform-ubo-and-ssbo :arg whole)
                 (assert (not (and (eq vkind 'input-variable)
                                   (or is-ubo is-ssbo)))
                         () "Varjo: input-variable cannot be a ubo or ssbo: ~a"
                         whole)
                 (assert (< (length layouts) 2) ()
                         "Varjo: Multiple layout qualifiers found for parameter '~a'~%Qualifiers: ~{~s~^, ~}"
                         name
                         layouts)
                 (assert (not (and is-ubo (eq layout :std-430))) ()
                         "Varjo: UBOs cannot have a layout of :std-430")
                 (if (and (or is-ubo is-ssbo) (null layout))
                     (cons :std-140 qualifiers)
                     qualifiers)))
             (make-var (vkind raw)
               (dbind (name type-spec . rest) raw
                 (vbind (qualifiers glsl-name) (extract-glsl-name rest)
                   (let ((qualifiers
                          (mapcar #'parse-qualifier
                                  (early-qualifier-checks name
                                                          vkind
                                                          raw
                                                          qualifiers))))
                     (make-instance
                      vkind
                      :name name
                      :glsl-name (or glsl-name (safe-glsl-name-string name))
                      :type (qualify-type (type-spec->type type-spec)
                                          qualifiers)))))))
      (let* ((version/s (process-context (ensure-list version/s)))
             (stage-type (if kind
                             (stage-kind-to-type kind)
                             'stage))
             (r (make-instance
                 stage-type
                 :input-variables (mapcar λ(make-var 'input-variable _)
                                          input-variables)
                 :uniform-variables (mapcar λ(make-var 'uniform-variable _)
                                            uniform-variables)
                 :shared-variables (mapcar λ(make-var 'shared-variable _)
                                            shared-variables)
                 :context version/s
                 :target-environment target-environment
                 :extensions (mapcar #'make-extension extensions)
                 :lisp-code code
                 :stemcells-allowed stemcells-allowed
                 :primitive-in (%process-primitive-type stage-type
                                                        primitive))))
        (when (eq target-environment :vulkan)
          ;; {TODO} proper error
          (assert (intersection version/s '(:440 :450 :460)) ()
                  "Varjo: Target environment Vulkan requires a GLSL version of at least 440."))
        (when (equal kind :geometry)
          ;; {TODO} proper error
          (assert (intersection version/s '(:150 :330 :400 :410 :420 :430 :440 :450 :460)) ()
                  "Varjo: Geometry stages require a GLSL version of at least 150"))
        (when (member kind '(:tessellation-control :tessellation-evaluation))
          ;; {TODO} proper error
          (assert (intersection version/s '(:400 :410 :420 :430 :440 :450 :460)) ()
                  "Varjo: Tessellation stages require a GLSL version of at least 400"))
        (when (member kind '(:compute))
          ;; {TODO} proper error
          (assert (intersection version/s '(:430 :440 :450 :460)) ()
                  "Varjo: Compute stages require a GLSL version of at least 430"))
        (check-for-stage-specific-limitations r)
        r))))


;;----------------------------------------------------------------------

(defun %process-primitive-type (stage-type primitive &key (allow-null t))
  (assert (find stage-type *stage-type-names*))
  (let ((primitive
         (etypecase primitive
           (null nil)
           (primitive primitive)
           ((or symbol list) (primitive-name-to-instance primitive)))))
    (unless (and allow-null (null primitive))
      (ecase stage-type
        (vertex-stage primitive)

        (tessellation-control-stage
         (assert (typep primitive 'patches) ()
                 'invalid-primitive-for-tessellation-stage
                 :prim (type-of primitive))
         primitive)

        (tessellation-evaluation-stage
         (assert (typep primitive 'patches) ()
                 'invalid-primitive-for-tessellation-stage
                 :prim (type-of primitive))
         primitive)

        (geometry-stage
         (assert (typep primitive 'geometry-primitive) ()
                 'invalid-primitive-for-geometry-stage
                 :prim (type-of primitive))
         primitive)

        (fragment-stage nil)

        (compute-stage
         (assert (null primitive) ()
                 'invalid-primitive-for-compute-stage
                 :prim (type-of primitive))
         nil)))))


;;----------------------------------------------------------------------

(defgeneric copy-stage (stage &key)
  (:method ((stage stage)
            &key (input-variables nil iv-set)
              (uniform-variables nil uv-set)
              (shared-variables nil sv-set)
              (context nil c-set)
              (target-environment nil env-set)
              (extensions nil ext-set)
              (lisp-code nil lc-set)
              (previous-stage nil ps-set)
              (stemcells-allowed nil sa-set)
              (primitive-in nil p-set)
              stage-type)
    (make-instance
     (or stage-type (type-of stage))
     :input-variables (if iv-set
                          input-variables
                          (input-variables stage))
     :uniform-variables (if uv-set
                            uniform-variables
                            (uniform-variables stage))
     :shared-variables (if sv-set
                           shared-variables
                           (shared-variables stage))
     :context (if c-set
                  context
                  (context stage))
     :target-environment (if env-set
                             target-environment
                             (target-environment stage))
     :extensions (if ext-set
                     extensions
                     (extensions stage))
     :lisp-code (if lc-set
                    lisp-code
                    (lisp-code stage))
     :previous-stage (if ps-set
                         previous-stage
                         (previous-stage stage))
     :stemcells-allowed (if sa-set
                            stemcells-allowed
                            (stemcells-allowed stage))
     :primitive-in (if p-set
                       primitive-in
                       (primitive-in stage)))))

;;------------------------------------------------------------

(defun destructure-context (context)
  (flet ((check-for-key (ctx key)
           (and (listp ctx)
                (>= (length ctx) 2)
                (eq (first ctx) key))))
    (let ((target-env :opengl)
          (extensions nil)
          (includes nil))
      (loop for ctx in context
            if (keywordp ctx) collect ctx into version/s
            if (check-for-key ctx :target-environment) do (setf target-env (second ctx))
            if (check-for-key ctx :extensions) do (setf extensions (cdr ctx))
            if (check-for-key ctx :includes) do (setf includes (cdr ctx))
            finally (return (values version/s
                                    target-env
                                    extensions
                                    includes))))))

(defun extract-glsl-name (qual-and-maybe-name)
  (let ((glsl-name (last1 qual-and-maybe-name)))
    (if (stringp glsl-name)
        (values (butlast qual-and-maybe-name)
                glsl-name)
        qual-and-maybe-name)))

(defun stage-obj-to-name (stage)
  (warn "stage-obj-to-name is deprecated, please use stage-kind instead")
  (stage-kind stage))

(defun stage-kind (stage)
  (etypecase stage
    (vertex-stage :vertex)
    (tessellation-control-stage :tessellation-control)
    (tessellation-evaluation-stage :tessellation-evaluation)
    (geometry-stage :geometry)
    (fragment-stage :fragment)
    (compute-stage :compute)))

(defun stage-kind-to-type (kind)
  (let ((map '((:vertex . vertex-stage)
               (:tessellation-control . tessellation-control-stage)
               (:tessellation-evaluation . tessellation-evaluation-stage)
               (:geometry . geometry-stage)
               (:fragment . fragment-stage)
               (:compute . compute-stage))))
    (or (assocr kind map)
        (when (subtypep kind 'stage) kind)
        (error 'invalid-stage-kind :kind kind))))

(defun compiled-stage-type-for (stage)
  (etypecase stage
    (vertex-stage 'compiled-vertex-stage)
    (tessellation-control-stage 'compiled-tessellation-control-stage)
    (tessellation-evaluation-stage 'compiled-tessellation-evaluation-stage)
    (geometry-stage 'compiled-geometry-stage)
    (fragment-stage 'compiled-fragment-stage)
    (compute-stage 'compiled-compute-stage)))

(defun process-context (raw-context)
  ;; As this was a more recent change we wanted a more explanatory error
  (assert (null (intersection raw-context *stage-names*))
          () 'stage-in-context
          :context (set-difference raw-context *stage-names*))
  (assert (null (find #'valid-primitive-name-p raw-context))
          () 'primitive-in-context
          :context raw-context)
  ;; ensure there is a version
  (labels ((valid (x) (find x *supported-versions*)))
    (assert (every #'valid raw-context) () 'invalid-context-symbols
            :symbols (remove-if #'valid raw-context)))
  ;;
  (if (intersection raw-context *supported-versions*)
      raw-context
      (list *default-version*)))

;;{TODO} proper error
(defun check-arg-form (arg
                       &key
                         uniformp
                         (target-environment :opengl))
  (unless
      (and
       ;; needs to at least have name and type
       (>= (length arg) 2)
       ;;
       (not (constantp (first arg)))
       ;; of the rest of the list it must be keyword qualifiers and optionally
       ;; a string at the end. The string is a declaration of what the name of
       ;; the var will be in glsl. This feature is intended for use only by the
       ;; compiler but I see not reason to lock this away.
       (let* ((qualifiers (subseq arg 2))
              (qualifiers (if (stringp (last1 qualifiers))
                              (butlast qualifiers)
                              qualifiers)))
         (loop :for qualifier-form :in qualifiers
               :for qualifier := (first (listify qualifier-form))
               :do (assert (keywordp qualifier) ()
                           "Varjo: Qualifier ~a within declaration ~a is badly formed.~%Should be (-var-name- -var-type- &optional qualifiers) and ~%the arg name may not have the same name as a constant."
                           qualifier arg)
               :when (find qualifier *glsl-vulkan-qualifiers* :key #'first)
               :do (assert (eq :vulkan target-environment) ()
                           "Varjo: Qualifier ~a within declaration ~a is only allowed in target environment :vulkan, but not in ~a"
                           qualifier arg target-environment)
               :when (eq qualifier :input-attachment-index)
               :do (assert (member (second arg) '(:subpass-input
                                                  :subpass-input-ms))
                           ()
                           "Varjo: Qualifier ~a within declaration ~a is only allowed for subpass inputs."
                           qualifier arg target-environment))
         t))
    (error "Declaration ~a is badly formed.~%Should be (-var-name- -var-type- &optional qualifiers) and ~%the arg name may not have the same name as a constant." arg))
  t)

;;{TODO} proper error
(defun check-for-dups (in-args uniforms shared)
  (if (intersection (intersection (mapcar #'first in-args)
                                  (mapcar #'first uniforms))
                    (mapcar #'first shared))
      (error "Varjo: Duplicates names found between in-args and uniforms")
      t))

;;{TODO} proper error
(defun check-for-stage-specific-limitations (stage)
  (assert (not (and (typep stage 'vertex-stage)
                    (some λ(qualifiers (v-type-of _))
                          (input-variables stage))))
          () "In args to vertex shaders can not have qualifiers"))

;;------------------------------------------------------------

(defgeneric stage-where-first-return-is-position-p (stage)
  (:method ((stage stage))
    (or (typep stage 'vertex-stage)
        (typep stage 'tessellation-evaluation-stage))))
