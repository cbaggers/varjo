(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------

(defun make-stage (kind in-args uniforms context code
                   &optional (stemcells-allowed t) primitive)
  (when (and (every #'check-arg-form in-args)
             (every #'check-arg-form uniforms)
             (check-for-dups in-args uniforms))
    (labels ((make-var (vkind raw)
               (dbind (name type-spec . rest) raw
                 (vbind (qualifiers glsl-name) (extract-glsl-name rest)
                   (make-instance
                    vkind
                    :name name
                    :glsl-name (or glsl-name (safe-glsl-name-string name))
                    :type (type-spec->type type-spec)
                    :qualifiers qualifiers)))))
      (let* ((context (process-context context))
             (stage-type (if kind
                             (stage-kind-to-type kind)
                             'stage))
             (r (make-instance
                 stage-type
                 :input-variables (mapcar λ(make-var 'input-variable _)
                                          in-args)
                 :uniform-variables (mapcar λ(make-var 'uniform-variable _)
                                            uniforms)
                 :context context
                 :lisp-code code
                 :stemcells-allowed stemcells-allowed
                 :primitive-in (%process-primitive-type stage-type
                                                        primitive))))
        (when (member kind '(:tessellation-control :tessellation-evaluation))
          ;; {TODO} proper error
          (assert (intersection context '(:400 :410 :420 :430 :440 :450)) ()
                  "Varjo: Tessellation stages require a GLSL version of at least 400"))
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

        (fragment-stage nil)))))


;;----------------------------------------------------------------------

(defgeneric copy-stage (stage &key)
  (:method ((stage stage)
            &key (input-variables nil iv-set)
              (uniform-variables nil uv-set)
              (context nil c-set)
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
     :context (if c-set
                  context
                  (context stage))
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

(defun extract-glsl-name (qual-and-maybe-name)
  (let ((glsl-name (last1 qual-and-maybe-name)))
    (if (stringp glsl-name)
        (values (butlast qual-and-maybe-name)
                glsl-name)
        qual-and-maybe-name)))

(defun stage-obj-to-name (stage)
  (etypecase stage
    (vertex-stage :vertex)
    (tessellation-control-stage :tessellation-control)
    (tessellation-evaluation-stage :tessellation-evaluation)
    (geometry-stage :geometry)
    (fragment-stage :fragment)))

(defun stage-kind-to-type (kind)
  (let ((map '((:vertex . vertex-stage)
               (:tessellation-control . tessellation-control-stage)
               (:tessellation-evaluation . tessellation-evaluation-stage)
               (:geometry . geometry-stage)
               (:fragment . fragment-stage))))
    (or (assocr kind map)
        (when (subtypep kind 'stage) kind)
        (error 'invalid-stage-kind :kind kind))))

(defun compiled-stage-type-for (stage)
  (etypecase stage
    (vertex-stage 'compiled-vertex-stage)
    (tessellation-control-stage 'compiled-tessellation-control-stage)
    (tessellation-evaluation-stage 'compiled-tessellation-evaluation-stage)
    (geometry-stage 'compiled-geometry-stage)
    (fragment-stage 'compiled-fragment-stage)))

(defun process-context (raw-context)
  ;; As this was a more recent change we wanted a more explanatory error
  (assert (null (remove-if-not λ(find _ *stage-names*) raw-context))
          () 'stage-in-context :context raw-context)
  ;; ensure there is a version
  (labels ((valid (x) (find x *valid-contents-symbols*)))
    (assert (every #'valid raw-context) () 'invalid-context-symbols
            :symbols (remove #'valid raw-context )))
  ;;
  (if (intersection raw-context *supported-versions*)
      raw-context
      (cons *default-version* raw-context)))

;;{TODO} proper error
(defun check-arg-form (arg)
  (unless
      (and
       ;; needs to at least have name and type
       (>= (length arg) 2)
       ;; of the rest of the list it must be keyword qualifiers and optionally
       ;; a string at the end. The string is a declaration of what the name of
       ;; the var will be in glsl. This feature is intended for use only by the
       ;; compiler but I see not reason to lock this away.
       (let* ((qualifiers (subseq arg 2))
              (qualifiers (if (stringp (last1 qualifiers))
                              (butlast qualifiers)
                              qualifiers)))
         (every #'keywordp qualifiers)))
    (error "Declaration ~a is badly formed.~%Should be (-var-name- -var-type- &optional qualifiers)" arg))
  t)

;;{TODO} proper error
(defun check-for-dups (in-args uniforms)
  (if (intersection (mapcar #'first in-args) (mapcar #'first uniforms))
      (error "Varjo: Duplicates names found between in-args and uniforms")
      t))

;;{TODO} proper error
(defun check-for-stage-specific-limitations (stage)
  (assert (not (and (typep stage 'vertex-stage)
                    (some #'qualifiers (input-variables stage))))
          () "In args to vertex shaders can not have qualifiers"))

;;------------------------------------------------------------

(defgeneric stage-where-first-return-is-position-p (stage)
  (:method ((stage stage))
    (or (typep stage 'vertex-stage)
        (typep stage 'tessellation-evaluation-stage))))
