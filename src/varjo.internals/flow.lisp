(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;; these are created whenever a new value is, they flow through the code-objects
;; allowing us to detect everywhere a value flows through a program.
;; There are two complications, multiple value return and conditionals
;; - Multiple value return is handled by having a list of flow-identifiers
;; - conditional handled by each flow-identifier contain a list of identifiers
;;   itself. This is the collection of all the values that could flow through
;;   this point.

;;----------------------------------------------------------------------
;; internals

(defun flow-id-p (id)
  (typep id 'flow-identifier))

(defmethod raw-ids ((flow-id flow-identifier))
  (mapcar λ(slot-value _ 'val) (ids flow-id)))


(defun bare-id! (val &key (return-pos 0))
  (assert (typep val 'number))
  (make-instance 'bare-flow-id :val val :return-pos return-pos))

(defmethod print-object ((o flow-identifier) stream)
  (let* ((ids (mapcar λ(slot-value _ 'val) (listify (ids o))))
         (ids (if (> (length ids) 6)
                  (append (subseq ids 0 6) '(:etc))
                  ids)))
    (format stream "#<FLOW-ID ~{~s~^ ~}>" ids)))

(let ((gl-flow-id 0))
  (defun %gen-flow-gl-id () (bare-id! (decf gl-flow-id))))

(defun singular-flow-id-p (flow-id)
  (= 1 (length (ids flow-id))))

;;----------------------------------------------------------------------

(defvar root-flow-gen-func
  (lambda ()
    (error "Trying to generate flow-id outside of a flow-id-scope")))

(defvar flow-gen-func root-flow-gen-func)

(defun %make-flow-id-source-func (from)
  (let ((%flow-id (typecase from
                    (function (funcall from :dump))
                    (integer from)
                    (otherwise (error "invalid 'from'")))))
    (lambda (&optional x)
      (if (eq x :dump)
          %flow-id
          (bare-id! (incf %flow-id))))))

(defstruct flow-id-checkpoint func)

(defmethod print-object ((obj flow-id-checkpoint) stream)
  (format stream "#<flow-id-checkpoint ~s>"
          (funcall (flow-id-checkpoint-func obj) :dump)))

;;----------------------------------------------------------------------
;; scoping

(defmacro flow-id-scope (&body body)
  `(let ((flow-gen-func
          (if (eq flow-gen-func root-flow-gen-func)
              (%make-flow-id-source-func -1)
              flow-gen-func)))
     ,@body))

(defun checkpoint-flow-ids ()
  (prog1 (make-flow-id-checkpoint :func flow-gen-func)
    (setf flow-gen-func (%make-flow-id-source-func flow-gen-func))))

(defun reset-flow-ids-to-checkpoint (checkpoint)
  (setf flow-gen-func (%make-flow-id-source-func
                       (flow-id-checkpoint-func checkpoint)))
  t)

;;----------------------------------------------------------------------
;; construction

(defun m-flow-id! (flow-ids)
  (error "titsicles ~a" flow-ids))

(defun flow-id! (&rest ids)
  (labels ((key (_) (slot-value _ 'val))
           (extract-id (thing)
             (etypecase thing
               (null nil)
               (flow-identifier thing)
               (compiled (flow-ids thing))
               (v-type (flow-ids thing)))))
    (let ((ids (remove nil (mapcar #'extract-id ids))))
      (if (null ids)
          (make-instance 'flow-identifier :ids (list (funcall flow-gen-func)))
          (make-instance 'flow-identifier
                         :ids (sort (copy-list (remove-duplicates
                                                (mappend #'ids ids)
                                                :key #'key))
                                    #'< :key #'key))))))

(defun flow-id+meta! (&key return-pos)
  (let ((r (flow-id!)))
    (setf (slot-value (nth-or-self 0 (ids r)) 'return-pos)
          return-pos)
    r))

(defun %gl-flow-id! ()
  (make-instance 'flow-identifier :ids (list (%gen-flow-gl-id))))

(defun type-doesnt-need-flow-id (type)
  (typep type 'v-error))

(defun function-return-spec-doesnt-need-flow-ids (spec)
  (assert (or (functionp spec)
              (typep spec 'return-type-generator)
              (<= (length spec) 1)))
  (and (vectorp spec)
       (or (= (length spec) 0)
           (typep (elt spec 0) 'v-error))))

(defun set-doesnt-need-flow-ids (set)
  (or (= (length set) 0) (typep (elt set 0) 'v-error)))


;;----------------------------------------------------------------------
;; inspection

(defun id~= (id-a id-b)
  (declare (type (or null flow-identifier) id-a id-b)
           (optimize (speed 3) (safety 1) (debug 1)))
  (unless (or (null id-a) (null id-b))
    (not (null (intersection (listify (ids id-a)) (listify (ids id-b))
                             :key λ(slot-value _ 'val))))))

(defun id= (id-a id-b)
  (declare (type (or null flow-identifier) id-a id-b)
           (optimize (speed 3) (safety 1) (debug 1)))
  (unless (or (null id-a) (null id-b))
    (equal (sort (copy-list (raw-ids id-a)) #'<)
           (sort (copy-list (raw-ids id-b)) #'<))))

(defun assert-flow-id-singularity (flow-id)
  (assert (or (null flow-id)
              (not (listp flow-id)))))


;;----------------------------------------------------------------------

(defgeneric replace-flow-id (type flow-id)
  (:method ((type v-type) (flow-id flow-identifier))
    (assert (flow-ids type) (type)
            "Varjo: Tried to replace the flow id for ~a but it didnt have any"
            type)
    (let ((new-type (copy-type type)))
      (setf (slot-value new-type 'flow-ids) flow-id)
      new-type)))

(defgeneric set-flow-id (type flow-id)
  (:method ((type v-type) (flow-id flow-identifier))
    (assert (null (flow-ids type)) (type)
            "Varjo: Tried to set the flow id for ~a but it already had one"
            type)
    (let ((new-type (copy-type type)))
      (setf (slot-value new-type 'flow-ids) flow-id)
      new-type)))


(defun replace-flow-ids-for-single-var (var-name
                                        new-flow-ids
                                        env)
  ;; for a wip version of this handling multiple vals see:
  ;; protocode/replace-flow-id-for-multiple-values.lisp
  (vbind (current-value env-holding-var) (get-symbol-binding var-name nil env)
    (replace-flow-ids-for-specific-value var-name
                                         current-value
                                         env-holding-var
                                         new-flow-ids
                                         env)))

(defun replace-flow-ids-for-specific-value (var-name
                                            current-value
                                            env-holding-var
                                            new-flow-ids
                                            env)
  ;;
  ;; Care must be taken when using this function as it is your job
  ;; to ensure the var-name, value & environments are correctly related.
  ;;
  ;; Where possible use the replace-flow-ids-for-single-var function
  ;; instead.
  ;;
  (if (typep env-holding-var 'base-environment)
      ;; We dont replace things in the base environment
      env
      (let (;; child-envs-of-env-holding-var is the list of environments
            ;; that are children of env-holding-var (in order of nearest
            ;; decendant to furthest)
            (child-envs-of-env-holding-var nil)
            ;; cur-env is the current environment we are visiting
            (cur-env env))
        ;; Walking up the tree until we find env-holding-var, this loop
        ;; pushes all children of env-holding-var onto child-envs-of-env-holding-var
        (loop
           :until (eq cur-env env-holding-var)
           :do
           (let ((parent (v-parent-env cur-env)))
             (push cur-env child-envs-of-env-holding-var)
             (setf cur-env parent)))
        (let (;; here we perform the flow-id surgery on env-holding-var
              (new-parent
               (env-replace-symbol-bindings
                env-holding-var
                (a-add var-name
                       (copy-value current-value
                                   :type (replace-flow-id
                                          (v-type-of current-value)
                                          new-flow-ids))
                       (copy-list (v-symbol-bindings env-holding-var))))))
          (loop :for env :in child-envs-of-env-holding-var :do
             (setf new-parent
                   (env-replace-parent env new-parent)))
          new-parent))))

;;----------------------------------------------------------------------
;; Helpers

(defmethod flow-ids ((obj compiled))
  (flow-ids (primary-type obj)))

(defmethod flow-ids ((obj v-value))
  (flow-ids (v-type-of obj)))

(defgeneric strip-flow-id (obj)
  ;; {TODO} need to address this one
  (:method ((obj v-type))
    (type-spec->type (type->type-spec obj))))

;;----------------------------------------------------------------------
