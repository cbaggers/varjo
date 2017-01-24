(in-package :varjo)
(in-readtable fn:fn-reader)

;;
;; Environment Public API
;;
;; In macros we have access to the environment, we should have safe ways to
;; interact with that environment.
;;

;;-------------------------------------------------------------------------
;; Symbol Bindings

(defun all-symbol-binding-names (env &key stop-at-base)
  (let ((result (v-symbol-bindings env)))
    (labels ((stop-p (e)
               (or (and stop-at-base (typep e 'base-environment))
                   (eq e *global-env*))))
      (let ((current-env env))
        (loop :until (stop-p current-env) :do
           (setf current-env (v-parent-env current-env))
           (loop :for binding :in (v-symbol-bindings current-env) :do
              (push binding result)))))
    (let* ((dedup (remove-duplicates result :key #'first))
           (accessible (remove-if-not λ(apply #'binding-accesible-p env _)
                                      dedup))
           (names (mapcar #'first accessible)))
      names)))

;;-------------------------------------------------------------------------
;; Variables

(defmethod variables-in-scope ((env environment))
  (let* ((all (all-symbol-binding-names env :stop-at-base t))
         (bindings (mapcar λ(list _ (get-symbol-binding _ t env)) all))
         (values (remove-if-not λ(typep (second _) 'v-value) bindings)))
    (mapcar #'first values)))

(defmethod variable-in-scope-p ((name symbol) (env environment))
  (not (null (find name (variables-in-scope env)))))

(defmethod variable-type ((name symbol) (env environment))
  (let* ((binding (get-symbol-binding name t env)))
    (assert (typep binding 'v-value) ()
            "The symbol ~a was not bound to a value. ~a"
            name (etypecase binding
                   (v-symbol-macro "A symbol-macro was found instead")
                   (null "Nothing was bound to that name")))
    (v-type binding)))

(defmethod variable-is-uniform-p ((name symbol) (env environment))
  (let* ((binding (get-symbol-binding name t env)))
    (assert (typep binding 'v-value) ()
            "The symbol ~a was not bound to a value. ~a"
            name (etypecase binding
                   (v-symbol-macro "A symbol-macro was found instead")
                   (null "Nothing was bound to that name")))
    (not (null (find (flow-ids binding) (v-uniforms env) :test #'id=
                     :key λ(flow-ids (second _)))))))

(defmethod variable-uniform-name ((name symbol) (env environment))
  (let* ((binding (get-symbol-binding name t env))
         (id (flow-ids binding)))
    (or (first (find id (v-uniforms env) :test #'id=
                     :key λ(flow-ids (second _))))
        (error "Varjo: The value variable ~a cannot be proved to have come from a uniform."
               name))))

;;-------------------------------------------------------------------------
;; Metadata

(defmethod metadata-for-variable ((name symbol) (metadata-key symbol)
                                  (env environment))
  (let* ((type (variable-type name env))
         (id (flow-ids type)))
    (metadata-for-flow-id metadata-key id env)))

(defmethod (setf metadata-for-variable) (value (name symbol) (env environment))
  (let* ((type (variable-type name env))
         (id (flow-ids type)))
    (setf (metadata-for-flow-id id env) value)))
