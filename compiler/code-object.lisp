(in-package :varjo)

(defun code! (&key (type nil set-type) (current-line "") signatures to-block
                return-set used-types multi-vals stemcells
                out-of-scope-args mutations
                place-tree node-tree)
  (let ((flow-ids (flow-ids type)))
    (assert-flow-id-singularity flow-ids)
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :code-object
             :code-type (type->type-spec type))))
  (unless set-type
    (error "Type must be specified when creating an instance of varjo:code"))
  (unless (or (eq node-tree :ignored)
              (and (typep node-tree 'ast-node)
                   (listp (slot-value node-tree 'args))))
    (error "invalid ast node-tree ~s" node-tree))
  (assert (typep type 'v-type))
  (let* ((used-types (if (and (not (find type used-types :test #'v-type-eq))
                              (not (v-type-eq type (gen-none-type))))
                         (cons type used-types)
                         used-types)))
    (make-instance 'code
                   :type type
                   :current-line current-line
                   :signatures signatures
                   :to-block to-block
                   :return-set return-set
                   :used-types used-types
                   :multi-vals multi-vals
                   :stemcells stemcells
                   :out-of-scope-args out-of-scope-args
                   :place-tree place-tree
                   :mutations mutations
                   :node-tree node-tree)))

(defmethod v-type-of ((obj code))
  (code-type obj))

(defmethod current-line (code-obj &optional even-when-ephemeral)
  (unless (and (ephemeral-p (code-type code-obj)) (not even-when-ephemeral))
    (slot-value code-obj 'current-line)))

(defun add-higher-scope-val (code-obj value)
  (let* ((type (v-type-of value))
         (new-oos-args
          (if (ephemeral-p type)
              (out-of-scope-args code-obj)
              (cons value (out-of-scope-args code-obj)))))
    (copy-code code-obj :out-of-scope-args new-oos-args)))

(defun normalize-out-of-scope-args (args)
  (remove-duplicates args :test #'v-value-equal))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod v-code-type-eq ((a code) (b code) &optional (env *global-env*))
  (v-type-eq (code-type a) (code-type b) env))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun make-code-obj (type current-line &key place-tree node-tree)
  (code! :type type :current-line current-line
         :place-tree (listify place-tree)
         :node-tree node-tree))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod copy-code ((code-obj code)
                      &key (type nil set-type)
                        (current-line nil set-current-line)
                        (signatures nil set-sigs)
                        (to-block nil set-block)
                        (return-set nil set-return-set)
                        (multi-vals nil set-multi-vals)
                        (stemcells nil set-stemcells)
                        (out-of-scope-args nil set-out-of-scope-args)
                        (place-tree nil set-place-tree)
                        (mutations nil set-mutations)
                        (node-tree nil set-node-tree))
  (let* ((type (if set-type type (code-type code-obj))))
    (code! :type type
           :current-line (if set-current-line current-line
                             (current-line code-obj t))
           :signatures (if set-sigs signatures (signatures code-obj))
           :to-block (if set-block to-block (remove nil (to-block code-obj)))
           :return-set (if set-return-set return-set (return-set code-obj))
           :used-types (used-types code-obj)
           :multi-vals (if set-multi-vals multi-vals (multi-vals code-obj))
           :stemcells (if set-stemcells stemcells (stemcells code-obj))
           :out-of-scope-args (if set-out-of-scope-args
                                  out-of-scope-args
                                  (remove nil (out-of-scope-args code-obj)))
           :place-tree (if set-place-tree place-tree (place-tree code-obj))
           :mutations (if set-mutations mutations (mutations code-obj))
           :node-tree (if set-node-tree node-tree (node-tree code-obj)))))

(defmethod merge-obs ((objs list)
                      &key type
                        current-line
                        (signatures nil set-sigs)
                        (to-block nil set-block)
                        (return-set nil set-return-set)
                        multi-vals
                        (stemcells nil set-stemcells)
                        (out-of-scope-args nil set-out-of-scope-args)
                        place-tree
                        (mutations nil set-mutations)
                        node-tree)
  (assert type () "type is mandatory")
  (assert (typep type 'v-type))
  (let ((flow-ids (flow-ids type))
        (return-set
         (if set-return-set
             return-set
             (merge-return-sets (remove nil (mapcar #'return-set objs))))))
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :code-object
             :code-type type))
    (code! :type type
           :current-line current-line
           :signatures (if set-sigs signatures
                           (mapcat #'signatures objs))
           :to-block (if set-block to-block
                         (mapcat #'to-block objs))
           :return-set return-set
           :used-types (mapcat #'used-types objs)
           :multi-vals multi-vals
           :stemcells (if set-stemcells stemcells
                          (mapcat #'stemcells objs))
           :out-of-scope-args
           (normalize-out-of-scope-args
            (if set-out-of-scope-args out-of-scope-args
                (mapcat #'out-of-scope-args objs)))
           :place-tree place-tree
           :mutations (if set-mutations mutations
                          (mapcat #'mutations objs))
           :node-tree node-tree)))

(defun merge-lines-into-block-list (objs)
  (when objs
    (let ((%objs (remove nil (butlast objs))))
      (remove #'null
              (append (loop :for i :in %objs
                         :for j :in (mapcar #'end-line %objs)
                         :append (remove nil (to-block i))
                         :append (listify (current-line j)));this should work
                      (to-block (last1 objs)))))))


(defun array-type-index-p (x)
  (or (numberp x)
      (and (symbolp x) (string= "*" x))))

(defun normalize-used-types (types)
  (remove-duplicates (flatten types) :from-end t :test #'v-type-eq))

(defun order-structs-by-dependency (struct-types)
  (let* ((type-graphs (mapcar (lambda (x) (cons x (walk-struct-dependencies x)))
                              struct-types))
         (flat-graphs (mapcar #'flatten type-graphs))
         (sorted-graphs (sort flat-graphs #'< :key #'length))
         (flat (flatten sorted-graphs)))
    (remove-duplicates flat :from-end t :test #'v-type-eq)))

(defun walk-struct-dependencies (type)
  (remove
   nil
   (mapcar (lambda (x)
             (destructuring-bind (_ slot-type &rest _1) x
               (declare (ignore _ _1))
               (when (typep slot-type 'v-struct)
                 (cons slot-type (walk-struct-dependencies slot-type)))))
           (v-slots type))))

;;----------------------------------------------------------------------
