(in-package :varjo)
(in-readtable :fn.reader)

(defun make-compiled (&key (type-set #() set-type-set) (current-line "")
                        to-block emit-set return-set used-types stemcells
                        out-of-scope-args pure
                        place-tree node-tree)
  (assert-flow-id-singularity (flow-ids (primary-type type-set)))
  (unless (or (every #'flow-ids type-set) (set-doesnt-need-flow-ids type-set))
    (error 'flow-ids-mandatory :for "compiled object"
           :primary-type (map 'vector #'type->type-spec type-set)))
  (unless set-type-set
    (error "The type-set must be specified when creating an instance of varjo:compiled"))
  (unless (or (eq node-tree :ignored)
              (and (typep node-tree 'ast-node)
                   (listp (slot-value node-tree 'args))))
    (error "invalid ast node-tree ~s" node-tree))
  (assert type-set () "Varjo: type-set is mandatory when making compiled objects")
  (assert-valid-type-set type-set :error-hint "ast-node")
  (let* ((used-types (%merge-used-types used-types type-set)))
    (make-instance 'compiled
                   :type-set type-set
                   :current-line current-line
                   :to-block to-block
                   :return-set return-set
                   :emit-set emit-set
                   :used-types used-types
                   :stemcells stemcells
                   :out-of-scope-args out-of-scope-args
                   :place-tree place-tree
                   :pure pure
                   :node-tree node-tree)))

(defun %merge-used-types (a b)
  (concatenate 'list a (remove-if λ(find _ a :test #'v-type-eq) b)))

(defmethod current-line (code-obj &optional even-when-ephemeral)
  (unless (and (ephemeral-p (primary-type code-obj)) (not even-when-ephemeral))
    (slot-value code-obj 'current-line)))

(defun add-higher-scope-val (code-obj value)
  (let* ((type (v-type-of value))
         (new-oos-args
          (if (ephemeral-p type)
              (out-of-scope-args code-obj)
              (cons value (out-of-scope-args code-obj)))))
    (copy-compiled code-obj :out-of-scope-args new-oos-args)))

(defun normalize-out-of-scope-args (args)
  (remove-duplicates args :test #'v-value-equal))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod v-primary-type-eq ((a compiled) (b compiled)
                              &optional (env *global-env*))
  (v-type-eq (primary-type a) (primary-type b) env))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod copy-compiled ((code-obj compiled)
                          &key (type-set nil set-type-set)
                            (current-line nil set-current-line)
                            (to-block nil set-block)
                            (emit-set nil set-emit-set)
                            (return-set nil set-return-set)
                            (stemcells nil set-stemcells)
                            (out-of-scope-args nil set-out-of-scope-args)
                            (place-tree nil set-place-tree)
                            (pure nil set-pure)
                            (node-tree nil set-node-tree))
  (let ((type-set (if set-type-set type-set (type-set code-obj))))
    (assert type-set () "Varjo: type-set is mandatory when copying compiled objects")
    (assert-valid-type-set type-set :error-hint "ast-node")
    (make-compiled
     :type-set type-set
     :current-line (if set-current-line current-line
                       (current-line code-obj t))
     :to-block (if set-block to-block (remove nil (to-block code-obj)))
     :return-set (if set-return-set return-set (return-set code-obj))
     :emit-set (if set-emit-set emit-set (emit-set code-obj))
     :used-types (used-types code-obj)
     :stemcells (if set-stemcells stemcells (stemcells code-obj))
     :out-of-scope-args (if set-out-of-scope-args
                            out-of-scope-args
                            (remove nil (out-of-scope-args code-obj)))
     :place-tree (if set-place-tree place-tree (place-tree code-obj))
     :pure (if set-pure pure (pure-p code-obj))
     :node-tree (if set-node-tree node-tree (node-tree code-obj)))))

(defmethod merge-compiled ((objs list)
                           &key type-set
                             current-line
                             (to-block nil set-block)
                             (emit-set nil set-emit-set)
                             (return-set nil set-return-set)
                             (stemcells nil set-stemcells)
                             (out-of-scope-args nil set-out-of-scope-args)
                             place-tree
                             (pure nil set-pure)
                             node-tree)
  (assert type-set () "Varjo: type-set is mandatory when merging compiled objects")
  (assert-valid-type-set type-set :error-hint "ast-node")
  (let ((return-set
         (if set-return-set
             return-set
             (merge-return-sets (remove nil (mapcar #'return-set objs)))))
        (emit-set
         (if set-emit-set
             emit-set
             (merge-emit-sets (remove nil (mapcar #'emit-set objs))))))
    (unless (or (every #'flow-ids type-set)
                (set-doesnt-need-flow-ids type-set))
      (error 'flow-ids-mandatory :for :code-object
             :primary-type (map 'vector #'type->type-spec type-set)))
    (make-compiled :type-set type-set
                   :current-line current-line
                   :to-block (if set-block to-block
                                 (mappend #'to-block objs))
                   :emit-set emit-set
                   :return-set return-set
                   :used-types (mappend #'used-types objs)
                   :stemcells (if set-stemcells stemcells
                                  (mappend #'stemcells objs))
                   :out-of-scope-args
                   (normalize-out-of-scope-args
                    (if set-out-of-scope-args out-of-scope-args
                        (mappend #'out-of-scope-args objs)))
                   :place-tree place-tree
                   :pure (if set-pure pure (every #'pure-p objs))
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
  (let* ((type-graphs (mapcar (lambda (x)
                                (cons x (walk-struct-dependencies x)))
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
