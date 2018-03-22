(in-package :varjo.internals)
(in-readtable :fn.reader)

(defun make-compiled (&key (type-set #() set-type-set) (current-line "")
                        to-block emit-set return-set used-types stemcells
                        out-of-scope-args pure
                        place-tree)
  (assert (or (glsl-chunk-p to-block) ;; temporary
              (null to-block)))
  (assert-flow-id-singularity (flow-ids (primary-type type-set)))
  (unless (or (every #'flow-ids type-set) (set-doesnt-need-flow-ids type-set))
    (error 'flow-ids-mandatory :for "compiled object"
           :primary-type (map 'vector #'type->type-spec type-set)))
  (unless set-type-set
    (error "The type-set must be specified when creating an instance of varjo:compiled"))
  (assert type-set () "Varjo: type-set is mandatory when making compiled objects")
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
                 :pure pure))

(defmethod primary-type ((compiled compiled))
  (primary-type (type-set compiled)))

(defmethod primary-type ((set vector))
  (if (emptyp set)
      (type-spec->type :void)
      (elt set 0)))


(defmethod current-line (code-obj &optional even-when-ephemeral)
  (unless (and (not even-when-ephemeral)
               (ephemeral-p (primary-type code-obj)))
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

(defmethod v-primary-type-eq ((a compiled) (b compiled))
  (v-type-eq (primary-type a) (primary-type b)))

(defmethod v-primary-type-eq ((a compiled) (b v-type))
  (v-type-eq (primary-type a) b))

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
                            (used-types nil set-used-types))
  (let ((type-set (if set-type-set type-set (type-set code-obj))))
    (when set-type-set
      (assert type-set () "Varjo: type-set is mandatory when copying compiled objects"))
    (make-compiled
     :type-set type-set
     :current-line (if set-current-line current-line
                       (current-line code-obj t))
     :to-block (if set-block to-block (to-block code-obj))
     :return-set (if set-return-set return-set (return-set code-obj))
     :emit-set (if set-emit-set emit-set (emit-set code-obj))
     :stemcells (if set-stemcells stemcells (stemcells code-obj))
     :out-of-scope-args (if set-out-of-scope-args
                            out-of-scope-args
                            (remove nil (out-of-scope-args code-obj)))
     :place-tree (if set-place-tree place-tree (place-tree code-obj))
     :pure (if set-pure pure (pure-p code-obj))
     :used-types (if set-used-types used-types (used-types code-obj)))))

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
                             (used-types nil set-used-types))
  (assert type-set () "Varjo: type-set is mandatory when merging compiled objects")
  (let ((return-set
         (if set-return-set
             return-set
             (merge-return-sets (mapcar #'return-set objs))))
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
                   :to-block (if set-block
                                 to-block
                                 (join-glsl-chunks
                                  (mapcar #'to-block objs)))
                   :emit-set emit-set
                   :return-set return-set
                   :used-types (if set-used-types
                                   used-types
                                   (mappend #'used-types objs))
                   :stemcells (if set-stemcells stemcells
                                  (mappend #'stemcells objs))
                   :out-of-scope-args
                   (normalize-out-of-scope-args
                    (if set-out-of-scope-args out-of-scope-args
                        (mappend #'out-of-scope-args objs)))
                   :place-tree place-tree
                   :pure (if set-pure pure (every #'pure-p objs)))))

(defun array-type-index-p (x)
  (or (numberp x)
      (and (symbolp x) (string= "*" x))))

(defun normalize-used-types (types)
  (remove-duplicates (flatten types) :from-end t :test #'v-type-eq))

;;----------------------------------------------------------------------

(defun end-line (obj &optional force)
  (assert (not force))
  (when obj
    (if (null (current-line obj))
        obj
        (copy-compiled obj :current-line (end-line-str (current-line obj))
                       :place-tree nil))))

(defun end-line-str (str)
  (format nil "~a;" str))

;;----------------------------------------------------------------------
