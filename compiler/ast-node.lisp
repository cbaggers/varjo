(in-package :varjo)
(in-readtable fn:fn-reader)

(defmethod initialize-instance :after ((ast ast-node) &rest initargs)
  (declare (ignore initargs))
  (assert-flow-id-singularity (flow-ids ast)))

(defmethod get-symbol-binding (var-name respect-scope-rules (node ast-node))
  (get-symbol-binding var-name respect-scope-rules (ast-starting-env node)))

(defmethod ast-kindp (node kind)
  (let* ((actual-kind (ast-kind node)))
    (cond
      ((typep kind 'v-function) (eq kind actual-kind))
      ((typep actual-kind 'v-function) (eq kind (name actual-kind)))
      (t (eq kind actual-kind)))))

(defmethod ast-typep (node type)
  (assert (typep type 'v-type))
  (v-typep (ast-return-type node) type))

;;----------------------------------------------------------------------

(defstruct origin)

(defstruct (ast-origin (:include origin))
  node)

(defstruct (uniform-origin (:include origin))
  name
  node)

(defstruct (stemcell-origin (:include origin))
  name
  node)

(defmethod origin-name ((origin stemcell-origin))
  (stemcell-origin-name origin))

(defmethod origin-name ((origin uniform-origin))
  (uniform-origin-name origin))

;;----------------------------------------------------------------------

(defmethod flow-id-origins ((flow-id flow-identifier)
                            &optional (error-on-missingp t) context)
  "Gets the ast-node/s where this flow-id originated"
  (assert error-on-missingp)
  (let ((r (typecase context
             (ast-node (slot-value context 'flow-id-origins))
             (t (error "When passing a flow-id to flow-id-origins the context
must be specified and must be of type 'ast-node")))))
    (labels ((get-seen (raw-id)
               (or (gethash raw-id r)
                   (error "Could not find origin for ~s" raw-id)))

             (per-id (val-id)
               (let ((raw (slot-value val-id 'val)))
                 (get-seen raw))))

      (flatten (mapcar #'per-id (ids flow-id))))))

(defmethod flow-id-origins ((node ast-node)
                            &optional error-on-missingp context)
  "Gets the ast node/s where this node's flow-ids originated.
   If it hasnt been found yet then this must be the origin and
   it is added to the origins map.
   To emphasise the above. THIS IS DESTRUCTIVE"
  (when context
    (error "Do not pass context when node argument is of type ast-node as
context is implicit"))
  (let ((r (slot-value node 'flow-id-origins)))
    (labels ((get-seen (raw-id)
               (or (gethash raw-id r)
                   (when error-on-missingp
                     (error "Could not find origin for ~s" raw-id))))

             (per-id (val-id)
               (let ((raw (slot-value val-id 'val)))
                 (or (get-seen raw)
                     (setf (gethash raw r)
                           (if (eq (ast-kind node) :get-stemcell)
                               (make-stemcell-origin
                                :name (first (ast-args node))
                                :node node)
                               (make-ast-origin :node node))))))

             (per-flow-id (flow-id)
               (mapcar #'per-id (ids flow-id)))

             (get-origins ()
               (mapcar #'per-flow-id (listify (flow-ids node)))))
      (flatten
       (typecase r
         (hash-table (get-origins))
         (null (ast-flow-id-origin node)))))))

(defmethod val-origins ((node ast-node) &optional error-on-missingp)
  (let ((origins-dict (slot-value node 'val-origins)))
    (labels ((get-seen (raw-id)
               (or (gethash raw-id origins-dict)
                   (when error-on-missingp
                     (error "Could not find origin for ~s" raw-id))))

             (f-origin (val-id fcall-node)
               (let* ((func (ast-kind fcall-node)))
                 (if (> (length (ast-return-type func)) 1)
                     (let ((return-pos (slot-value val-id 'return-pos)))
                       (mapcar λ(or (get-seen (slot-value _ 'val)) fcall-node)
                               (ids (flow-ids
                                     (nth return-pos
                                          (ast-return-type func))))))
                     (progn
                       (assert (flow-ids func) (func)
                               "trying to process flow-ids of ~a but found nil"
                               func)
                       (mapcar λ(or (get-seen (slot-value _ 'val)) fcall-node)
                               (ids (flow-ids func)))))))

             (per-id (val-id node)
               (let ((raw (slot-value val-id 'val)))
                 (or (get-seen raw)
                     (setf (gethash raw origins-dict)
                           (if (typep (ast-kind node) 'v-user-function)
                               (f-origin val-id node)
                               node)))))

             (per-flow-id (flow-id node)
               (mapcar λ(per-id _ node) (ids flow-id)))

             (get-origins (node)
               (mapcar λ(per-flow-id _ node) (listify (flow-ids node)))))
      (flatten
       (typecase origins-dict
         (hash-table (get-origins node))
         (null (ast-val-origin node)))))))


(defun ast-node! (kind args return-type-set starting-env ending-env)
  (assert (if (keywordp kind)
              (member kind *ast-node-kinds*)
              t))
  (assert-valid-type-set return-type-set :error-hint "ast-node")
  (make-instance 'ast-node
                 :kind kind
                 :args (listify args)
                 :return-type return-type-set
                 :starting-env starting-env
                 :ending-env ending-env))

(defun copy-ast-node (node
                      &key
                        (kind nil set-kind)
                        (args nil set-args)
                        (return-type-set nil set-return-type)
                        (flow-id-origin nil set-fio)
                        (val-origin nil set-vo)
                        (starting-env nil set-starting-env)
                        (ending-env nil set-ending-env)
                        (parent nil set-parent))
  (let ((return-type-set (if set-return-type
                             return-type-set
                             (ast-return-type node))))
    (assert-valid-type-set return-type-set)
    (make-instance
     'ast-node
     :kind (if set-kind kind (ast-kind node))
     :args (if set-args args (ast-args node))
     :return-type return-type-set
     :starting-env (if set-starting-env starting-env (ast-starting-env node))
     :ending-env (if set-ending-env ending-env (ast-ending-env node))
     :parent (if set-parent parent (ast-parent node))
     :flow-id-origin (if set-fio flow-id-origin (ast-flow-id-origin node))
     :val-origin (if set-vo val-origin (ast-val-origin node)))))

(defun walk-ast (func from-node &key include-parent)
  (labels ((walk-node (ast &key parent)
             (if (eq ast :ignored)
                 :ignored
                 (typecase ast
                   (ast-node
                    (let ((args `(,ast
                                  ,#'walk-node
                                  ,@(when include-parent `(:parent ,parent)))))
                      (apply func args)))
                   (list (mapcar λ(walk-node _ :parent parent) ast))
                   (t ast)))))
    (typecase from-node
      (compiled (walk-node (node-tree from-node) :parent nil))
      (compiled-stage (walk-node (ast from-node) :parent nil))
      (ast-node (walk-node from-node :parent nil))
      (t (error "object with the invalid type ~s passed to ast->code"
                (type-of from-node))))))

(defun visit-ast-nodes (func x)
  (labels ((f (node walk)
             (funcall func node)
             (with-slots (args) node
               (mapcar walk args))))
    (walk-ast #'f x)
    t))

(defun filter-ast-nodes (func x)
  (let (r)
    (visit-ast-nodes λ(when (funcall func _) (push _ r)) x)
    (reverse r)))

(defun ast->pcode (x &key show-flow-ids)
  (labels ((f (node walk)
             (with-slots (kind args) node
               (let ((name (if (typep kind 'v-function)
                               (name kind)
                               kind)))
                 `(,@(when show-flow-ids
                           (flow-ids node))
                     ,name ,@(mapcar walk args))))))
    (walk-ast #'f x)))

(defun filter-&-func (node filter-func-pairs
                      set-seen has-been-seen)
  (let ((has-changed nil))
    (labels ((ff (accum pair)
               (dbind (filter func) pair
                 (if (and (typep accum 'ast-node)
                          (not (funcall has-been-seen node filter))
                          (funcall filter accum))
                     (progn
                       (setf has-changed t)
                       (funcall set-seen node filter)
                       (funcall func accum))
                     accum))))
      (let ((res (reduce #'ff filter-func-pairs :initial-value node)))
        (values res has-changed)))))

(defun ast->code (ast &key filter-func-pairs)
  (let ((has-changed nil)
        (seen nil))
    (labels ((set-seen (node filter)
               (push (cons node filter) seen))
             (has-been-seen (node filter)
               (member (cons node filter) seen :test #'equal))
             (serialize-node (node walk)
               (with-slots (kind args) node
                 (if (keywordp kind)
                     (case kind
                       (:function-top-level (mapcar walk args))
                       (:get (first args))
                       (:get-stemcell (first args))
                       (:literal (first args))
                       (:code-section args)
                       (:funcall `(funcall ,@(mapcar walk args)))
                       (:break `(%break ,@args))
                       (t (error "invalid node kind ~s found in result"
                                 kind)))
                     `(,kind ,@(mapcar walk args)))))
             (f (node walk)
               (vbind (node changed?)
                   (filter-&-func node filter-func-pairs
                                  #'set-seen #'has-been-seen)
                 (when changed? (setf has-changed t))
                 (if changed?
                     (funcall walk node)
                     (serialize-node node walk)))))
      (values (walk-ast #'f ast) has-changed))))

(defun ast-deep-replace (ast filter func)
  (labels ((to-code (x)
             (typecase x
               (ast-node (ast->code x))
               (list (mapcar #'to-code x))
               (t x)))
           (or-max (x y)
             (cond ((null x) y)
                   ((null y) x)
                   (t (max x y))))
           (args-sweep (ast-args)
             (let* ((first-pass (mapcar λ(multiple-value-list
                                          (ast-deep-replace _ filter func))
                                        ast-args))
                    (max-halted-depth (when first-pass
                                        (reduce #'or-max
                                                (mapcar #'second first-pass))))
                    (args (mapcar λ(if (or (null max-halted-depth)
                                           (and (second _)
                                                (= (second _)
                                                   max-halted-depth)))
                                       (to-code (first _))
                                       (to-code _1))
                                  first-pass
                                  ast-args)))
               (list args max-halted-depth))))
    (typecase ast
      (ast-node
       (dbind (args halted-at) (args-sweep (ast-args ast))
         (let ((kind (ast-kind ast)))
           (if (and (not halted-at) (funcall filter ast))
               (values (funcall func ast) 0)
               (values `(,kind ,@args) (when halted-at (1+ halted-at)))))))
      (list (dbind (args halted-at) (args-sweep ast)
              (values args halted-at)))
      (t (values ast nil)))))
