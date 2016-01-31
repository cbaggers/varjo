(in-package :varjo)
(in-readtable fn:fn-reader)

(defclass ast-node ()
  ((starting-env :initarg :starting-env :reader ast-starting-env)
   (ending-env :initarg :ending-env :reader ast-ending-env)
   (kind :initarg :kind :reader ast-kind)
   (return-type :initarg :return-type :reader ast-return-type)
   (flow-id :initarg :flow-id :reader ast-flow-id)
   (flow-id-origin :initarg :flow-id-origin :initform :incomplete
		   :reader ast-flow-id-origin)
   (val-origin :initarg :val-origin :initform :incomplete
	       :reader ast-val-origin)
   (parent :initarg :parent :initform :incomplete :reader ast-parent)
   (args :initarg :args :initform nil :reader ast-args)
   (val-origins :initarg :val-origins :initform :incomplete)
   (flow-id-origins :initarg :flow-id-origins :initform :incomplete)))

(defmethod initialize-instance :after ((ast ast-node) &rest initargs)
  (declare (ignore initargs))
  (assert-flow-id-singularity (ast-flow-id ast)))

(defmethod get-var (var-name (node ast-node))
  (get-var var-name (ast-starting-env node)))

(defmethod ast-kindp (node kind)
  (let* ((actual-kind (ast-kind node)))
    (cond
      ((typep kind 'v-function) (eq kind actual-kind))
      ((typep actual-kind 'v-function) (eq kind (name actual-kind)))
      (t (eq kind actual-kind)))))



(defmethod ast-typep (node type)
  (let ((type (if (or (typep type 'v-spec-type) (typep type 'v-t-type))
		  type
		  (type-spec->type type))))
    (v-typep (ast-return-type node) type)))

;;----------------------------------------------------------------------

(deftclass origin)

(deftclass (ast-origin (:include origin))
  node)

(deftclass (uniform-origin (:include origin))
  name
  node)

(deftclass (stemcell-origin (:include origin))
  name
  node)

(defmethod origin-name ((origin stemcell-origin))
  (stemcell-origin-name origin))

(defmethod origin-name ((origin uniform-origin))
  (uniform-origin-name origin))

;;----------------------------------------------------------------------

(defgeneric flow-id-origins (node &optional error-on-missingp context))

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
	       (mapcar #'per-flow-id (listify (ast-flow-id node)))))
      (flatten
       (typecase r
	 (hash-table (get-origins))
	 (null (ast-flow-id-origin node)))))))

(defmethod val-origins ((node ast-node) &optional error-on-missingp)
  (let ((r (slot-value node 'val-origins)))
    (labels ((get-seen (raw-id errorp)
	       (or (gethash raw-id r)
		   (when errorp
		     (error "Could not find origin for ~s" raw-id))))

	     (f-origin (val-id fcall-node)
	       (let* ((func (ast-kind fcall-node))
		      (flow-result (flow-ids func)))
		 (if (m-flow-id-p flow-result)
		     (let ((return-pos (slot-value val-id 'return-pos)))
		       (mapcar λ(get-seen (slot-value _ 'val) t)
			       (ids (nth return-pos
					 (m-value-ids flow-result)))))
		     (mapcar λ(get-seen (slot-value _ 'val) t)
		       (ids flow-result)))))

	     (per-id (val-id node)
	       (let ((raw (slot-value val-id 'val)))
		 (or (get-seen raw error-on-missingp)
		     (setf (gethash raw r)
			   (if (typep (ast-kind node) 'v-user-function)
			       (f-origin val-id node)
			       node)))))

	     (per-flow-id (flow-id node)
	       (mapcar λ(per-id _ node) (ids flow-id)))

	     (get-origins (node)
	       (mapcar λ(per-flow-id _ node) (listify (ast-flow-id node)))))
      (flatten
       (typecase r
	  (hash-table (get-origins node))
	  (null (ast-val-origin node)))))))


(defmethod flow-ids ((node ast-node))
  (ast-flow-id node))

(defparameter *node-kinds* '(:get :get-stemcell :get-v-value :literal
			     :error))

(defun ast-node! (kind args return-type flow-id starting-env ending-env)
  (assert (if (keywordp kind)
	      (member kind *node-kinds*)
	      t))
  (let ((return-type (if (and return-type
			      (or (listp return-type)
				  (symbolp return-type)))
			 (type-spec->type return-type)
			 return-type)))
    (make-instance 'ast-node
		   :kind kind
		   :args (listify args)
		   :return-type return-type
		   :flow-id flow-id
		   :starting-env starting-env
		   :ending-env ending-env)))

(defun copy-ast-node (node
		      &key
			(kind nil set-kind)
			(args nil set-args)
			(return-type nil set-return-type)
			(flow-id nil set-flow-id)
			(flow-id-origin nil set-fio)
			(val-origin nil set-vo)
			(starting-env nil set-starting-env)
			(ending-env nil set-ending-env)
			(parent nil set-parent))
  (make-instance
   'ast-node
   :kind (if set-kind kind (ast-kind node))
   :args (if set-args args (ast-args node))
   :return-type (if set-return-type return-type (ast-return-type node))
   :starting-env (if set-starting-env starting-env (ast-starting-env node))
   :ending-env (if set-ending-env ending-env (ast-ending-env node))
   :parent (if set-parent parent (ast-parent node))
   :flow-id (if set-flow-id flow-id (ast-flow-id node))
   :flow-id-origin (if set-fio flow-id-origin (ast-flow-id-origin node))
   :val-origin (if set-vo val-origin (ast-val-origin node))))

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
      (code (walk-node (node-tree from-node) :parent nil))
      (varjo-compile-result (walk-node (ast from-node) :parent nil))
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
			   (ast-flow-id node))
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
		       (:get (first args))
		       (:get-stemcell (first args))
		       (:literal (first args))
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
  (declare (optimize (debug 3) (speed 0)))
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
