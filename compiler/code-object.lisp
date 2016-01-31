;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)

(defclass code ()
  ((type :initarg :type :initform nil :reader code-type)
   (current-line :initarg :current-line :initform "" :reader current-line)
   (signatures :initarg :signatures :initform nil :reader signatures)
   (to-block :initarg :to-block :initform nil :reader to-block)
   (to-top :initarg :to-top :initform nil :reader to-top)
   (out-vars :initarg :out-vars :initform nil :reader out-vars)
   (used-types :initarg :used-types :initform nil :reader used-types)
   (returns :initarg :returns :initform nil :reader returns)
   (multi-vals :initarg :multi-vals :initform nil :reader multi-vals)
   (stem-cells :initarg :stemcells :initform nil :reader stemcells)
   (out-of-scope-args :initarg :out-of-scope-args :initform nil
                      :reader out-of-scope-args)
   (flow-ids :initarg :flow-ids :initform (error 'flow-id-must-be-specified-co)
	     :reader flow-ids)
   (mutations :initarg :mutations :initform nil :reader mutations)
   (place-tree :initarg :place-tree :initform nil :reader place-tree)
   (node-tree :initarg :node-tree :initform nil :reader node-tree)))

(defun code! (&key (type nil set-type) (current-line "") signatures to-block
		to-top out-vars used-types returns multi-vals stemcells
		out-of-scope-args flow-ids mutations place-tree
		node-tree)
  (assert-flow-id-singularity flow-ids)
  (unless set-type
    (error "Type must be specified when creating an instance of varjo:code"))
  (unless (or flow-ids (type-doesnt-need-flow-id type))
    (error 'flow-ids-mandatory :for :code-object))
  (unless (or (eq node-tree :ignored)
	      (and (typep node-tree 'ast-node)
		   (listp (slot-value node-tree 'args))))
    (error "invalid ast node-tree ~s" node-tree))
  (let* ((type-obj (if (typep type 'v-t-type) type (type-spec->type type)))
         (type-spec (type->type-spec type-obj))
	 (used-types (if (and (not (find type-spec used-types))
			      (not (eq type-spec 'v-none)))
			 (cons (listify type-spec) used-types)
			 used-types)))
    (make-instance 'code
		   :type type-obj
		   :current-line current-line
		   :signatures signatures
		   :to-block to-block
		   :to-top to-top
		   :out-vars out-vars
		   :returns returns
		   :used-types used-types
		   :multi-vals multi-vals
		   :stemcells stemcells
		   :out-of-scope-args out-of-scope-args
		   :flow-ids flow-ids
		   :place-tree place-tree
		   :mutations mutations
		   :node-tree node-tree)))

(defun add-higher-scope-val (code-obj value)
  (copy-code code-obj
	     :out-of-scope-args (cons value (out-of-scope-args code-obj))))

(defun normalize-out-of-scope-args (args)
  (remove-duplicates args :test #'v-value-equal))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod v-code-type-eq ((a code) (b code) &optional (env *global-env*))
  (v-type-eq (code-type a) (code-type b) env))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun make-code-obj (type current-line &key flow-ids place-tree node-tree)
  (code! :type type :current-line current-line
	 :flow-ids flow-ids :place-tree (listify place-tree)
	 :node-tree node-tree))

(defun make-none-ob () (make-code-obj :none nil))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; [TODO] this doesnt work (properly) yet but is a fine starting point
(defgeneric copy-code (code-obj &key type current-line to-block to-top
                                  out-vars returns multi-vals
                                  stemcells out-of-scope-args flow-ids
				  place-tree mutations node-tree))
(defmethod copy-code ((code-obj code)
                      &key (type nil set-type)
                        (current-line nil set-current-line)
                        (signatures nil set-sigs)
                        (to-block nil set-block)
                        (to-top nil set-top)
                        (out-vars nil set-out-vars)
                        (returns nil set-returns)
                        (multi-vals nil set-multi-vals)
                        (stemcells nil set-stemcells)
                        (out-of-scope-args nil set-out-of-scope-args)
			(flow-ids nil set-flow-ids)
			(place-tree nil set-place-tree)
			(mutations nil set-mutations)
			(node-tree nil set-node-tree))
  (let ((type (if set-type type (code-type code-obj)))
	(flow-ids (if set-flow-ids flow-ids (flow-ids code-obj))))
    (unless (or flow-ids (type-doesnt-need-flow-id type))
      (error 'flow-ids-mandatory :for :code-object :code-type type))
    (code! :type type
	   :current-line (if set-current-line current-line
			     (current-line code-obj))
	   :signatures (if set-sigs signatures (signatures code-obj))
	   :to-block (if set-block to-block (remove nil (to-block code-obj)))
	   :to-top (if set-top to-top (remove nil (to-top code-obj)))
	   :out-vars (if set-out-vars out-vars (out-vars code-obj))
	   :returns (listify (if set-returns returns (returns code-obj)))
	   :used-types (used-types code-obj)
	   :multi-vals (if set-multi-vals multi-vals (multi-vals code-obj))
	   :stemcells (if set-stemcells stemcells (stemcells code-obj))
	   :out-of-scope-args (if set-out-of-scope-args
				  out-of-scope-args
				  (remove nil (out-of-scope-args code-obj)))
	   :flow-ids flow-ids
	   :place-tree (if set-place-tree place-tree (place-tree code-obj))
	   :mutations (if set-mutations mutations (mutations code-obj))
	   :node-tree (if set-node-tree node-tree (node-tree code-obj)))))


(defgeneric merge-obs (objs &key type current-line to-block
                              to-top out-vars returns multi-vals
                              stemcells out-of-scope-args flow-ids
			      place-tree mutations node-tree))

(defmethod merge-obs ((objs list)
                      &key type
                        current-line
                        (signatures nil set-sigs)
                        (to-block nil set-block)
                        (to-top nil set-top)
                        (out-vars nil set-out-vars)
                        (returns nil set-returns)
                        multi-vals
                        (stemcells nil set-stemcells)
                        (out-of-scope-args nil set-out-of-scope-args)
			(flow-ids nil set-flow-ids)
			place-tree
			(mutations nil set-mutations)
			node-tree)
  (unless (or flow-ids (type-doesnt-need-flow-id type))
    (error 'flow-ids-mandatory :for :code-object))
  (code! :type (if type type (error "type is mandatory"))
	 :current-line current-line
	 :signatures (if set-sigs signatures
			 (mapcat #'signatures objs))
	 :to-block (if set-block to-block
		       (mapcat #'to-block objs))
	 :to-top (if set-top to-top (mapcat #'to-top objs))
	 :out-vars (if set-out-vars out-vars (mapcat #'out-vars objs))
	 :returns (listify (if set-returns returns (merge-returns objs)))
	 :used-types (mapcar #'used-types objs)
	 :multi-vals multi-vals
	 :stemcells (if set-stemcells stemcells
			(mapcat #'stemcells objs))
	 :out-of-scope-args
	 (normalize-out-of-scope-args
	  (if set-out-of-scope-args out-of-scope-args
	      (mapcat #'out-of-scope-args objs)))
	 :flow-ids (if set-flow-ids
		       flow-ids
		       (error 'flow-id-must-be-specified-co))
	 :place-tree place-tree
	 :mutations (if set-mutations mutations
			(mapcat #'mutations objs))
	 :node-tree node-tree))

(defun merge-returns (objs)
  (let* ((returns (mapcar #'returns objs))
         (returns (remove nil returns))
         (first (first returns))
         (match (or (every #'null returns)
                    (loop :for r :in (rest returns) :always
                       (and (= (length first) (length r))
                            (mapcar #'v-type-eq first r))))))
    ;; {TODO} Proper error needed here
    (if match
        (listify first)
        (progn (error 'return-type-mismatch :returns returns)))))

(defun merge-lines-into-block-list (objs)
  (when objs
    (let ((%objs (remove nil (butlast objs))))
      (remove #'null
              (append (loop :for i :in %objs
                         :for j :in (mapcar #'end-line %objs)
                         :append (remove nil (to-block i))
                         :append (listify (current-line j)));this should work
                      (to-block (last1 objs)))))))


(defun normalize-used-types (types)
  (remove-duplicates
   (loop :for item :in (remove nil types) :append
      (cond ((atom item) (list item))
	    ((and (listp item) (or (numberp (second item))
				   (and (listp (second item))
					(> (length (second item)) 0)
					(every #'numberp (second item)))))
	     (list item))
	    (t (normalize-used-types item))))
   :from-end t))

(defun find-used-user-structs (code-obj env)
  (declare (ignore env))
  (let* ((used-types (normalize-used-types (used-types code-obj)))
	 (struct-types
	  (remove nil
		  (loop :for spec :in used-types
		     :for type = (type-spec->type spec)
		     :if (or (typep type 'v-struct)
			     (and (typep type 'v-array)
				  (typep (v-element-type type) 'v-struct)))
		     :collect spec)))
	 (result (order-structs-by-dependency struct-types)))
    result))

(defun order-structs-by-dependency (struct-types)
  (let* ((types (mapcar #'type-spec->type struct-types))
	 (type-graphs (mapcar (lambda (x n)
				(cons n (walk-struct-dependencies x)))
			      types struct-types))
	 (flat-graphs (mapcar #'flatten type-graphs))
	 (sorted-graphs (sort flat-graphs #'< :key #'length))
	 (flat (flatten sorted-graphs)))
    (remove-duplicates flat :from-end t)))

(defun walk-struct-dependencies (type)
  (remove
   nil
   (mapcar (lambda (x)
	     (destructuring-bind (_ slot-type &rest _1) x
	       (declare (ignore _ _1))
	       (let ((stype (type-spec->type slot-type)))
		 (when (typep stype 'v-struct)
		   (cons slot-type (walk-struct-dependencies stype))))))
	   (v-slots type))))
