(in-package :varjo)
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

(defclass multi-return-flow-id ()
  ((m-value-ids :initform nil :initarg :m-value-ids :reader m-value-ids)))

(defun m-flow-id-p (id)
  (typep id 'multi-return-flow-id))

(defun flow-id-p (id)
  (typep id 'flow-identifier))

(defclass flow-identifier ()
  ((ids :initform nil :initarg :ids :reader ids)))

(defclass bare-flow-id ()
  ((val :initarg :val)
   (return-pos :initform 0 :initarg :return-pos)))

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

(defvar %flow-id -1)
(defvar flow-gen-func
  (lambda ()
    (error "Trying to generate flow-id outside of a flow-id-scope")))

;;----------------------------------------------------------------------
;; scoping

(defmacro flow-id-scope (&body body)
  `(let ((%flow-id %flow-id)
	 (flow-gen-func (lambda () (bare-id! (incf %flow-id)))))
     ,@body))

;;----------------------------------------------------------------------
;; construction

(defun m-flow-id! (flow-ids)
  (assert (and (listp flow-ids)
	       (every #'flow-id-p flow-ids)))
  (make-instance 'multi-return-flow-id
		 :m-value-ids flow-ids))

(defun flow-id! (&rest ids)
  (let ((ids (remove nil ids)))
    (labels ((key (_) (slot-value _ 'val)))
      (if (null ids)
	  (make-instance 'flow-identifier :ids (list (funcall flow-gen-func)))
	  (make-instance 'flow-identifier
			 :ids (sort (copy-list (remove-duplicates
						(mapcat #'ids ids)
						:key #'key))
				    #'< :key #'key))))))

(defun flow-id+meta! (&key return-pos)
  (let ((r (flow-id!)))
    (setf (slot-value (first (listify (ids r))) 'return-pos)
	  return-pos)
    r))

(defun %gl-flow-id! ()
  (make-instance 'flow-identifier :ids (list (%gen-flow-gl-id))))

(defun type-doesnt-need-flow-id (type)
  (or (typep type 'v-error)
      (typep type 'v-void)
      (typep type 'v-none)
      (eq type 'v-void)
      (eq type 'v-none)
      (eq type :void)
      (eq type :none)))


;;----------------------------------------------------------------------
;; inspection

(defun id~= (id-a id-b)
  (assert (typep id-a 'flow-identifier))
  (assert (typep id-b 'flow-identifier))
  (unless (or (null id-a) (null id-b))
    (not (null (intersection (listify (ids id-a)) (listify (ids id-b))
			     :key λ(slot-value _ 'val))))))

(defun id= (id-a id-b)
  (assert (typep id-a 'flow-identifier))
  (assert (typep id-b 'flow-identifier))
  (labels ((key (_) (slot-value _ 'val)))
    (unless (or (null id-a) (null id-b))
      (equal (sort (copy-list (ids id-a)) #'< :key #'key)
	     (sort (copy-list (ids id-b)) #'< :key #'key)))))

(defun assert-flow-id-singularity (flow-id)
  (assert (or (null flow-id)
	      (not (listp flow-id)))))
