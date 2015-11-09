(in-package :varjo)

;; these are created whenever a new value is, they flow through the code-objects
;; allowing us to detect everywhere a value flows through a program.
;; There are two complications, multiple value return and conditionals
;; - Multiple value return is handled by having a list of flow-identifiers
;; - conditional handled by each flow-identifier contain a list of identifiers
;;   itself. This is the collection of all the values that could flow through
;;   this point.

(defclass flow-identifier ()
  ((ids :initform nil :initarg :ids :reader ids)))

(defmethod print-object ((o flow-identifier) stream)
  (let* ((ids (slot-value o 'ids))
	 (ids (if (> (length ids) 6)
		  (append (subseq ids 0 6) '(:etc))
		  ids)))
    (format stream "#<FLOW-IDENTIFIER :ids ~s>" ids)))

(let ((flow-id -1))
  (defun %gen-flow-id () (list (incf flow-id))))

(let ((gl-flow-id 0))
  (defun %gen-flow-gl-id () (list (decf gl-flow-id))))

(defun id~= (id-a id-b)
  (not (null (intersection (slot-value id-a 'ids)
			   (slot-value id-b 'ids)))))

(defun id= (id-a id-b)
  (equal (sort (copy-list (slot-value id-a 'ids)) #'<)
	 (sort (copy-list (slot-value id-b 'ids)) #'<)))

(defun flow-id! (&rest ids)
  (labels ((internal-ids (x) (slot-value x 'ids)))
    (if (null ids)
	(make-instance 'flow-identifier :ids (%gen-flow-id))
	(make-instance 'flow-identifier
		       :ids (sort (copy-list (remove-duplicates
					      (mapcat #'internal-ids ids)))
				  #'<)))))

(defun %gl-flow-id! ()
  (make-instance 'flow-identifier :ids (%gen-flow-gl-id)))

(defun type-doesnt-need-flow-id (type)
  (or (typep type 'v-error)
      (typep type 'v-void)
      (typep type 'v-none)
      (eq type 'v-void)
      (eq type 'v-none)
      (eq type :void)
      (eq type :none)))

(defun take (count func &rest args)
  (loop for i below count collect (apply func args)))
