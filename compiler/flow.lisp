(in-package :varjo)

;; these are created whenever a new value is, they flow through the code-objects
;; allowing us to detect everywhere a value flows through a program.
;; There are two complications, multiple value return and conditionals
;; - Multiple value return is handled by having a list of flow-identifiers
;; - conditional handled by each flow-identifier contain a list of identifiers
;;   itself. This is the collection of all the values that could flow through
;;   this point.

;;----------------------------------------------------------------------
;; internals

(defclass flow-identifier ()
  ((ids :initform nil :initarg :ids :reader ids)))

(defmethod print-object ((o flow-identifier) stream)
  (let* ((ids (ids o))
	 (ids (if (> (length ids) 6)
		  (append (subseq ids 0 6) '(:etc))
		  ids)))
    (format stream "#<FLOW-ID :ids ~s>" ids)))

(let ((gl-flow-id 0))
  (defun %gen-flow-gl-id () (list (decf gl-flow-id))))

(defvar %flow-id -1)
(defvar flow-gen-func
  (lambda ()
    (error "Trying to generate flow-id outside of a flow-id-scope")))

;;----------------------------------------------------------------------
;; scoping

(defmacro flow-id-scope (&body body)
  `(let ((%flow-id %flow-id)
	 (flow-gen-func (lambda () (list (incf %flow-id)))))
     ,@body))

;;----------------------------------------------------------------------
;; construction

(defun flow-id! (&rest ids)
  (let ((ids (remove nil ids)))
    (labels ((internal-ids (x) (ids x)))
      (if (null ids)
	  (make-instance 'flow-identifier :ids (funcall flow-gen-func))
	  (make-instance 'flow-identifier
			 :ids (sort (copy-list (remove-duplicates
						(mapcat #'internal-ids ids)))
				    #'<))))))

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

;;----------------------------------------------------------------------
;; inspection

(defun id~= (id-a id-b)
  (unless (or (null id-a) (null id-b))
    (not (null (intersection (ids id-a) (ids id-b))))))

(defun id= (id-a id-b)
  (unless (or (null id-a) (null id-b))
    (equal (sort (copy-list (ids id-a)) #'<)
	   (sort (copy-list (ids id-b)) #'<))))
