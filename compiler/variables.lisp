(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defclass v-value ()
  ((type :initarg :type :initform nil :accessor v-type)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)
   (flow-id :initarg :flow-id :initform (error 'flow-id-must-be-specified-vv)
	    :reader flow-id)))

(let ((flow-id -1))
  (defun gen-flow-id () (incf flow-id)))

(defmethod v-make-value ((type v-t-type) env
                         &optional (glsl-name (free-name 'unspecified))
			   (flow-id (gen-flow-id)) function-scope)
  (make-instance 'v-value :type type :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))
		 :flow-id flow-id))

(defmethod v-make-value ((type t) env
                         &optional (glsl-name (free-name 'unspecified))
			   (flow-id (gen-flow-id)) function-scope)
  (make-instance 'v-value :type (type-spec->type type) :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))
		 :flow-id flow-id))

(defun v-value-equal (a b)
  (equal (v-glsl-name a) (v-glsl-name b)))

;;[TODO] this smells a bit, it is only used for glsl strings, and we should
;;       rename this to that end
(let ((num 0))
  (defun free-name (name &optional env counter)
    (declare (ignore env))
    (when counter (setf num counter))
    (if (valid-user-defined-name name)
        (progn (incf num) (symb name '- num 'v))
        (error 'name-unsuitable :name name))))

(defun valid-user-defined-name (name-symbol)
  (not (glsl-var-namep name-symbol)))

(defun glsl-var-namep (name-symbol)
  (let ((name (symbol-name name-symbol)))
    (or (when (> (length name) 2) (equal "GL-" (subseq name 0 3)))
        (when (> (length name) 2) (equal "FK-" (subseq name 0 3)))
        (when (> (length name) 3) (equal "-SC-" (subseq name 0 4))))))

(defun add-glsl-vars (env source)
  (loop :for (restrict . vars) :in source
     :if (or (equal restrict t)
             (context-ok-given-restriction (v-context env) (listify restrict)))
     :do (loop :for (name type-spec place) :in vars :do
            (let ((type (type-spec->type type-spec)))
              (when place (setf (v-placep type) t))
              (add-var name (v-make-value type env (gen-reserved-var-string name)
					  (- (gen-flow-id)))
                       env t))))
  env)
