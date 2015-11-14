(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defclass v-value ()
  ((type :initarg :type :initform nil :accessor v-type)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)
   (read-only :initarg :read-only :initform nil :reader v-read-only)
   (flow-ids :initarg :flow-ids :initform (error 'flow-id-must-be-specified-vv)
	     :reader flow-ids)))

(defmethod v-make-value ((type v-t-type) env
                         &key (glsl-name (free-name 'unspecified))
			   (flow-ids (flow-id!)) function-scope
			   read-only)
  (make-instance 'v-value :type type :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))
		 :flow-ids flow-ids :read-only read-only))

(defmethod v-make-value ((type t) env
                         &key (glsl-name (free-name 'unspecified))
			   (flow-ids (flow-id!)) function-scope
			   read-only)
  (make-instance 'v-value :type (type-spec->type type) :glsl-name glsl-name
                 :function-scope (or function-scope (v-function-scope env))
		 :flow-ids flow-ids :read-only read-only))

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
     :do (loop :for (name type-spec setable) :in vars :do
            (let ((type (type-spec->type type-spec)))
              (add-var name (v-make-value
			     type env :glsl-name (gen-reserved-var-string name)
			     :flow-ids (%gl-flow-id!) :read-only (not setable))
                       env))))
  env)
