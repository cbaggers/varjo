(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Varjo's root type

(define-v-type-class v-type ()
  ((core :initform nil :reader core-typep)
   (superclass :initform nil)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1 :reader v-glsl-size)
   (casts-to :initform nil)
   (flow-ids :initarg :flow-ids :initform nil :reader flow-ids)
   (ctv :initform nil :initarg :ctv :accessor ctv)
   (default-value :initarg :default-value)
   (qualifiers :initform nil :initarg :qualifiers :reader qualifiers)
   (tertiary-score :initform 0 :initarg :tertiary-score
                   :reader tertiary-score)))

;;------------------------------------------------------------
;; Core type methods

(defgeneric copy-type (type)
  (:documentation
   "This function returns a new instance of the provided type with the exact
same values in it's slots.

It is different from (type-spec->type (type->type-spec type)) in that it
handles compile/unrepresentable values and flow-ids correctly, which the
type-spec trick doesnt"))

(defmethod copy-type ((type v-type))
  (let* ((type-name (class-name (class-of type)))
         (new-inst (make-instance type-name
                                  :flow-ids (flow-ids type)
                                  :qualifiers (qualifiers type))))
    (setf (ctv new-inst) (ctv type))
    new-inst))

(defmethod qualify-type ((type v-type) qualifiers)
  (let ((type (copy-type type)))
    (setf (slot-value type 'qualifiers) qualifiers)
    type))

(defmethod type->type-spec ((type v-type))
  (let ((name (class-name (class-of type))))
    (or (car (rassoc name *type-shorthand*))
        name)))

(defmethod make-load-form ((type v-type) &optional environment)
  (declare (ignore environment))
  `(type-spec->type ',(type->type-spec type)))

(defmethod post-initialise ((object v-type)))

(defmethod initialize-instance :after ((type-obj v-type) &rest initargs)
  (declare (ignore initargs))
  (post-initialise type-obj))

(defmethod v-true-type ((object v-type))
  object)

(defun vtype-existsp (type-name)
  (let ((type-name (expand-keyword-type-spec-shorthand type-name)))
    (etypecase type-name
      (symbol
       (and type-name
            (find-class type-name nil)
            (values (subtypep type-name 'v-type))))
      (list (vtype-existsp (first type-name))))))

;;------------------------------------------------------------
;; Compilation Error
;;
;; Attached to failed compilation objects when delaying errors
;; see functions.lisp for use

(define-v-type-class v-error (v-type)
  ((payload :initform nil :initarg :payload :accessor v-payload)))

(defun v-errorp (obj)
  (typep obj 'v-error))

;;------------------------------------------------------------

(defun &rest-p (x)
  (and (symbolp x) (string= x :&rest)))
