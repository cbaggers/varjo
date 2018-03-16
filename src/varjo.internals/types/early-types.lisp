(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Varjo's root type

(define-v-type-class v-type ()
  ((core :initform nil :reader core-typep)
   (superclass :initform nil)
   (glsl-string :initform "£-vtype-£" :reader v-glsl-string)
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

;;------------------------------------------------------------
;; Type shadowing

(defvar *alternate-ht* (make-hash-table))
(defvar *alternate-ht-backward* (make-hash-table))

(defun resolve-name-from-alternative (spec)
  (if (listp spec)
      `(,(or (gethash (first spec) *alternate-ht*) (first spec)) ,@(rest spec))
      (or (gethash spec *alternate-ht*) spec)))

(defun alternate-name-for (type-spec)
  (if (listp type-spec)
      `(,(or (gethash (first type-spec) *alternate-ht-backward*)
             (first type-spec))
         ,@(rest type-spec))
      (or (gethash type-spec *alternate-ht-backward*)
          type-spec)))

;;------------------------------------------------------------
;; Converting specs into types

(defun expand-keyword-type-spec-shorthand (spec)
  (or (when (keywordp spec)
        (cdr (assoc spec *type-shorthand*)))
      spec))

(defun try-type-spec->type (spec flow-id)
  (flet ((array-shorthand-spec-p (spec)
           (labels ((valid-dim-p (x)
                      (or (typep x 'unsigned-byte)
                          (and (symbolp x) (string= x "*")))))
             (and ;; (listp spec) << already checked below
                  (= (length spec) 2)
                  (or (valid-dim-p (second spec))
                      (and (listp (second spec))
                           (every #'valid-dim-p (second spec))))
                  (vtype-existsp (first spec))))))
    (let ((spec (expand-keyword-type-spec-shorthand spec)))
      (if (listp spec)
          (cond
            ((and (eq (first spec) 'function))
             (try-type-spec->type `(v-function-type ,@(rest spec)) flow-id))
            ;;
            ((and (eq (first spec) 'or))
             (try-type-spec->type `(v-or ,@(rest spec)) flow-id))
            ;;
            ((array-shorthand-spec-p spec)
             (try-type-spec->type `(v-array ,@spec) flow-id))
            ;;
            ((and (type-name-known (first spec)))
             (apply #'v-make-type
                    (allocate-instance (find-class (first spec)))
                    flow-id
                    (rest spec)))
            (t nil))
          (cond ((null spec) nil)
                ;;
                ((eq spec t) (make-instance 'v-type))
                ;;
                ((and (symbolp spec) (type-name-known spec))
                 (make-instance spec :flow-ids flow-id))
                ;;
                ;;
                (t nil))))))

;; shouldnt the resolve-name-from-alternative be in try-type-spec->type?
(defmethod type-spec->type (spec &optional flow-id)
  (or (try-type-spec->type (resolve-name-from-alternative spec) flow-id)
      (error 'unknown-type-spec :type-spec spec)))

(define-compiler-macro type-spec->type (&whole whole spec &optional flow-id)
  (if flow-id
      whole
      (let ((type (try-type-spec->type spec nil)))
        (or type whole))))

(defun type-specp (spec)
  (not (null (try-type-spec->type (resolve-name-from-alternative spec) nil))))

;;------------------------------------------------------------
