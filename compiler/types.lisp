(in-package :varjo)

;;------------------------------------------------------------
;; Macro for defining varjo types

(defmacro def-v-type-class (name direct-superclass direct-slots &rest options)
  (let ((new-names (if (equal (package-name (symbol-package name)) "VARJO")
                       `(append (list ,(kwd (subseq (symbol-name name) 2))
                                      ',name)
                                *registered-types*)
                       `(cons ',name *registered-types*))))
    `(progn (defclass ,name ,direct-superclass ,direct-slots ,@options)
            (setf *registered-types* (remove-duplicates ,new-names))
            ',name)))

;;------------------------------------------------------------
;; Varjo's root type

(def-v-type-class v-type ()
  ((core :initform nil :reader core-typep)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1 :reader v-glsl-size)
   (casts-to :initform nil)
   (flow-ids :initarg :flow-ids :initform nil :reader flow-ids)))

;;------------------------------------------------------------
;; Core type methods

(defgeneric copy-type (type)
  (:documentation
   "This function returns a new instance of the provided type with the exact
same values in it's slots.

It is different from (type-spec->type (type->type-spec type)) in that it handles
compile time values and flow-ids correctly, which the type-spec trick doesnt"))

(defmethod copy-type ((type v-type))
  (let* ((type-name (class-name (class-of type))))
    (make-instance type-name :flow-ids (flow-ids type))))

(defmethod type->type-spec ((type v-type))
  (class-name (class-of type)))

(defmethod make-load-form ((type v-type) &optional environment)
  (declare (ignore environment))
  `(type-spec->type ',(type->type-spec type)))

(defmethod post-initialise ((object v-type)))

(defmethod initialize-instance :after ((type-obj v-type) &rest initargs)
  (declare (ignore initargs))
  (post-initialise type-obj))

(defmethod v-true-type ((object v-type))
  object)

;;------------------------------------------------------------
;; None

(def-v-type-class v-none (v-type) ())

(defun gen-none-type ()
  (type-spec->type :none))

;;------------------------------------------------------------
;; Void

(def-v-type-class v-void (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "void" :reader v-glsl-string)
   (glsl-size :initform :sizeless)))

;;------------------------------------------------------------
;; Compilation Error
;;
;; Attached to failed compilation objects when delaying errors
;; see functions.lisp for use

(def-v-type-class v-error (v-type)
  ((payload :initform nil :initarg :payload :accessor v-payload)))

(defun v-errorp (obj)
  (typep obj 'v-error))

;;------------------------------------------------------------
;; Compile Time Value
;;
;; The supertype for all types with CTVs

(def-v-type-class v-compile-time-value (v-type)
  ((ctv :initform nil :initarg :ctv :accessor ctv)))

(defmethod copy-type ((type v-compile-time-value))
  (let* ((type-name (class-name (class-of type)))
         (new-inst (make-instance type-name :flow-ids (flow-ids type))))
    (setf (ctv new-inst) (ctv type))
    new-inst))

;;------------------------------------------------------------
;; Container
;;
;; The supertype of all types that can have values stored into, and
;; retrieved from, themselves

(def-v-type-class v-container (v-type)
  ((element-type :initform t)
   (dimensions :initform nil :accessor v-dimensions)))

(defmethod post-initialise ((object v-container))
  (with-slots (dimensions element-type) object
    (setf dimensions (listify dimensions))
    (unless (or (typep element-type 'v-type) (eq element-type t))
      (setf element-type (type-spec->type element-type)))))

;;------------------------------------------------------------
;; Array

(def-v-type-class v-array (v-container)
  ((element-type :initform t :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)))

(defmethod v-glsl-size ((type v-array))
  (* (apply #'* (v-dimensions type))
     (slot-value (v-element-type type) 'glsl-size)))

(defmethod v-element-type ((object v-container))
  (let ((result (slot-value object 'element-type)))
    ;; {TODO} dedicated error
    (assert (typep result 'v-type) (object)
            "The element-type of ~a was ~a which is not an instance of a type."
            object result)
    result))

(defmethod copy-type ((type v-array))
  (let* ((new-inst (call-next-method)))
    (setf (v-dimensions new-inst) (v-dimensions type))
    new-inst))

(defmethod type->type-spec ((type v-array))
  (if (and (v-element-type type) (v-dimensions type))
      (list (type->type-spec (v-element-type type)) (v-dimensions type))
      'v-array))

(defmethod v-glsl-string ((object v-array))
  (format nil "~a ~~a~{[~a]~}" (v-glsl-string (v-element-type object))
          (mapcar (lambda (x)
                    (if (numberp x) x ""))
                  (v-dimensions object))))

;;------------------------------------------------------------
;; Sampler

(def-v-type-class v-sampler (v-type) ())

(defmethod post-initialise ((object v-sampler))
  (with-slots (element-type) object
    (unless (typep element-type 'v-type)
      (setf element-type (type-spec->type element-type)))))

(defmethod v-element-type ((object v-sampler))
  (let ((result (slot-value object 'element-type)))
    ;; {TODO} dedicated error
    (assert (typep result 'v-type) (object)
            "The element-type of ~a was ~a which is not an instance of a type."
            object result)
    result))

;;------------------------------------------------------------

(def-v-type-class v-any-one-of (v-compile-time-value)
  ((types :initform nil :initarg :types :reader v-types)))

;;------------------------------------------------------------
;; Struct
;;
;; Supertype of all structs

(def-v-type-class v-struct (v-type)
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (signature :initform nil :initarg :signature :accessor v-signature)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (slots :initform nil :initarg :slots :reader v-slots)))

;; Supertype of all structs that are not from the glsl spec
(def-v-type-class v-user-struct (v-struct) ())

;;------------------------------------------------------------
;; Function Type
;;
;; The type of all function objects

(def-v-type-class v-function-type (v-compile-time-value)
  ((argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)))

(defmethod print-object ((object v-function-type) stream)
  (with-slots (name argument-spec return-spec) object
    (format stream "#<V-FUNCTION-TYPE ~s -> ~s>"
            (if (eq t argument-spec)
                '(t*)
                (mapcar #'type-of argument-spec))
            (typecase (first return-spec)
              (function t)
              (v-type (type-of (first return-spec)))
              (otherwise return-spec)))))

(defun v-closure-p (type)
  (and (typep type 'v-function-type)
       (ctv type)
       (implicit-args (ctv type))))

(defmethod type->type-spec ((type v-function-type))
  (with-slots (argument-spec return-spec) type
    ;; {TODO} remove this, done now
    (assert (listp return-spec))
    (let* ((in (mapcar #'type->type-spec argument-spec))
           (out (if (= (length return-spec) 1)
                    (type->type-spec (first return-spec))
                    (mapcar #'type->type-spec return-spec))))
      `(function ,in ,out))))

;;------------------------------------------------------------
;; Stemcell

(def-v-type-class v-stemcell (v-type) ())

;;------------------------------------------------------------
;; Converting specs into types

(defun try-type-spec->type (spec flow-id)
  (let ((spec (cond ((keywordp spec) (p-symb 'varjo 'v- spec))
                    ((and (listp spec) (keywordp (first spec)))
                     (cons (p-symb 'varjo 'v- (first spec)) (rest spec)))
                    (t spec))))
    (cond ((null spec) nil)
          ((and (symbolp spec) (vtype-existsp spec))
           (let ((type (make-instance spec :flow-ids flow-id)))
             (when (typep type 'v-type)
               type)))
          ((and (listp spec) (eq (first spec) 'function))
           (make-instance
            'v-function-type :arg-spec (mapcar #'type-spec->type (second spec))
            :return-spec (mapcar #'type-spec->type
                                 (uiop:ensure-list (third spec)))
            :flow-ids flow-id))
          ((and (listp spec) (vtype-existsp (first spec)))
           (destructuring-bind (type dimensions) spec
             (make-instance 'v-array :element-type (if (keywordp type)
                                                       (symb 'v- type)
                                                       type)
                            :dimensions dimensions
                            :flow-ids flow-id)))
          (t nil))))

;; shouldnt the un-shadow be in try-type-spec->type?
(defun type-spec->type (spec &optional flow-id)
  (v-true-type
   (or (try-type-spec->type (un-shadow spec) flow-id)
       (error 'unknown-type-spec :type-spec spec))))

;; A probably too forgiving version of type-spec->type, it is a no-op
;; on v-types
(defun as-v-type (thing)
  (etypecase thing
    (v-type thing)
    (symbol (type-spec->type thing))))

;;------------------------------------------------------------
;; Valid type spec predicate

(defun type-specp (spec)
  (handler-case (and (type-spec->type spec) t)
    (unknown-type-spec (e)
      (declare (ignore e))
      nil)))

;;------------------------------------------------------------
;; Type shadowing

(let* ((shadow-ht (or (when (boundp '*type-shadow*) (symbol-value '*type-shadow*))
                      (make-hash-table)))
       (shadow-ht-backward
        (let ((ht (make-hash-table)))
          (maphash #'(lambda (k v) (setf (gethash v ht) k))
                   shadow-ht)
          ht)))
  (defun add-type-shadow (type shadowing-this-type)
    (assert (and (symbolp type) (symbolp shadowing-this-type)))
    (setf (gethash type shadow-ht) shadowing-this-type)
    (setf (gethash shadowing-this-type shadow-ht-backward) type))
  (defun un-shadow (spec)
    (if (listp spec)
        `(,(or (gethash (first spec) shadow-ht) (first spec)) ,@(rest spec))
        (or (gethash spec shadow-ht) spec)))
  (defun reverse-shadow-lookup (shadowed-type-spec)
    (if (listp shadowed-type-spec)
        `(,(or (gethash (first shadowed-type-spec) shadow-ht-backward)
               (first shadowed-type-spec))
           ,@(rest shadowed-type-spec))
        (or (gethash shadowed-type-spec shadow-ht-backward)
            shadowed-type-spec))))

;;------------------------------------------------------------
;; Type Equality

;; Type <-> Type

(defmethod v-type-eq ((a v-type) (b v-type) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec b)))

;; Type <-> Spec

(defmethod v-type-eq ((a v-type) (b symbol) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec (type-spec->type b))))

(defmethod v-type-eq ((a v-type) (b list) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec (type-spec->type b))))

;; Void type {TODO} redundant now void is v-type?
;;
(defmethod v-type-eq ((a v-void) (b v-void) &optional (env *global-env*))
  (declare (ignore env))
  t)

(defmethod v-type-eq (a (b v-void) &optional (env *global-env*))
  (declare (ignore env))
  nil)

(defmethod v-type-eq ((a v-void) b &optional (env *global-env*))
  (declare (ignore env))
  nil)

;; None type {TODO} redundant now none is v-type?
;;
(defmethod v-type-eq ((a v-none) (b v-none) &optional (env *global-env*))
  (declare (ignore env))
  t)

(defmethod v-type-eq (a (b v-none) &optional (env *global-env*))
  (declare (ignore env))
  nil)

(defmethod v-type-eq ((a v-none) b &optional (env *global-env*))
  (declare (ignore env))
  nil)

;;------------------------------------------------------------
;; Type predicate

(defmethod v-typep ((a t) (b v-none) &optional (env *global-env*))
  (declare (ignore env))
  (typep a (type-of b)))

(defmethod v-typep ((a v-type) (b v-type) &optional (env *global-env*))
  (declare (ignore env))
  (typep a (type-of b)))

(defmethod v-typep ((a v-type) (b v-type) &optional (env *global-env*))
  (v-typep a (type->type-spec b) env))

(defmethod v-typep ((a v-type) b &optional (env *global-env*))
  (declare (ignore env))
  (cond ((symbolp b)
         (typep a (un-shadow b)))
        ((and (listp b) (numberp (second b)))
         (typep a (un-shadow (first b))))))

(defmethod v-typep ((a null) b &optional (env *global-env*))
  (declare (ignore env a b))
  nil)
(defmethod v-typep (a (b null) &optional (env *global-env*))
  (declare (ignore env a b))
  nil)
(defmethod v-typep ((a v-stemcell) b &optional (env *global-env*))
  (declare (ignore env a b))
  t)

;;------------------------------------------------------------
;; Casting

(defmethod v-casts-to ((from-type v-function-type) (to-type v-function-type) env)
  (when (and (every #'v-type-eq (v-argument-spec from-type)
                    (v-argument-spec to-type))
             (every #'v-type-eq (v-return-spec from-type)
                    (v-return-spec to-type)))
    to-type))

(defmethod v-casts-to ((from-type v-any-one-of) (to-type v-function-type) env)
  (let* ((funcs (mapcar (lambda (fn)
                          (when (v-casts-to (v-type-of fn) to-type env)
                            fn))
                        (functions (ctv from-type))))
         (funcs (remove nil funcs))
         (f-set (make-instance 'v-function-set :functions funcs)))
    (when funcs (v-type-of f-set))))

(defmethod v-casts-to ((from-type v-stemcell) (to-type v-type) env)
  (declare (ignore env from-type))
  to-type)

(defmethod v-casts-to-p (from-type to-type env)
  (not (null (v-casts-to from-type to-type env))))

;;[TODO] vtypep here?
(defmethod v-casts-to ((from-type v-type) (to-type v-type) env)
  (if (v-typep from-type to-type)
      (strip-flow-id from-type)
      (when (slot-exists-p from-type 'casts-to)
        (loop :for cast-type :in (slot-value from-type 'casts-to)
           :if (v-typep (type-spec->type cast-type) to-type env)
           :return (type-spec->type cast-type)))))


(defun find-mutual-cast-type (&rest types)
  (let ((names (loop :for type :in types
                  :collect (if (typep type 'v-type)
                               (type->type-spec type)
                               type))))
    (if (loop :for name :in names :always (eq name (first names)))
        (type-spec->type (first names))
        (let* ((all-casts (sort (loop :for type :in types :for name :in names :collect
                                   (cons name
                                         (if (symbolp type)
                                             (slot-value (type-spec->type type)
                                                         'casts-to)
                                             (slot-value type 'casts-to))))
                                #'> :key #'length))
               (master (first all-casts))
               (rest-casts (rest all-casts)))
          (type-spec->type
           (first (sort (loop :for type :in master
                           :if (loop :for casts :in rest-casts
                                  :always (find type casts))
                           :collect type) #'> :key #'v-superior-score)))))))

(let ((order-or-superiority '(v-double v-float v-int v-uint v-vec2 v-ivec2
                              v-uvec2 v-vec3 v-ivec3 v-uvec3 v-vec4 v-ivec4
                              v-uvec4 v-mat2 v-mat2x2 v-mat3 v-mat3x3 v-mat4
                              v-mat4x4)))
  (defun v-superior-score (type)
    (or (position type order-or-superiority) -1))
  (defun v-superior (x y)
    (< (or (position x order-or-superiority) -1)
       (or (position y order-or-superiority) -1))))

(defun v-superior-type (&rest types)
  (first (sort types #'v-superior)))

;;------------------------------------------------------------
;; Finding similarly named types
;;
;; used in errors.lisp

(defun find-alternative-types-for-spec (type-spec)
  (when (symbolp type-spec)
    (let ((sn (symbol-name type-spec)))
      (append
       (remove-if-not
        (lambda (x)
          (let ((x (symbol-name x)))
            (or (string= sn x)
                (> (vas-string-metrics:jaro-winkler-distance sn x) 0.9))))
        *registered-types*)))))
