(in-package :varjo)
(in-readtable :fn.reader)
;;------------------------------------------------------------
;; Macro for defining varjo types

;; {TODO} proper errors
(defmacro def-v-type-class (name direct-superclass direct-slots &rest options)
  (unless (eq name 'v-type)
    (assert (and (listp direct-superclass)
                 (symbolp (first direct-superclass))
                 (or (= (length direct-superclass) 1)
                     (and (= (length direct-superclass) 2)
                          (find 'v-ephemeral-type direct-superclass))))
            ()
            "Varjo: All types must specify one superclass, this will usually be v-type"))
  ;;
  (let ((new-names (if (and (equal (package-name (symbol-package name)) "VARJO")
                            (equal (subseq (symbol-name name) 2) "V-")
                            (not (member name '(v-type))))
                       `(append (list ,(kwd (subseq (symbol-name name) 2))
                                      ',name)
                                *registered-types*)
                       `(cons ',name *registered-types*))))
    `(progn
       (defclass ,name ,direct-superclass
         ,(if (eq name 'v-type)
              direct-slots
              (cons `(superclass :initform ',(first direct-superclass))
                    direct-slots))
         ,@options)
       (setf *registered-types* (remove-duplicates ,new-names))
       ',name)))

;;------------------------------------------------------------
;; Varjo's root type

(def-v-type-class v-type ()
  ((core :initform nil :reader core-typep)
   (superclass :initform nil)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1 :reader v-glsl-size)
   (casts-to :initform nil)
   (flow-ids :initarg :flow-ids :initform nil :reader flow-ids)
   (ctv :initform nil :initarg :ctv :accessor ctv)
   (default-value :initarg :default-value)
   (qualifiers :initform nil :initarg :qualifiers :reader qualifiers)))

(defmethod v-superclass ((type v-type))
  (with-slots (superclass) type
    (when superclass
      (type-spec->type superclass))))

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
;; Shadow Type
;;
;; The supertype for all types which that are shadowing a core
;; glsl type.

(def-v-type-class v-shadow-type (v-type)
  ((shadowed-type :initform nil :reader shadowed-type)))

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
    (unless (typep element-type 'v-type)
      (setf element-type (type-spec->type element-type)))))

(defmethod v-dimensions (object)
  (error 'doesnt-have-dimensions :vtype object))

(defmethod v-element-type ((object v-container))
  (let ((result (slot-value object 'element-type)))
    ;; {TODO} dedicated error
    (assert (typep result 'v-type) (object)
            "The element-type of ~a was ~a which is not an instance of a type."
            object result)
    result))

;;------------------------------------------------------------
;; Array

(def-v-type-class v-array (v-container)
  ((element-type :initform t :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)))

(defmethod v-make-type ((type v-array) flow-id &rest args)
  (destructuring-bind (element-type length) args
    (initialize-instance
     type
     :element-type (type-spec->type element-type (when flow-id (flow-id!)))
     :dimensions (listify length)
     :flow-ids flow-id)))

(defun v-array-spec-p (spec)
  (labels ((valid-dim-p (x)
             (or (typep x '(integer 0 #xFFFF))
                 (and (symbolp x) (string= x :*)))))
    (and (listp spec)
         (= (length spec) 2)
         (or (valid-dim-p (second spec))
             (and (listp (second spec))
                  (every #'valid-dim-p (second spec))))
         (vtype-existsp (first spec)))))

(defmethod v-glsl-size ((type v-array))
  (* (apply #'* (v-dimensions type))
     (slot-value (v-element-type type) 'glsl-size)))

(defmethod post-initialise ((object v-array))
  (labels ((valid-size-p (l)
             (or (and (integerp l) (>= l 0))
                 (eq l '*))))
    (with-slots (dimensions element-type) object
      (let ((dim (listify dimensions)))
        (assert (<= (length dim) 1) (dim)
                'multi-dimensional-array :dimensions dim)
        (assert (every #'valid-size-p dim))
        (setf dimensions dim))
      (unless (typep element-type 'v-type)
        (setf element-type (type-spec->type element-type))))))

(defmethod copy-type ((type v-array))
  (make-instance 'v-array
                 :ctv (ctv type)
                 :flow-ids (flow-ids type)
                 :qualifiers (qualifiers type)
                 :dimensions (v-dimensions type)
                 :element-type (v-element-type type)))

(defmethod type->type-spec ((type v-array))
  (if (and (v-element-type type) (v-dimensions type))
      (list (type->type-spec (v-element-type type)) (v-dimensions type))
      'v-array))

(defmethod v-glsl-string ((object v-array))
  (labels ((dims (x)
             (append (v-dimensions x)
                     (when (typep (v-element-type x) 'v-array)
                       (dims (v-element-type x)))))
           (root-type (x)
             (if (typep (v-element-type x) 'v-array)
                 (root-type (v-element-type x))
                 (v-element-type x))))
    ;;
    (let ((dims (mapcar λ(if (eq _ '*) "" _)
                        (dims object)))
          ;; The reason for this ↑↑ logic is that we want
          ;; * to become []  not [*]
          (elem-type (root-type object)))
      (format nil "~a~{[~a]~}" (v-glsl-string elem-type) dims))))

(defmethod v-array-type-of ((element-type v-type) dimensions flow-id)
  (let ((dimensions (ensure-list dimensions)))
    (make-instance 'v-array :dimensions dimensions
                   :element-type element-type
                   :flow-ids flow-id)))

(defmethod v-typep ((a v-array) (b v-array) &optional (env *global-env*))
  (declare (ignore env))
  (v-typep (v-element-type a) (v-element-type b)))

;;------------------------------------------------------------
;; Sampler

(def-v-type-class v-sampler (v-type)
  ((element-type :initform 'v-type)))

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
;; Ephemeral Values
;;
;; The supertype for all types which that have no representation
;; in glsl.
;;
;; Make sure you define these using deftype
;;

(def-v-type-class v-ephemeral-type (v-type) ())

(defgeneric ephemeral-p (x)
  (:method ((x v-type))
    (typep x 'v-ephemeral-type))
  (:method (x)
    (ephemeral-p (v-type-of x))))

;;------------------------------------------------------------
;; Ephemeral Array
;;
;; An array for item which that have no representation in glsl.
;;

(def-v-type-class v-ephemeral-array (v-array v-ephemeral-type) ())

(defmethod v-make-type ((type v-ephemeral-array) flow-id &rest args)
  (destructuring-bind (element-type length) args
    (assert (or (and (numberp length) (typep length '(integer 0 #xFFFF)))
                (and (symbolp length) (string= length :*))))
    (initialize-instance type
                         :element-type (type-spec->type element-type)
                         :dimensions (listify length)
                         :flow-ids flow-id)))

;;------------------------------------------------------------
;; Unrepresentable Value
;;
;; The supertype for all types which are not representable in any
;; way in glsl. First class functions is the classic example of this.
;;
;; {TODO} I think this should be a field on ephemeral-types. We then
;;        have func spicing ephemerals and regular.

(def-v-type-class v-unrepresentable-value (v-ephemeral-type) ())

;;------------------------------------------------------------
;; Block Array
;;
;; These serve a very specific purpose; they are used when you have an
;; array'd interface block but you want to pretend that that members of
;; the block are array'd.
;;
;; This is different from ephemeral arrays where you are mimicking a true
;; array.
;;

(def-v-type-class v-block-array (v-ephemeral-type)
  ((element-type :initform t :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)
   (block-name :initarg :block-name :initform "<invalid>" :reader block-name)))

(defmethod v-make-type ((type v-block-array) flow-id &rest args)
  (destructuring-bind (block-name element-type length) args
    (let* ((dims (listify length))
           (length (first dims)))
      (assert (= 1 (length dims)))
      (assert (or (and (numberp length) (typep length '(integer 0 #xFFFF)))
                  (and (symbolp length) (string= length :*))))
      (initialize-instance type
                           :block-name block-name
                           :element-type (type-spec->type element-type)
                           :dimensions dims
                           :flow-ids flow-id))))

(defmethod post-initialise ((object v-block-array))
  (with-slots (dimensions element-type) object
    (setf dimensions (listify dimensions))
    (unless (typep element-type 'v-type)
      (setf element-type (type-spec->type element-type))))

  (defmethod v-element-type ((object v-block-array))
    (let ((result (slot-value object 'element-type)))
      ;; {TODO} dedicated error
      (assert (typep result 'v-type) (object)
              "The element-type of ~a was ~a which is not an instance of a type."
              object result)
      result)))

(defmethod type->type-spec ((type v-block-array))
  `(v-block-array ,(if (slot-boundp type 'block-name)
                       (block-name type)
                       "<invalid>")
                  ,(type->type-spec (v-element-type type))
                  ,(v-dimensions type)))

(defmethod copy-type ((type v-block-array))
  (make-instance 'v-block-array
                 :block-name (block-name type)
                 :dimensions (v-dimensions type)
                 :element-type (v-element-type type)))

(defun make-into-block-array (array-type block-name)
  (assert (v-typep array-type 'v-array))
  (let* ((dim (v-dimensions array-type))
         (r (make-instance 'v-block-array
                           :block-name block-name
                           :dimensions dim
                           :element-type (v-element-type array-type)
                           :ctv (ctv array-type)
                           :flow-ids (flow-ids array-type))))
    (when (slot-boundp array-type 'default-value)
      (setf (slot-value r 'default-value)
            (slot-value array-type 'default-value)))
    r))

(defun block-array-to-regular-array (block-array)
  (assert (v-typep block-array 'v-block-array))
  (let ((r (make-instance 'v-array
                          :dimensions (v-dimensions block-array)
                          :element-type (v-element-type block-array)
                          :ctv (ctv block-array)
                          :flow-ids (flow-ids block-array))))
    (when (slot-boundp block-array 'default-value)
      (setf (slot-value r 'default-value)
            (slot-value block-array 'default-value)))
    r))

(defmethod v-glsl-string ((object v-block-array))
  (v-glsl-string (v-element-type object)))

;;------------------------------------------------------------
;; Or

(def-v-type-class v-or (v-type)
  ((types :initform nil :initarg :types :reader v-types)))

(defmethod copy-type ((type v-or))
  (assert (null (ctv type)))
  (make-instance 'v-or :types (v-types type) :flow-ids (flow-ids type)))

(defmethod type->type-spec ((type v-or))
  `(or ,@(mapcar #'type->type-spec (v-types type))))

(defmethod v-make-type ((type v-or) flow-id &rest args)
  (declare (ignore flow-id))
  ;; flow-id is discarded as the flow-id will be the union of the
  ;; ids of the types in the spec
  (gen-or-type args))

(defgeneric gen-or-type (types)
  (:method (types)
    (let* ((types (mapcar (lambda (type)
                            (etypecase type
                              (v-type type)
                              ((or list symbol)
                               (type-spec->type type (flow-id!)))))
                          types))
           (reduced (reduce-types-for-or-type types)))
      (if (= (length reduced) 1)
          (first reduced)
          (make-instance 'v-or :types reduced
                         :flow-ids (apply #'flow-id! reduced))))))

(defgeneric reduce-types-for-or-type (types)
  (:method (types)
    (labels ((inner (type)
               (if (typep type 'v-or)
                   (mappend #'inner (v-types type))
                   (list type))))
      (remove-duplicates (mappend #'inner types) :test #'v-type-eq))))

(defmethod print-object ((obj v-or) stream)
  (format stream "#<OR ~{~s~^ ~}>" (mapcar #'type->type-spec (v-types obj))))

;;------------------------------------------------------------
;; Any one of
;;
;; This value differs from v-or' becaume v-or means it is one
;; of the contained types, whereas this means it's represents
;; all of the values and the compiler is free to pick any which
;; satisfies it's needs

(def-v-type-class v-any-one-of (v-unrepresentable-value)
  ((types :initform nil :initarg :types :reader v-types)))

(defmethod copy-type ((type v-any-one-of))
  (assert (null (ctv type)))
  (make-instance 'v-any-one-of :types (v-types type) :flow-ids (flow-ids type)))

(defmethod type->type-spec ((type v-any-one-of))
  `(any-one-of ,@(mapcar #'type->type-spec (v-types type))))

(defgeneric gen-any-one-of-type (types)
  (:method (types)
    (let* ((types (mapcar (lambda (type)
                            (etypecase type
                              (v-type
                               (if (flow-ids type)
                                   type
                                   (set-flow-id type (flow-id!))))
                              ((or list symbol)
                               (type-spec->type type (flow-id!)))))
                          types))
           (reduced (reduce-types-for-or-type types)))
      (if (= (length reduced) 1)
          (first reduced)
          (make-instance 'v-any-one-of :types reduced
                         :flow-ids (apply #'flow-id! reduced))))))

(defmethod print-object ((obj v-any-one-of) stream)
  (format stream "#<ANY-ONE-OF ~{~s~^ ~}>"
          (mapcar #'type->type-spec (v-types obj))))

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

(def-v-type-class v-function-type (v-unrepresentable-value)
  ((argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)))

(defmethod v-make-type ((type v-function-type) flow-id &rest args)
  (destructuring-bind (arg-types return-type) args
    (make-instance
     'v-function-type :arg-spec (mapcar #'type-spec->type arg-types)
     :return-spec (make-type-set*
                   (mapcar #'type-spec->type
                           (uiop:ensure-list return-type)))
     :flow-ids flow-id)))

(defun %print-func-type-common (stream header-string argument-spec return-spec
                                &optional name)
  (labels ((extract (x)
             (etypecase x
               (v-type (type->type-spec x))
               (ret-gen-superior-type :mutual-cast)
               (ret-gen-nth-arg-type
                (type->type-spec
                 (elt argument-spec (arg-num x))))
               (ret-gen-element-of-nth-arg-type
                (type->type-spec
                 (v-element-type
                  (elt argument-spec (arg-num x))))))))
    (format stream "#<~a~@[ ~s~] ~s -> ~s>"
            header-string
            name
            (if (eq t argument-spec)
                '(t*)
                (mapcar #'type->type-spec argument-spec))
            (if (vectorp return-spec)
                (map 'list #'extract return-spec)
                return-spec))))

(defmethod print-object ((object v-function-type) stream)
  (with-slots (name argument-spec return-spec) object
    (%print-func-type-common
     stream "V-FUNCTION-TYPE" argument-spec return-spec)))

(defmethod copy-type ((type v-function-type))
  (let* ((new-inst (call-next-method)))
    (setf (v-argument-spec new-inst) (v-argument-spec type))
    (setf (v-return-spec new-inst) (v-return-spec type))
    new-inst))

(defun v-closure-p (type)
  (and (typep type 'v-function-type)
       (ctv type)
       (implicit-args (ctv type))))

(defmethod type->type-spec ((type return-type-generator))
  type)

(defmethod type->type-spec ((type v-function-type))
  (with-slots (argument-spec return-spec) type
    ;; {TODO} remove this, done now
    (assert (valid-func-return-spec-p return-spec))
    (let* ((in (if (eq argument-spec t)
                   argument-spec ;; happens in special ops
                   (mapcar #'type->type-spec argument-spec)))
           (out (cond
                  ((functionp return-spec) return-spec)
                  ((= (length return-spec) 1)
                   (type->type-spec (elt return-spec 0)))
                  (t (map 'list #'type->type-spec return-spec)))))
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
          ;;
          ((eq spec t) (type-spec->type 'v-type))
          ;;
          ((and (symbolp spec) (vtype-existsp spec))
           (make-instance spec :flow-ids flow-id))
          ;;
          ;;
          ((and (listp spec) (eq (first spec) 'function))
           (try-type-spec->type `(v-function-type ,@(rest spec)) flow-id))
          ;;
          ((and (listp spec) (eq (first spec) 'or))
           (try-type-spec->type `(v-or ,@(rest spec)) flow-id))
          ;;
          ((v-array-spec-p spec)
           (try-type-spec->type `(v-array ,@spec) flow-id))
          ;;
          ((and (listp spec) (vtype-existsp (first spec)))
           (apply #'v-make-type
                  (allocate-instance (find-class (first spec)))
                  flow-id
                  (rest spec)))
          (t nil))))

;; shouldnt the resolve-name-from-alternative be in try-type-spec->type?
(defun type-spec->type (spec &optional flow-id)
  (v-true-type
   (or (try-type-spec->type (resolve-name-from-alternative spec) flow-id)
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

(let* ((alternate-ht (make-hash-table))
       (alternate-ht-backward (make-hash-table)))
  (defun add-alternate-type-name (alt-type-name src-type-name)
    (assert (and (symbolp src-type-name) (symbolp alt-type-name)))
    (setf (gethash alt-type-name alternate-ht) src-type-name)
    (setf (gethash src-type-name alternate-ht-backward) alt-type-name)
    (when (and (ephemeral-p (type-spec->type src-type-name))
               (not (get-form-binding alt-type-name *global-env*)))
      (add-alt-ephemeral-constructor-function src-type-name alt-type-name))
    alt-type-name)
  (defun resolve-name-from-alternative (spec)
    (if (listp spec)
        `(,(or (gethash (first spec) alternate-ht) (first spec)) ,@(rest spec))
        (or (gethash spec alternate-ht) spec)))
  (defun alternate-name-for (type-spec)
    (if (listp type-spec)
        `(,(or (gethash (first type-spec) alternate-ht-backward)
               (first type-spec))
           ,@(rest type-spec))
        (or (gethash type-spec alternate-ht-backward)
            type-spec))))

;;------------------------------------------------------------
;; Type Equality

;; Type <-> Type

(defmethod v-type-eq ((a v-type) (b v-type) &optional (env *global-env*))
  (declare (ignore env))
  (and (equal (type->type-spec a) (type->type-spec b))
       (eq (ctv a) (ctv b))))

;; Type <-> Spec

(defmethod v-type-eq ((a v-type) (b symbol) &optional (env *global-env*))
  (declare (ignore env))
  (v-type-eq a (type-spec->type b)))

(defmethod v-type-eq ((a v-type) (b list) &optional (env *global-env*))
  (declare (ignore env))
  (v-type-eq a (type-spec->type b)))

;; Function-Type <-> Function-Type
;;

;; (defmethod v-type-eq ((a v-function-type) (b v-function-type)
;;                       &optional (env *global-env*))
;;   (declare (ignore env))
;;   (if (or (ctv a) (ctv b))
;;       (eq (ctv a) (ctv b))
;;       (equal (type->type-spec a) (type->type-spec b))))

;; (defmethod v-type-eq ((a v-function-type) (b v-type)
;;                       &optional (env *global-env*))
;;   (declare (ignore a b env))
;;   nil)

;; (defmethod v-type-eq ((a v-type) (b v-function-type)
;;                       &optional (env *global-env*))
;;   (declare (ignore a b env))
;;   nil)

;;------------------------------------------------------------
;; Type predicate

(defmethod v-typep ((a v-type) (b symbol) &optional (env *global-env*))
  (declare (ignore env))
  (v-typep a (type-spec->type (resolve-name-from-alternative b))))

(defmethod v-typep ((a v-type) (b list) &optional (env *global-env*))
  (declare (ignore env))
  (let ((b (resolve-name-from-alternative
            (mapcar #'resolve-name-from-alternative b))))
    (v-typep a (type-spec->type b))))

(defmethod v-typep ((a v-type) (b v-type) &optional (env *global-env*))
  (declare (ignore env))
  (typep a (type-of b)))

(defmethod v-typep ((a null) b &optional (env *global-env*))
  (declare (ignore env a b))
  nil)

(defmethod v-typep (a (b null) &optional (env *global-env*))
  (declare (ignore env a b))
  nil)

(defmethod v-typep ((a v-stemcell) b &optional (env *global-env*))
  (declare (ignore env a b))
  t)

(defmacro v-typecase (varjo-form &body cases)
  (alexandria:with-gensyms (type)
    (when cases
      `(let ((,type (v-type-of ,varjo-form)))
         (cond
           ,@(loop :for (type-name . body) :in cases :collect
                `((v-type-eq ,type ',type-name) ,@body)))))))

(defmacro v-etypecase (varjo-form &body cases)
  (alexandria:with-gensyms (type)
    (when cases
      `(let ((,type (v-type-of ,varjo-form)))
         (cond
           ,@(loop :for (type-name . body) :in cases :collect
                `((v-type-eq ,type ',type-name) ,@body))
           (t (error 'fell-through-v-typecase
                     :vtype ,type :wanted ',(mapcar #'first cases))))))))

;;------------------------------------------------------------
;; Casting


(defmethod v-casts-to ((from-type v-function-type) (to-type v-function-type)
                       env)
  (declare (ignore env))
  (when (and (every #'v-type-eq (v-argument-spec from-type)
                    (v-argument-spec to-type))
             (every #'v-type-eq (v-return-spec from-type)
                    (v-return-spec to-type)))
    to-type))

(defmethod v-casts-to ((from-type v-any-one-of) (to-type v-function-type) env)
  (let* ((funcs (remove-if-not (lambda (fn) (v-casts-to fn to-type env))
                               (v-types from-type))))
    (when funcs
      (gen-any-one-of-type funcs))))

(defmethod v-casts-to ((from-type v-stemcell) (to-type v-type) env)
  (declare (ignore env from-type))
  to-type)

(defmethod v-casts-to-p (from-type to-type env)
  (not (null (v-casts-to from-type to-type env))))

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
    (if (loop :for name :in names :always (equal name (first names)))
        (when (first names) (type-spec->type (first names)))
        (let* ((all-casts (sort (loop :for type :in types :for name :in names :collect
                                   (cons name
                                         (if (symbolp type)
                                             (slot-value (type-spec->type type)
                                                         'casts-to)
                                             (slot-value type 'casts-to))))
                                #'> :key #'length))
               (master (first all-casts))
               (rest-casts (rest all-casts))
               (result (first (sort (loop :for type :in master
                                       :if (loop :for casts :in rest-casts
                                              :always (find type casts))
                                       :collect type) #'> :key #'v-superior-score))))
          (when result (type-spec->type result))))))

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
;; Distance

;; {TODO} proper errors
(defgeneric get-type-distance (ancestor type &optional value-in-place-of-error)
  (:method ((ancestor v-type) (type v-type)
            &optional (value-in-place-of-error nil vset))
    (labels ((inner (base child accum)
               (cond
                 ((null child) (if vset
                                   value-in-place-of-error
                                   (error "Varjo: ~a is not a descendant of type ~a"
                                          type ancestor)))
                 ((v-type-eq base child) accum)
                 (t (inner base (v-superclass child) (1+ accum))))))
      (inner ancestor type 0))))

;;------------------------------------------------------------
;; Finding similarly named types
;;
;; used in errors.lisp

(defun find-alternative-types-for-spec (type-spec)
  (find-similarly-named-symbol type-spec *registered-types*))

;;------------------------------------------------------------

(defun make-type-set* (members)
  (when (and (= (length members) 1)
             (not (typep (first members) 'v-stemcell)))
    (assert (not (v-typep (first members) :void))))
  (apply #'vector members))

(defun make-type-set (&rest members)
  (let ((subs (mappend λ(typecase _
                          (list _)
                          (vector (coerce _ 'list))
                          (otherwise (list _)))
                       members)))
    (make-type-set* subs)))

;;------------------------------------------------------------

(defun valid-type-set-member-p (x)
  (typep x 'v-type))

(defun valid-type-set-p (set)
  ;; {TODO} replace with (every #'valid-type-set-member set)
  (let ((set-list (coerce set 'list)))
    (and (arrayp set)
         (every λ(typep _ 'v-type) set-list))))

(defun assert-valid-type-set (set &key (error-hint ""))
  (assert (valid-type-set-p set)
          (set) "Invalid ~a type-set: ~a" error-hint set))

(defun type-set-to-type-list (set)
  (coerce set 'list))

;;------------------------------------------------------------

(defun valid-return-spec-member-p (x)
  (or (typep x 'return-type-generator)
      (valid-type-set-member-p x)))

(defun valid-func-return-spec-p (spec)
  (or (functionp spec)
      (every #'valid-return-spec-member-p spec)))

;;------------------------------------------------------------
;; Typed Names (these probably need a better home)

(defgeneric make-typed-external-name (type glsl-name &optional qualifiers)
  (:method ((type v-type) (glsl-name string) &optional qualifiers)
    (let ((qualifiers (sort (copy-list (union (qualifiers type) qualifiers))
                            #'string<)))
      (make-instance 'typed-external-name
                     :type (qualify-type type qualifiers)
                     :glsl-name glsl-name))))

(defmethod flow-ids ((obj typed-external-name))
  (flow-ids (v-type-of obj)))

;;------------------------------------------------------------

(defgeneric v-voidp (x)
  (:method ((x compiled))
    (v-voidp (type-set x)))
  (:method ((x v-type))
    (typep x 'v-void))
  (:method ((x vector))
    (= (length x) 0)))

(defgeneric swizzlable-p (x)
  (:method ((x compiled))
    (swizzlable-p (primary-type x)))
  (:method ((x v-type))
    (v-typep x 'v-vector)))
