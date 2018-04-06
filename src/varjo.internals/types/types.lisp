(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Alternate Type Names

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop :for (shorthand . type) :in *type-shorthand* :do
     (force-alternate-type-name shorthand type)))

(defun add-alternate-type-name (alt-type-name src-type-name)
  (declare (notinline type-spec->type))
  (assert (and (symbolp src-type-name) (symbolp alt-type-name)))
  (unless (gethash alt-type-name *alternate-ht*)
    (assert (not (try-type-spec->type alt-type-name nil)) ()
            'alt-type-name-already-taken
            :alt-name alt-type-name
            :src-name src-type-name))
  (let (;; Ensure that, if the src-type-name is another alias, that
        ;; we resolve the real type name before going any further
        (src-type-name (or (gethash src-type-name *alternate-ht*)
                           src-type-name))
        ;; We need this for the ephemeral check but it has the added
        ;; bonus of erroring if the source type doesnt exist.
        ;;              ↓↓↓
        (src-type (type-spec->type src-type-name)))
    (setf (gethash alt-type-name *alternate-ht*) src-type-name)
    (setf (gethash src-type-name *alternate-ht-backward*) alt-type-name)
    (when (and (ephemeral-p src-type)
               (not (get-global-form-binding alt-type-name)))
      (add-alt-ephemeral-constructor-function src-type-name alt-type-name)))
  alt-type-name)

(defun remove-alternate-type-name (alt-type-name)
  (let ((alt-type (type-spec->type alt-type-name))
        (src-type-name (gethash alt-type-name *alternate-ht*)))
    (assert src-type-name ()
            'unknown-alt-type-name
            :name alt-type-name)
    ;;
    ;; Remove all functions & macros that have the type in their signatures
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (loop :for binding :in v :do
          (typecase binding
            ((or v-function external-function)
             (when (or (argument-spec-includes-type binding alt-type)
                       (return-spec-includes-type binding alt-type))
               (remove-global-form-binding binding))))))
     *global-env-form-bindings*)
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (loop :for binding :in v :do
          (when (argument-spec-includes-type binding alt-type)
            (remove-global-compiler-macro binding))))
     *global-env-compiler-macros*)
    ;;
    ;; And finally remove the alias
    (remhash alt-type-name *alternate-ht*)
    (remhash src-type-name *alternate-ht-backward*))
  nil)

(defun argument-spec-includes-type (func type)
  (assert (typep func '(or v-function external-function v-compiler-macro)))
  (assert (typep type 'v-type))
  (flet ((t-eq (x y)
           (when (typep y 'v-type)
             (v-type-eq x y))))
    (and (listp (v-argument-spec func))
         (find type (v-argument-spec func) :test #'t-eq))))

(defun return-spec-includes-type (func type)
  (assert (typep func '(or v-function external-function)))
  (assert (typep type 'v-type))
  (flet ((t-eq (x y)
           (when (typep y 'v-type)
             (v-type-eq x y))))
    (and (arrayp (v-return-spec func))
         (find type (v-return-spec func) :test #'t-eq))))

;;------------------------------------------------------------
;; Type predicate

(define-compiler-macro v-typep (&whole whole a b)
  (let ((lit-spec
         (when (constantp b)
           (cond
             ((keywordp b) b)
             ((and (listp b) (eq (first b) 'quote))
              (second b))))))
    (if lit-spec
        (let ((type (try-type-spec->type lit-spec nil)))
          (if type
              (if (core-typep type)
                  `(typep ,a ',(slot-value type 'type-name))
                  `(v-typep ,a ,type))
              whole))
        whole)))

(defmethod v-typep ((a v-type) (b symbol))
  (v-typep a (type-spec->type (resolve-name-from-alternative b))))

(defmethod v-typep ((a v-type) (b list))
  (let ((b (resolve-name-from-alternative
            (mapcar #'resolve-name-from-alternative b))))
    (v-typep a (type-spec->type b))))

(defmethod v-typep ((a v-type) (b v-type))
  (typep a (type-of b)))

(defmethod v-typep ((a null) b)
  (declare (ignore a b))
  nil)

(defmethod v-typep (a (b null))
  (declare (ignore a b))
  nil)

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

(defun arg-form->type-spec (arg-form)
  (if (&rest-p arg-form)
      arg-form
      (type->type-spec arg-form)))

(defun arg-form->type (arg-form)
  (if (&rest-p arg-form)
      arg-form
      (type-spec->type arg-form)))

;; A probably too forgiving version of type-spec->type, it is a no-op
;; on v-types
(defun as-v-type (thing)
  (etypecase thing
    (v-type thing)
    (symbol (type-spec->type thing))))

;;------------------------------------------------------------
;; Opaque

(define-v-type-class v-opaque (v-type) ())

;;------------------------------------------------------------
;; Void

(define-v-type-class v-void (v-type)
  ((core :initform t :reader core-typep)
   (glsl-string :initform "void" :reader v-glsl-string)
   (glsl-size :initform :sizeless)))

;;------------------------------------------------------------
;; Discarded
;;
;; The type of the discard expression. Indicates a termination
;; of execution without a return.

(define-v-type-class v-discarded (v-type)
  ((glsl-string :initform "void" :reader v-glsl-string)))

(defun v-discarded-p (obj)
  (etypecase obj
    (v-type (typep obj 'v-discarded))
    (compiled (v-discarded-p (type-set obj)))
    (vector (not (null (find-if λ(typep _ 'v-discarded) obj))))))

;;------------------------------------------------------------
;; Returned
;;
;; Indicates a termination of execution due to 'return'

(define-v-type-class v-returned (v-type) ())

(defun v-returned-p (obj)
  (etypecase obj
    (v-type (typep obj 'v-returned))
    (compiled (v-returned-p (type-set obj)))
    (vector (not (null (find-if λ(typep _ 'v-returned) obj))))))

;;------------------------------------------------------------
;; Shadow Type
;;
;; The supertype for all types which that are shadowing a core
;; glsl type.

(define-v-type-class v-shadow-type (v-type)
  ((shadowed-type :initform nil :reader shadowed-type)))

;;------------------------------------------------------------
;; Sampler

(define-v-type-class v-sampler (v-opaque)
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
;; Container
;;
;; The supertype of all types that can have values stored into, and
;; retrieved from, themselves

(define-v-type-class v-container (v-type)
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

(define-v-type-class v-array (v-container)
  ;; note: not a core type. Changing this changes assumptions about
  ;;       the v-typep compiler-macro
  ((element-type :initform t :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)))

(defmethod v-typep ((a v-array) (b v-array))
  (v-typep (v-element-type a) (v-element-type b)))

(defmethod v-make-type ((type v-array) flow-id &rest args)
  (destructuring-bind (element-type length) args
    (let ((length (if (and (symbolp length) (string= length "*"))
                      'cl:*
                      length)))
      (initialize-instance
       type
       :element-type (type-spec->type element-type (when flow-id (flow-id!)))
       :dimensions (listify length)
       :flow-ids flow-id))))

(defmethod v-glsl-size ((type v-array))
  (* (apply #'* (v-dimensions type))
     (v-glsl-size (v-element-type type))))

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

;;------------------------------------------------------------
;; Ephemeral Values
;;
;; The supertype for all types which that have no representation
;; in glsl.
;;
;; Make sure you define these using deftype
;;

(define-v-type-class v-ephemeral-type (v-type) ())

(defun ephemeral-p (obj)
  (typecase obj
    (v-type (typep obj 'v-ephemeral-type))
    (compiled (if (slot-boundp obj 'is-ephemeral)
                  (slot-value obj 'is-ephemeral)
                  (setf (slot-value obj 'is-ephemeral)
                        (ephemeral-p (type-set obj)))))
    (vector (not (null (some #'ephemeral-p obj))))
    (otherwise (ephemeral-p (v-type-of obj)))))

;;------------------------------------------------------------
;; Ephemeral Array
;;
;; An array for item which that have no representation in glsl.
;;

(define-v-type-class v-ephemeral-array (v-array v-ephemeral-type) ())

(defmethod v-make-type ((type v-ephemeral-array) flow-id &rest args)
  (destructuring-bind (element-type length) args
    (assert (or (and (numberp length) (typep length 'unsigned-byte))
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

(define-v-type-class v-unrepresentable-value (v-ephemeral-type) ())

;;------------------------------------------------------------
;; Trait

(define-v-type-class v-trait (v-unrepresentable-value) ())

;;------------------------------------------------------------
;; Block Struct
;;
;; A vari struct who's representation is a glsl interface block

(define-v-type-class v-block-struct (v-ephemeral-type)
  ((element-type :initform t :initarg :element-type)
   (block-name :initarg :block-name :initform "£-v-block-array-£"
               :reader block-name)))

(defmethod v-make-type ((type v-block-struct) flow-id &rest args)
  (destructuring-bind (block-name element-type) args
    (initialize-instance type
                         :block-name block-name
                         :element-type (type-spec->type element-type)
                         :flow-ids flow-id)))

(defmethod post-initialise ((object v-block-struct))
  (with-slots (element-type) object
    (unless (typep element-type 'v-type)
      (setf element-type (type-spec->type element-type)))))

(defmethod v-element-type ((object v-block-struct))
  (let ((result (slot-value object 'element-type)))
    ;; {TODO} dedicated error
    (assert (typep result 'v-type) (object)
            "The element-type of ~a was ~a which is not an instance of a type."
            object result)
    result))

(defmethod type->type-spec ((type v-block-struct))
  `(v-block-struct ,(if (slot-boundp type 'block-name)
                        (block-name type)
                        "£-unknown-block-name-£")
                   ,(type->type-spec (v-element-type type))))

(defmethod copy-type ((type v-block-struct))
  (make-instance 'v-block-struct
                 :block-name (block-name type)
                 :element-type (v-element-type type)
                 :flow-ids (flow-ids type)))

(defun make-into-block-struct (struct-type block-name)
  (assert (typep struct-type 'v-struct))
  (let* ((r (make-instance 'v-block-struct
                           :block-name block-name
                           :element-type struct-type
                           :ctv (ctv struct-type)
                           :flow-ids (flow-ids struct-type)
                           :qualifiers (qualifiers struct-type))))
    (when (slot-boundp struct-type 'default-value)
      (setf (slot-value r 'default-value)
            (slot-value struct-type 'default-value)))
    r))

(defmethod v-glsl-string ((object v-block-struct))
  (v-glsl-string (v-element-type object)))

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

(define-v-type-class v-block-array (v-ephemeral-type)
  ((element-type :initform t :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)
   (block-name :initarg :block-name :initform "£-v-block-array-£"
               :reader block-name)))

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
      (setf element-type (type-spec->type element-type)))))

(defmethod v-element-type ((object v-block-array))
  (let ((result (slot-value object 'element-type)))
    ;; {TODO} dedicated error
    (assert (typep result 'v-type) (object)
            "The element-type of ~a was ~a which is not an instance of a type."
            object result)
    result))

(defmethod type->type-spec ((type v-block-array))
  `(v-block-array ,(if (slot-boundp type 'block-name)
                       (block-name type)
                       "£-unknown-block-name-£")
                  ,(type->type-spec (v-element-type type))
                  ,(v-dimensions type)))

(defmethod copy-type ((type v-block-array))
  (make-instance 'v-block-array
                 :block-name (block-name type)
                 :dimensions (v-dimensions type)
                 :element-type (v-element-type type)
                 :flow-ids (flow-ids type)))

(defun make-into-block-array (array-type block-name)
  (assert (typep array-type 'v-array))
  (let* ((dim (v-dimensions array-type))
         (r (make-instance 'v-block-array
                           :block-name block-name
                           :dimensions dim
                           :element-type (v-element-type array-type)
                           :ctv (ctv array-type)
                           :flow-ids (flow-ids array-type)
                           :qualifiers (qualifiers array-type))))
    (when (slot-boundp array-type 'default-value)
      (setf (slot-value r 'default-value)
            (slot-value array-type 'default-value)))
    r))

(defun block-array-to-regular-array (block-array)
  (assert (typep block-array 'v-block-array))
  (let ((r (make-instance 'v-array
                          :dimensions (v-dimensions block-array)
                          :element-type (v-element-type block-array)
                          :ctv (ctv block-array)
                          :flow-ids (flow-ids block-array)
                          :qualifiers (qualifiers block-array))))
    (when (slot-boundp block-array 'default-value)
      (setf (slot-value r 'default-value)
            (slot-value block-array 'default-value)))
    r))

(defmethod v-glsl-string ((object v-block-array))
  (v-glsl-string (v-element-type object)))


;;------------------------------------------------------------
;; Or

(define-v-type-class v-or (v-type)
  ((types :initform nil :initarg :types :reader v-types)))

(defmethod copy-type ((type v-or))
  (assert (null (ctv type)))
  (make-instance 'v-or :types (v-types type) :flow-ids (flow-ids type)))

(defmethod type->type-spec ((type v-or))
  `(or ,@(mapcar #'type->type-spec (v-types type))))

(defmethod v-make-type ((type v-or) flow-id &rest args)
  ;; flow-id is discarded as the flow-id will be the union of the
  ;; ids of the types in the spec
  (gen-or-type args flow-id))

(defun gen-or-type (types flow-id)
  (let* ((types (mapcar (lambda (type)
                          (etypecase type
                            (v-type type)
                            ((or list symbol)
                             (type-spec->type type (flow-id!)))))
                        types))
         (reduced (reduce-types-for-or-type types)))
    (case (length reduced)
      (0 (make-instance 'v-or :types nil :flow-ids flow-id))
      (1 (first reduced))
      (otherwise
       (let ((no-terminated (remove-if λ(or (v-discarded-p _)
                                            (v-returned-p _))
                                       types)))
         (case= (length no-terminated)
           (0 (type-spec->type 'v-discarded flow-id))
           (1 (first no-terminated))
           (otherwise
            (make-instance 'v-or :types no-terminated
                           :flow-ids (apply #'flow-id! no-terminated)))))))))

(defgeneric reduce-types-for-or-type (types)
  (:method (types)
    (labels ((inner (type)
               (if (typep type 'v-or)
                   (mappend #'inner (v-types type))
                   (list type))))
      (remove-duplicates (mappend #'inner types) :test #'v-type-eq))))

(defmethod print-object ((obj v-or) stream)
  (format stream "#<OR~{ ~s~}>" (mapcar #'type->type-spec (v-types obj))))

;;------------------------------------------------------------
;; Any one of
;;
;; This value differs from v-or' becaume v-or means it is one
;; of the contained types, whereas this means it's represents
;; all of the values and the compiler is free to pick any which
;; satisfies it's needs

(define-v-type-class v-any-one-of (v-unrepresentable-value)
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

(define-v-type-class v-struct (v-type)
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (signature :initform nil :initarg :signature :accessor v-signature)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (slots :initform nil :initarg :slots :reader v-slots)))

;; Supertype of all structs that are not from the glsl spec
(define-v-type-class v-user-struct (v-struct) ())

;;------------------------------------------------------------
;; Function Type
;;
;; The type of all function objects

(define-v-type-class v-function-type (v-unrepresentable-value)
  ((argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)))

(defmethod v-make-type ((type v-function-type) flow-id &rest args)
  (destructuring-bind (arg-types return-type) args
    (make-instance
     'v-function-type :arg-spec (mapcar #'arg-form->type arg-types)
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
                (mapcar #'arg-form->type-spec argument-spec))
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
                   (mapcar #'arg-form->type-spec argument-spec)))
           (out (cond
                  ((functionp return-spec) return-spec)
                  ((= (length return-spec) 1)
                   (type->type-spec (elt return-spec 0)))
                  (t (map 'list #'type->type-spec return-spec)))))
      `(function ,in ,out))))

(defmethod v-typep ((a v-function-type)
                    (b v-function-type))
  (and (every #'v-type-eq (v-argument-spec a)
              (v-argument-spec b))
       (every #'v-type-eq (v-return-spec a)
              (v-return-spec b))))

;;------------------------------------------------------------
;; Stemcell

(define-v-type-class v-stemcell (v-type) ())

(defmethod v-typep ((a v-stemcell) b)
  (declare (ignore a b))
  t)

;;------------------------------------------------------------
;; Type Equality

;; Type <-> Type
(defmethod v-type-eq ((a v-type) (b v-type))
  (and (eq (slot-value a 'type-name) (slot-value b 'type-name))
       (eq (ctv a) (ctv b))))

(defmethod v-type-eq ((a v-array) (b v-array))
  (and (eq (slot-value a 'type-name) (slot-value b 'type-name))
       (v-type-eq (v-element-type a) (v-element-type b))
       (eq (ctv a) (ctv b))
       ;; assuming num or wild
       (let* ((len-a (first (v-dimensions a)))
              (len-a-num (numberp len-a))
              (len-b (first (v-dimensions b)))
              (len-b-num (numberp len-b)))
         (if len-a-num
             (if len-b-num
                 (= len-a len-b)
                 t)
             (not len-b-num)))))

(defmethod v-type-eq ((a v-block-array) (b v-block-array))
  (and (eq (slot-value a 'type-name) (slot-value b 'type-name))
       (v-type-eq (v-element-type a) (v-element-type b))
       (eq (ctv a) (ctv b))))

(defmethod v-type-eq ((a v-block-struct) (b v-block-struct))
  (and (eq (slot-value a 'type-name) (slot-value b 'type-name))
       (v-type-eq (v-element-type a) (v-element-type b))
       (eq (ctv a) (ctv b))))

;; Type <-> Spec

(defmethod v-type-eq ((a v-type) (b symbol))
   (v-type-eq a (type-spec->type b)))

(defmethod v-type-eq ((a v-type) (b list))
   (v-type-eq a (type-spec->type b)))

;; Function-Type <-> Function-Type
;;

(defmethod v-type-eq ((a v-function-type) (b v-function-type))
  ;; this 'and ctv' stuff makes me nervous.
  ;; See https://github.com/cbaggers/varjo/issues/181
  ;;        ↓↓
  (and (if (and (ctv a) (ctv b))
           (eq (ctv a) (ctv b))
           t)
       (every #'v-type-eq (v-argument-spec a)
              (v-argument-spec b))
       (every #'v-type-eq (v-return-spec a)
              (v-return-spec b))))

;;------------------------------------------------------------
;; Casting


(defmethod v-casts-to ((from-type v-function-type) (to-type v-function-type))
  (when (v-type-eq from-type to-type)
    to-type))

(defmethod v-casts-to ((from-type v-any-one-of) (to-type v-function-type))
  (let* ((funcs (remove-if-not (lambda (fn) (v-casts-to fn to-type))
                               (v-types from-type))))
    (when funcs
      (gen-any-one-of-type funcs))))

(defmethod v-casts-to ((from-type v-stemcell) (to-type v-type))
  (declare (ignore from-type))
  to-type)

(defmethod v-casts-to-p (from-type to-type)
  (not (null (v-casts-to from-type to-type))))

(defmethod v-casts-to ((from-type v-type) (to-type v-type))
  (if (v-typep from-type to-type)
      (strip-flow-id from-type)
      (when (slot-exists-p from-type 'casts-to)
        (loop :for cast-type :in (slot-value from-type 'casts-to)
           :if (v-typep (type-spec->type cast-type) to-type)
           :return (type-spec->type cast-type)))))


(defun find-mutual-cast-type (&rest types)
  (assert (every λ(typep _ 'v-type) types) ()
          'find-mutual-type-bug :types types)
  (if (every λ(v-type-eq _ (first types)) (rest types))
      (first types)
      (let* ((all-casts
              (sort (mapcar (lambda (type)
                              (cons type
                                    (mapcar #'type-spec->type
                                            (slot-value type 'casts-to))))
                            types)
                    #'> :key #'length))
             (master
              (first all-casts))
             (rest-casts
              (rest all-casts))
             (result
              (first (sort (loop :for type :in master
                              :if (every λ(find type _ :test #'v-type-eq)
                                         rest-casts)
                              :collect type)
                           #'>
                           :key #'v-superior-score))))
        result)))

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
  (find-similarly-named-symbol
   type-spec (alexandria:hash-table-values *registered-types*)))

;;------------------------------------------------------------

(defun make-type-set* (members)
  (let ((len (length members)))
    (when (and (= len 1)
               (not (typep (first members) 'v-stemcell)))
      (assert (not (v-voidp (first members)))))
    (make-array len :initial-contents members)))

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
    (let ((qualifiers (sort (copy-list (union (qualifiers type) qualifiers
                                              :test #'qualifier=))
                            #'string<)))
      (make-instance 'typed-external-name
                     :type (qualify-type type qualifiers)
                     :glsl-name glsl-name))))

(defmethod flow-ids ((obj typed-external-name))
  (flow-ids (v-type-of obj)))

;;------------------------------------------------------------

(defun v-terminated-p (x)
  (or (v-returned-p x)
      (v-discarded-p x)))

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
    (typep x 'v-vector)))

(defgeneric has-any-opaque-slots-p (type)
  (:method ((type v-struct))
    (some (lambda (slot)
            (holds-opaque-data-p (second slot)))
          (v-slots type)))
  (:method ((type t))
    nil))

(defgeneric holds-opaque-data-p (type)
  (:method ((type v-struct))
    (has-any-opaque-slots-p type))
  (:method ((type v-container))
    (holds-opaque-data-p (v-element-type type)))
  (:method ((type v-opaque))
    t)
  (:method ((type t))
    nil))

;;------------------------------------------------------------

(defun strip-qualifiers (type)
  (let ((new-type (copy-type type)))
    (setf (slot-value new-type 'qualifiers) nil)
    new-type))

;;------------------------------------------------------------

(defmethod v-superclass ((type v-type))
  (with-slots (superclass) type
    (when superclass
      (type-spec->type superclass))))

;;------------------------------------------------------------
