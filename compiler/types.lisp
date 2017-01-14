(in-package :varjo)

;;------------------------------------------------------------

(defmethod core-typep ((type v-t-type))
  nil)

;;------------------------------------------------------------

(def-v-type-class v-void (v-t-type) ())
(def-v-type-class v-none (v-t-type) ())

;;------------------------------------------------------------

(def-v-type-class v-error (v-type)
  ((payload :initform nil :initarg :payload :accessor v-payload)))

;;------------------------------------------------------------

(def-v-type-class v-compile-time-value (v-type)
  ((ctv :initform nil :initarg :ctv :accessor ctv)))

;;------------------------------------------------------------

(def-v-type-class v-container (v-type)
  ((element-type :initform t)
   (dimensions :initform nil :accessor v-dimensions)))

(def-v-type-class v-array (v-container)
  ((element-type :initform nil :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)))

(def-v-type-class v-sampler (v-type) ())

(defmethod v-glsl-string ((object v-array))
  (format nil "~a ~~a~{[~a]~}" (v-glsl-string (v-element-type object))
          (mapcar (lambda (x)
                    (if (numberp x) x ""))
                  (v-dimensions object))))

;; (v-glsl-string (type-spec->type '(:vec3 *)))

;;------------------------------------------------------------

;; {TODO} dedicated error
(defmethod v-element-type ((object v-t-type))
  (let ((result (slot-value object 'element-type)))
    (assert (typep result 'v-t-type) (object)
            "The element-type of ~a was ~a which is not an instance of a type."
            object result)
    result))

;;------------------------------------------------------------

(def-v-type-class v-any-one-of (v-compile-time-value)
  ((types :initform nil :initarg :types :reader v-types)))

;;------------------------------------------------------------

(def-v-type-class v-struct (v-type)
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (signature :initform nil :initarg :signature :accessor v-signature)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (slots :initform nil :initarg :slots :reader v-slots)))

(def-v-type-class v-user-struct (v-struct) ())

;;------------------------------------------------------------

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
              (v-t-type (type-of (first return-spec)))
              (otherwise return-spec)))))

(defun v-closure-p (type)
  (and (typep type 'v-function-type)
       (ctv type)
       (implicit-args (ctv type))))

;;------------------------------------------------------------

(def-v-type-class v-stemcell (v-type) ())

;;------------------------------------------------------------

(defmethod type->type-spec ((type v-t-type))
  (class-name (class-of type)))

(defmethod type->type-spec ((type v-array))
  (if (and (v-element-type type) (v-dimensions type))
      (list (type->type-spec (v-element-type type)) (v-dimensions type))
      'v-array))

(defmethod type->type-spec ((type v-function-type))
  (with-slots (argument-spec return-spec) type
    ;; {TODO} remove this, done now
    (assert (listp return-spec))
    (let* ((in (mapcar #'type->type-spec argument-spec))
           (out (if (= (length return-spec) 1)
                    (type->type-spec (first return-spec))
                    (mapcar #'type->type-spec return-spec))))
      `(function ,in ,out))))

(defun try-type-spec->type (spec)
  (let ((spec (cond ((keywordp spec) (p-symb 'varjo 'v- spec))
                    ((and (listp spec) (keywordp (first spec)))
                     (cons (p-symb 'varjo 'v- (first spec)) (rest spec)))
                    (t spec))))
    (cond ((null spec) nil)
          ((and (symbolp spec) (vtype-existsp spec))
           (let ((type (make-instance spec)))
             (when (typep type 'v-t-type)
               type)))
          ((and (listp spec) (eq (first spec) 'function))
           (make-instance
            'v-function-type :arg-spec (mapcar #'type-spec->type (second spec))
            :return-spec (mapcar #'type-spec->type
                                 (uiop:ensure-list (third spec)))))
          ((and (listp spec) (vtype-existsp (first spec)))
           (destructuring-bind (type dimensions) spec
             (make-instance 'v-array :element-type (if (keywordp type)
                                                       (symb 'v- type)
                                                       type)
                            :dimensions dimensions)))
          (t nil))))

(defun type-specp (spec)
  (handler-case (and (type-spec->type spec) t)
    (unknown-type-spec (e)
      (declare (ignore e))
      nil)))

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

;; shouldnt the un-shadow be in try-type-spec->type?
(defun type-spec->type (spec)
  (v-true-type
   (or (try-type-spec->type spec)
       (try-type-spec->type (un-shadow spec))
       (error 'unknown-type-spec :type-spec spec))))

(defun gen-none-type ()
  (type-spec->type :none))

(defmethod v-true-type ((object v-t-type))
  object)

(defmethod v-glsl-size ((type t))
  (slot-value type 'glsl-size))

(defmethod v-glsl-size ((type v-array))
  (* (apply #'* (v-dimensions type))
     (slot-value (v-element-type type) 'glsl-size)))

(defmethod v-type-eq ((a v-type) (b v-type) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec b)))
(defmethod v-type-eq ((a v-type) (b symbol) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec (type-spec->type b))))
(defmethod v-type-eq ((a v-type) (b list) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec (type-spec->type b))))

(defmethod v-type-eq ((a v-void) (b v-void) &optional (env *global-env*))
  (declare (ignore env))
  t)
(defmethod v-type-eq (a (b v-void) &optional (env *global-env*))
  (declare (ignore env))
  nil)
(defmethod v-type-eq ((a v-void) b &optional (env *global-env*))
  (declare (ignore env))
  nil)

(defmethod v-type-eq ((a v-none) (b v-none) &optional (env *global-env*))
  (declare (ignore env))
  t)
(defmethod v-type-eq (a (b v-none) &optional (env *global-env*))
  (declare (ignore env))
  nil)
(defmethod v-type-eq ((a v-none) b &optional (env *global-env*))
  (declare (ignore env))
  nil)

(defmethod v-typep ((a t) (b v-none) &optional (env *global-env*))
  (declare (ignore env))
  (typep a (type-of b)))

(defmethod v-typep ((a v-t-type) (b v-type) &optional (env *global-env*))
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

(defmethod v-casts-to ((from-type v-stemcell) (to-type v-t-type) env)
  (declare (ignore env from-type))
  to-type)

(defmethod v-casts-to-p (from-type to-type env)
  (not (null (v-casts-to from-type to-type env))))

;;[TODO] vtypep here?
(defmethod v-casts-to ((from-type v-type) (to-type v-type) env)
  (if (v-typep from-type to-type)
      from-type
      (when (slot-exists-p from-type 'casts-to)
        (loop :for cast-type :in (slot-value from-type 'casts-to)
           :if (v-typep (type-spec->type cast-type) to-type env)
           :return (type-spec->type cast-type)))))


(defun find-mutual-cast-type (&rest types)
  (let ((names (loop :for type :in types
                  :collect (if (typep type 'v-t-type)
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

(defun v-errorp (obj) (typep obj 'v-error))

(defmethod post-initialise ((object v-t-type)))

(defmethod post-initialise ((object v-container))
  (with-slots (dimensions element-type) object
    (setf dimensions (listify dimensions))
    (unless (or (typep element-type 'v-t-type) (eq element-type t))
      (setf element-type (type-spec->type element-type)))))

(defmethod post-initialise ((object v-sampler))
  (with-slots (element-type) object
    (unless (typep element-type 'v-t-type)
      (setf element-type (type-spec->type element-type)))))

(defmethod initialize-instance :after ((type-obj v-t-type) &rest initargs)
  (declare (ignore initargs))
  (post-initialise type-obj))

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
