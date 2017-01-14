(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defclass v-function ()
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)
   (v-place-index :initform nil :initarg :v-place-index :reader v-place-index)
   (name :initform nil :initarg :name :reader name)
   (implicit-args :initform nil :initarg :implicit-args :reader implicit-args)
   (in-out-args :initform nil :initarg :in-out-args :reader in-out-args)
   (in-arg-flow-ids :initform (error 'flow-ids-mandatory :for :v-function
                                     :code-type :v-function)
                    :initarg :in-arg-flow-ids :reader in-arg-flow-ids)
   (flow-ids :initform (error 'flow-ids-mandatory :for :v-function
                              :code-type :v-function)
             :initarg :flow-ids :reader flow-ids)))

(defmethod functions ((fn v-function))
  (list fn))

;;------------------------------------------------------------

(def-v-type-class v-user-function (v-function)
  ((code :initform nil :initarg :code :reader v-code)))

(defmethod functions ((fn v-user-function))
  (list fn))

;;------------------------------------------------------------

(defclass v-function-set ()
  ((functions :initform nil :initarg :functions :reader functions)))

(defmethod initialize-instance :after ((set v-function-set)
                                       &key functions &allow-other-keys)
  (assert (every λ(or (typep _ 'v-function) (typep _ 'external-function))
                 functions)
          (functions)
          "Failed to initialize v-function-set:~% functions: ~s" functions))

(defmethod print-object ((fs v-function-set) stream)
  (if (null (functions fs))
      (format stream "#<EMPTY-FUNCTION-SET>")
      (call-next-method)))

;;------------------------------------------------------------

(defmethod v-type-of ((func v-function))
  (with-slots (argument-spec return-spec) func
    (assert (listp return-spec))
    (make-instance 'v-function-type
                   :ctv func ;; make the func the compile-time-val in this type
                   :arg-spec argument-spec
                   :return-spec return-spec)))

(defmethod v-type-of ((func-set v-function-set))
  (make-instance 'v-any-one-of
                 :types (mapcar #'v-type-of (functions func-set))
                 :ctv func-set))

(defmethod v-place-function-p ((f v-function))
  (not (null (v-place-index f))))

(defmethod print-object ((object v-function) stream)
  (with-slots (name argument-spec return-spec) object
    (format stream "#<V-FUNCTION ~s ~s -> ~s>"
            name
            (if (eq t argument-spec)
                '(t*)
                (mapcar #'type-of argument-spec))
            (typecase (first return-spec)
              (function t)
              (v-type (type-of (first return-spec)))
              (otherwise return-spec)))))

;;------------------------------------------------------------



(defun v-make-f-spec (name transform versions arg-types return-spec
                      &key v-place-index glsl-name implicit-args
                        in-out-args flow-ids in-arg-flow-ids
                        code)
  (assert (listp return-spec))
  (list transform arg-types return-spec versions v-place-index
        glsl-name name implicit-args in-out-args flow-ids in-arg-flow-ids
        code))

(defun make-function-obj (name transform versions arg-spec return-spec
                          &key v-place-index glsl-name implicit-args
                            in-out-args flow-ids in-arg-flow-ids)
  (make-instance 'v-function
                 :glsl-string transform
                 :arg-spec (if (listp arg-spec)
                               (loop :for spec :in arg-spec :collect
                                  (type-spec->type spec))
                               arg-spec)
                 :return-spec
                 (mapcar (lambda (rspec)
                           (if (type-specp rspec)
                               (type-spec->type rspec)
                               rspec))
                         return-spec)
                 :versions versions :v-place-index v-place-index
                 :glsl-name glsl-name
                 :name name
                 :implicit-args implicit-args
                 :in-out-args in-out-args
                 :flow-ids flow-ids
                 :in-arg-flow-ids in-arg-flow-ids))

(defun make-user-function-obj (name transform versions arg-spec return-spec
                               &key v-place-index glsl-name implicit-args
                                 in-out-args flow-ids in-arg-flow-ids
                                 code)
  (make-instance 'v-user-function
                 :glsl-string transform
                 :arg-spec (if (listp arg-spec)
                               (loop :for spec :in arg-spec :collect
                                  (type-spec->type spec))
                               arg-spec)
                 :return-spec
                 (mapcar (lambda (rspec)
                           (if (type-specp rspec)
                               (type-spec->type rspec)
                               rspec))
                         return-spec)
                 :versions versions :v-place-index v-place-index
                 :glsl-name glsl-name
                 :name name
                 :implicit-args implicit-args
                 :flow-ids flow-ids
                 :in-arg-flow-ids in-arg-flow-ids
                 :in-out-args in-out-args
                 :code code))

;; {TODO} make this use the arg & return types
(defun gen-dummy-func-glsl-name (func-type)
  (declare (ignore func-type))
  "<dummy-func>")

(defun make-dummy-function-from-type (func-type)
  (let ((arg-spec (v-argument-spec func-type))
        (return-spec (v-return-spec func-type))
        (glsl-name (gen-dummy-func-glsl-name func-type)))
    (make-instance
     'v-function
     :glsl-string (format nil "~a(~{~a~})" glsl-name
                          (loop :for i :in arg-spec :collect "~a"))
     :arg-spec arg-spec
     :return-spec return-spec
     :versions *supported-versions*
     :v-place-index nil
     :glsl-name glsl-name
     :name 'dummy-func
     :implicit-args nil
     :flow-ids (flow-id!)
     :in-arg-flow-ids (loop :for i :in arg-spec :collect (flow-id!))
     :in-out-args nil)))

;;------------------------------------------------------------
