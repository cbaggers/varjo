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

(def-v-type-class v-user-function (v-function)
  ((code :initform nil :initarg :code :reader v-code)))

(defmethod v-type-of ((func v-function))
  (with-slots (argument-spec return-spec) func
    (assert (listp return-spec))
    (make-instance 'v-function-type
                   :arg-spec argument-spec
                   :return-spec return-spec)))

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
              (v-t-type (type-of (first return-spec)))
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

(defun %func-spec->function (spec env userp)
  (destructuring-bind (transform arg-spec return-spec versions v-place-index
                                 glsl-name name implicit-args in-out-args
                                 flow-ids in-arg-flow-ids code)
      spec
    (if userp
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
                       :code code)
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
                       :in-arg-flow-ids in-arg-flow-ids))))

(defun func-spec->function (spec env)
  (%func-spec->function spec env nil))

(defun func-spec->user-function (spec env)
  (%func-spec->function spec env t))

(defun function->func-spec (func)
  (let ((arg-spec (v-argument-spec func)))
    (v-make-f-spec (name func)
                   (v-glsl-string func)
                   nil ;;{TODO} this must be versions
                   (when (listp arg-spec)
                     (loop :for a :in arg-spec :collect (type->type-spec a)))
                   (mapcar (lambda (spec)
                             (if (type-specp spec)
                                 (type->type-spec spec)
                                 spec))
                           (v-return-spec func))
                   :v-place-index (v-place-index func)
                   :glsl-name (v-glsl-name func)
                   :implicit-args (implicit-args func)
                   :flow-ids (flow-ids func)
                   :in-arg-flow-ids (in-arg-flow-ids func)
                   :in-out-args (in-out-args func)
                   :code (when (typep func 'v-user-function)
                           (v-code func)))))

;;------------------------------------------------------------
