(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; - √ rename v-func-val to v-function-type
;; - √ make v-compile-time-value a subclass of v-type (not a mixin)
;; - √ add 'value' slot to v-compile-time-value class
;; - √ remove v-compile-time-value's initialize-instance method
;; - √ remove the v-type from v-function-type's superclass list
;; - √ change (def-v-type-class v-function ..) to (defclass v-function ..)
;; - √ move v-function & v-place-function-p out of types.lisp
;; - √ comment out 'funcall' special func for now
;; - √ make tests pass.
;; - X modify add-function to add function-types to type-pool - not needed
;; - √ make the special function 'function' return a v-function-type with
;;     the 'value' slot populated.
;; - √ remove #'%funcall-literal, just use val always
;; - in compile-funcall.lisp modify #'compile-list-form so that the first
;;   arg can be the name OR a code-object with function-type (and value)
;; - modify 'funcall' special function to just compile the func arg and then
;;   emit `(,func-code-obj ,@args)
;; - now logic will live in compile-funcall.lisp (yay!)
;; - get simple function calls to work again (tests 0->2 in first-class-tests)
;; - get the 'break' in #'compile-function-taking-ctvs to fire.
;; We are now in exciting new teritory! make a new plan
;; - make v-type-of work for v-values & code-objects

(defclass v-function ()
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)
   (v-place-index :initform nil :initarg :v-place-index :reader v-place-index)
   (name :initform nil :initarg :name :reader name)
   (implicit-args :initform nil :initarg :implicit-args :reader implicit-args)
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
