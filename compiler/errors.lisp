(in-package :varjo)

;-----------EXAMPLE-----------;
(define-condition machine-error (error)
  ((machine-name :initarg :machine-name :reader machine-error-machine-name))
  (:report (lambda (condition stream)
             (format stream "There is a problem with ~A."
                     (machine-error-machine-name condition)))))
;------------HELPER-----------;

;;[TODO] need better arg test
(defmacro deferror (name (&rest args) error-string &body body)
  (unless (every #'symbolp args) (error "can only take simple args"))
  (loop :for arg :in args :do
     (setf body (subst `(,arg condition) arg body :test #'eq)))
  `(define-condition ,name (error)
     (,@(loop :for arg :in args :collect
           `(,arg :initarg ,(kwd arg) :reader ,arg)))
     (:report (lambda (condition stream)
                (declare (ignorable condition))
                (format stream ,error-string ,@body)))))

;-----------------------------;

(define-condition missing-function-error (error)
  ((text :initarg :text :reader text)))

(deferror problem-with-the-compiler ()
  "This shouldnt have been possible so this needs a bug report. Sorry about that")

(deferror cannot-compile (code)
  "Cannot compile the following code:~%~a" code)

(deferror not-core-type-error (type-name)
  "Type ~a is not a core type and thus cannot end up in glsl src code
   It must end up being used or converted to something that resolves 
   to a glsl type." type-name)

(deferror invalid-function-return-spec (func spec)
    "Outbound spec of function ~a is invalid:~%~a" func spec)

(deferror clone-global-env-error ()
    "Cannot clone the global environment")

(deferror no-valid-function (name types)
    "There is no applicable method for the glsl function '~s'~%when called with argument types:~%~s " name types)

;-----------------------------;

(defclass deferred-error ()
  ((error-name :initform nil :initarg :error-name :reader error-name)
   (error-args :initform nil :initarg :error-args :reader error-args)))

(defmethod raise-deffered-error ((error deferred-error))
  (if (error-name error)
      (apply #'error (cons (error-name error) (error-args error)))
      (apply #'error (error-args error))))
