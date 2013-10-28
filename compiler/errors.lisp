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
                (format stream ,error-string ,@body)))))

;-----------------------------;

(define-condition missing-function-error (error)
  ((text :initarg :text :reader text)))

(deferror not-core-type-error (type-name)
  "Type ~a is not a core type and thus cannot end up in glsl src code
   It must end up being used or converted to something that resolves 
   to a glsl type." type-name)
