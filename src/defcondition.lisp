(in-package :varjo.internals)

;;-----------INSPIRATION-----------;

;; (define-condition machine-error (error)
;;   ((machine-name :initarg :machine-name :reader machine-error-machine-name))
;;   (:report (lambda (condition stream)
;;              (format stream "There is a problem with ~A."
;;                      (machine-error-machine-name condition)))))

;;------------HELPERS-----------;

(defmacro defcondition (name (&key error-type prefix)
                                (&rest args) error-string &body body)
  (assert error-type () "DEFCONDITION: error-type is a mandatory argument")
  (unless (every #'symbolp args) (error "can only take simple args"))
  (let ((control-str (format nil "~@[~a: ~]~a" prefix error-string)))
    `(define-condition ,name (,error-type)
       ,(mapcar (lambda (arg) `(,arg :initarg ,(kwd arg))) args)
       (:report (lambda (condition stream)
                  (declare (ignorable condition))
                  (with-slots ,args condition
                    (format stream ,control-str ,@body)))))))

(defmacro define-bug (name (&key (error-type 'varjo-error) (prefix "Varjo Bug"))
                            (&rest args) error-string &body body)
  `(defcondition ,name (:error-type ,error-type :prefix ,prefix) ,args
       ,error-string ,@body))

(defmacro define-error (name (&key (error-type 'varjo-error) (prefix "Varjo"))
                            (&rest args) error-string &body body)
  `(defcondition ,name (:error-type ,error-type :prefix ,prefix) ,args
       ,error-string ,@body))

(defmacro define-warning (name (&key (error-type 'varjo-warning) (prefix "Varjo"))
                              (&rest args) error-string &body body)
  `(defcondition ,name (:error-type ,error-type :prefix ,prefix) ,args
       ,error-string ,@body))

(define-condition varjo-error (error) ())
(define-condition varjo-critical-error (error) ())
(define-condition varjo-warning (warning) ())

;;-----------------------------;
