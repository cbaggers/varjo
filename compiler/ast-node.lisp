(in-package :varjo)
(in-readtable fn:fn-reader)

(defclass ast-node ()
  ((starting-env :initarg :starting-env)
   (ending-env :initarg :ending-env)
   (node-kind :initarg :node-kind)
   (return-type :initarg :return-type)
   (args :initarg :args)))

(defparameter *node-kinds* '(:function-call :get :literal))

(defun ast-node! (node-kind args return-type starting-env ending-env)
  ;;(assert (member node-kind *node-kinds*))
  (make-instance 'ast-node
		 :node-kind node-kind
		 :args (listify args)
		 :return-type return-type
		 :starting-env starting-env
		 :ending-env ending-env))

(defun ast->code (x)
  (labels ((walk-node (ast)
	     (if (eq ast :ignored)
		 :ignored
		 (typecase ast
		   (ast-node
		    (cons (slot-value ast 'node-kind)
			  (mapcar #'walk-node (slot-value ast 'args))))
		   (list (mapcar #'walk-node ast))
		   (t ast)))))
    (typecase x
      (code (walk-node (node-tree x)))
      (ast-node (walk-node x))
      (t (error "object with the invalid type ~s passed to ast->code"
		(type-of x))))))
