(in-package :varjo)
(in-readtable fn:fn-reader)

(defclass ast-node ()
  ((starting-env :initarg :starting-env :reader ast-starting-env)
   (ending-env :initarg :ending-env :reader ast-ending-env)
   (node-kind :initarg :node-kind :reader ast-node-kind)
   (return-type :initarg :return-type :reader ast-return-type)
   (flow-id :initarg :flow-id :reader ast-flow-id)
   (flow-id-origin :initarg :flow-id-origin :reader ast-flow-id-origin)
   (args :initarg :args :reader ast-args)))

(defparameter *node-kinds* '(:function-call :get :literal))

(defun ast-node! (node-kind args return-type flow-id starting-env ending-env)
  ;;(assert (member node-kind *node-kinds*))
  (make-instance 'ast-node
		 :node-kind node-kind
		 :args (listify args)
		 :return-type return-type
		 :flow-id flow-id
		 :starting-env starting-env
		 :ending-env ending-env))

(defun copy-ast-node (node
		      &key
			(node-kind nil set-node-kind)
			(args nil set-args)
			(return-type nil set-return-type)
			(flow-id nil set-flow-id)
			(flow-id-origin nil set-fio)
			(starting-env nil set-starting-env)
			(ending-env nil set-ending-env))
  (make-instance
   'ast-node
   :node-kind (if set-node-kind node-kind (ast-node-kind node))
   :args (if set-args args (ast-args node))
   :return-type (if set-return-type return-type (ast-return-type node))
   :flow-id (if set-flow-id flow-id (ast-flow-id node))
   :flow-id-origin (if set-fio flow-id-origin (ast-flow-id-origin node))
   :starting-env (if set-starting-env starting-env (ast-starting-env node))
   :ending-env (if set-ending-env ending-env (ast-ending-env node))))))

(defun walk-ast (func from-node)
  (labels ((walk-node (ast)
	     (if (eq ast :ignored)
		 :ignored
		 (typecase ast
		   (ast-node (funcall func ast #'walk-node))
		   (list (mapcar #'walk-node ast))
		   (t ast)))))
    (typecase from-node
      (code (walk-node (node-tree from-node)))
      (varjo-compile-result (walk-node (ast from-node)))
      (ast-node (walk-node from-node))
      (t (error "object with the invalid type ~s passed to ast->code"
		(type-of from-node))))))

(defun visit-ast-nodes (func x)
  (labels ((f (node walk)
	     (funcall func node)
	     (with-slots (args) node
	       (mapcar walk args))))
    (walk-ast #'f x)
    t))

(defun ast->pcode (x)
  (labels ((f (node walk)
	     (with-slots (node-kind args) node
	       (cons node-kind (mapcar walk args)))))
    (walk-ast #'f x)))
