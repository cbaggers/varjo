(in-package :varjo)

(defparameter *stemcell-infer-hook* (lambda (name) (declare (ignore name)) nil))

(defmacro with-stemcell-infer-hook (func &body body)
  (let ((func-name (gensym "hook")))
    `(let* ((,func-name ,func)
            (*stemcell-infer-hook* ,func-name))
       ,@body)))

(defun suitable-symbol-for-stemcellp (symb env)
  (and (allows-stemcellsp env)
       (let ((str-name (symbol-name symb)))
         (and (char= (elt str-name 0) #\*)
              (char= (elt str-name (1- (length str-name)))
                     #\*)))))

(defun add-type-to-stemcell-code (code-obj type-name)
  (assert (stemcellp (code-type code-obj)))
  (let ((type (type-spec->type type-name))
	(stemcells (stemcells code-obj)))
    (assert (= 1 (length stemcells)))
    (copy-code code-obj
               :type type
               :stemcells (with-slots (name string-name flow-id)
			      (first stemcells)
			    (list (stemcell! name
					     string-name
					     type-name
					     flow-id))))))

(def-v-type-class v-stemcell (v-type) ())

(defmethod v-dimensions ((object v-stemcell)) 0)

(defclass stemcell ()
  ((name :initarg :name)
   (string-name :initarg :string-name)
   (type :initarg :type)
   (flow-id :initarg :flow-id)))

(defun stemcell! (original-name string-name type flow-id)
  (make-instance 'stemcell
		 :name original-name
		 :string-name string-name
		 :type type
		 :flow-id flow-id))

(defun make-stem-cell (symbol env)
  (let* ((string-name (string (safe-glsl-name-string symbol)))
	 (original-name symbol)
	 (flow-id (get-flow-id-for-stem-cell original-name env)))
    (code!
     :type 'v-stemcell
     :current-line string-name
     :stemcells `(,(stemcell! original-name string-name :|unknown-type|
			      flow-id))
     :node-tree (ast-node! :get-stemcell symbol nil flow-id env env)
     :flow-ids (list flow-id))))

(defun stemcellp (x)
  (typep x 'v-stemcell))

(defmethod v-casts-to ((from-type v-stemcell) (to-type v-t-type) env)
  to-type)

(defmethod v-typep ((a v-stemcell) b &optional (env *global-env*))
  (declare (ignore env))
  t)
