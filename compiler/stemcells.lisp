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
  (let ((type (type-spec->type type-name)))
    (copy-code code-obj
               :type type
               :stemcells (let ((stemcell (first (stemcells code-obj))))
                            `((,(first stemcell)
                                ,(second stemcell)
                                ,type-name))))))

(def-v-type-class v-stemcell (v-type) ())

(defmethod v-dimensions ((object v-stemcell)) 0)

(defun make-stem-cell (symbol env)
  (let* ((string-name (string (safe-glsl-name-string symbol)))
	 (original-name symbol)
	 (flow-id (get-flow-id-for-stem-cell original-name env)))
    (code!
     :type 'v-stemcell
     :current-line string-name
     :stemcells `((,original-name ,string-name :|unknown-type|))
     :node-tree (ast-node! :get-stemcell symbol nil env env)
     :flow-ids (list flow-id))))

(defun stemcellp (x)
  (typep x 'v-stemcell))

(defmethod v-casts-to ((from-type v-stemcell) (to-type v-t-type) env)
  to-type)

(defmethod v-typep ((a v-stemcell) b &optional (env *global-env*))
  (declare (ignore env))
  t)
