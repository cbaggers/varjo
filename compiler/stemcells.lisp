(in-package :varjo)

(defmacro with-stemcell-infer-hook (func &body body)
  (let ((func-name (gensym "hook")))
    `(let* ((,func-name ,func)
            (*stemcell-infer-hook* ,func-name))
       ,@body)))

(defun suitable-symbol-for-stemcellp (symb env)
  (and (allows-stemcellsp env)
       (boundp symb)))

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

(defmethod v-dimensions ((object v-stemcell)) 0)

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
     :type (type-spec->type 'v-stemcell flow-id)
     :current-line string-name
     :stemcells `(,(stemcell! original-name string-name :|unknown-type|
                              flow-id))
     :node-tree (ast-node! :get-stemcell symbol nil flow-id env env))))

(defun stemcellp (x)
  (typep x 'v-stemcell))
