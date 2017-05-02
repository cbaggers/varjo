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
  (assert (stemcellp (primary-type code-obj)))
  (let ((type (type-spec->type type-name (flow-ids code-obj)))
        (stemcells (stemcells code-obj)))
    (assert (= 1 (length stemcells)))
    (copy-compiled
     code-obj
     :current-line (unless (ephemeral-p type) (current-line code-obj))
     :type-set (make-type-set type)
     :stemcells (with-slots (name string-name flow-id cpu-side-transform)
                    (first stemcells)
                  (list (stemcell! name string-name type-name flow-id
                                   cpu-side-transform))))))

(defmethod v-dimensions ((object v-stemcell)) 0)

(defun stemcell! (original-name string-name type flow-id
                  &optional cpu-side-transform)
  (make-instance 'stemcell
                 :name original-name
                 :string-name string-name
                 :type type
                 :flow-id flow-id
                 :cpu-side-transform (or cpu-side-transform original-name)))

(defun make-stem-cell (symbol env &optional cpu-side-transform)
  (let* ((string-name (string (safe-glsl-name-string symbol)))
         (original-name symbol)
         (flow-id (get-flow-id-for-stem-cell original-name env))
         (type (type-spec->type 'v-stemcell flow-id))
         (type-set (make-type-set type)))
    (make-compiled
     :type-set type-set
     :current-line string-name
     :stemcells `(,(stemcell! original-name string-name :|unknown-type|
                              flow-id cpu-side-transform))
     :node-tree (ast-node! :get-stemcell symbol type-set env env))))

(defun inject-implicit-uniform (symbol type-spec env
                                &optional cpu-side-transform)
  (assert (type-specp type-spec) (type-spec))
  (let ((code (add-type-to-stemcell-code
               (make-stem-cell symbol env cpu-side-transform)
               type-spec))
        (ast (ast-node! 'lisp-code-as-uniform
                        (list symbol type-spec cpu-side-transform)
                        (make-type-set (type-spec->type type-spec))
                        env env)))
    (copy-compiled code :node-tree ast)))

(defun stemcellp (x)
  (typep x 'v-stemcell))
