(in-package :varjo)

(defun find-injected-functions (code env)
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (f (get-external-function head *global-env*)))
             (append (when f `(,head ,f))
                     (loop :for c :in code :if (listp c) :collect
                        (find-injected-functions c env)))))))

(defun inject-functions-pass (code env)
  (let ((injected-funcs (find-injected-functions code env)))
    (values `(labels (,@(loop :for (name f) :in injected-funcs :collect 
                           `(,name ,(v-glsl-string f))))
               ,@code)
            env)))

;;----------------------------------------------------------------------

(defun v-macroexpand-all (code env)
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-macro head env))
                  (code (if m (apply m code) code)))
             (loop :for c :in code :collect (v-macroexpand-all c env))))))

(defun macroexpand-pass (code env)
  (values (v-macroexpand-all code env) env))

;;----------------------------------------------------------------------

(defun compile-bool (code env)
  (declare (ignore env))
  (if code
      (make-instance 'code :current-line "true" :type '(:bool nil))
      (make-instance 'code :current-line "false" :type '(:bool nil))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (cond ((floatp x) (type-spec->type :float))
        ((integerp x) (type-spec->type :int))
        (t (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (declare (ignore env))
  (let ((num-type (get-number-type code)))
    (make-instance 'code 
                   :current-line (gen-number-string code num-type)
                   :type num-type)))

(defun v-variable->code-obj (var-name v-value)
  (make-instance 'code :type (v-type v-value)
                 :current-line (gen-variable-string var-name)))

(defun compile-symbol (code env)
  (let* ((var-name code)
         (v-value (get-var var-name env)))
    (if v-value
        (v-variable->code-obj var-name v-value)
        (error "Varjo: '~s' is unidentified." code))))

(defun compile-form (code env)
  (let* ((func-name (first code)) 
         (args (rest code))
         (arg-objs (loop :for a :in args :collect (varjo->glsl a env)))
         (func (find-function-for-args func-name arg-objs env)))
    (if func
        (merge-obs arg-objs
                   :type (glsl-resolve-func-type func args)
                   :current-line (gen-function-string func arg-objs))
        (error 'no-valid-function :name func-name 
               :types (mapcar #'code-type arg-objs)))))

(defun varjo->glsl (code env)
  (cond ((or (null code) (eq t code)) (compile-bool code env))
        ((numberp code) (compile-number code env))
        ((symbolp code) (compile-symbol code env))
        ((listp (first code)) (compile-form code env))
        (t (error 'cannot-compile :code code))))


