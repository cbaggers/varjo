(in-package :varjo)

(defun varjo->glsl (code env)
  (multiple-value-bind (code new-env)
      (cond ((or (null code) (eq t code)) (compile-bool code env))
            ((numberp code) (compile-number code env))
            ((symbolp code) (compile-symbol code env))
            ((and (listp code) (listp (first code))) 
             (error 'cannot-compile :code code))
            ((listp code) (compile-form code env))
            ((typep code 'code) code)
            (t (error 'cannot-compile :code code)))
    (values code (or new-env env))))

(defun compile-bool (code env)
  (declare (ignore env))
  (if code
      (make-instance 'code :current-line "true" :type 'v-bool)
      (make-instance 'code :current-line "false" :type 'v-bool)))

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
                 :current-line (gen-variable-string var-name v-value)))

;; [TODO] move error
(defun compile-symbol (code env)
  (let* ((var-name code)
         (v-value (get-var var-name env)))
    (if v-value
        (v-variable->code-obj var-name v-value)
        (if (allows-stemcellsp env)
            (make-instance 'code :type 'v-stemcell 
                           :current-line (string (free-stemcell-name code)))
            (error "Varjo: Symbol '~s' is unidentified." code)))))

(defun compile-form (code env)
  (let* ((func-name (first code)) 
         (args-code (rest code)))
    (destructuring-bind (func args stemcells)
        (find-function-for-args func-name args-code env)
      (cond 
        ((typep func 'v-function)
         (multiple-value-bind (code-obj new-env)
             (if (v-special-functionp func) 
                 (glsl-resolve-special-func-type func args env)
                 (let ((type (glsl-resolve-func-type func args)))
                   (if type (merge-obs args :type type :current-line
                                       (gen-function-string func args))
                       (error 'unable-to-resolve-func-type :func-name func-name
                              :args args))))
           (push stemcells (stemcells code-obj))
           (values code-obj (or new-env env))))
          ((typep func 'v-error) (if (v-payload func)
                                     (error (v-payload func))
                                     (error 'cannot-compile :code code)))
          (t (error 'problem-with-the-compiler :target func))))))

;;[TODO] This is why break needs a semicolon
(defun end-line (obj)
  (when obj
    (if (typep (code-type obj) 'v-none)
        obj
        (if (null (current-line obj))
            (peek obj)
            (merge-obs obj :current-line (format nil "~a;" (current-line obj)))))))
