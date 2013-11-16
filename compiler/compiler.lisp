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
  (print code)
  (let* ((func-name (first code)) 
         (args-code (rest code))
         (f-result (find-function-for-args func-name args-code env)))
    (multiple-value-bind (code-obj new-env)
        (cond ((listp f-result)
               (format t "at ~a~%" func-name)
               (destructuring-bind (func args) f-result
                 (if (v-special-functionp func)                 
                     (glsl-resolve-special-func-type func args env)
                     (merge-obs args :type (glsl-resolve-func-type func args)
                                :current-line (gen-function-string func args)))))
              ((typep f-result 'v-error) (if (v-payload f-result)
                                             (error (v-payload f-result))
                                             (error 'cannot-compile :code code)))
              (t (error 'problem-with-the-compiler)))
      (format t "--------------------~%")
      (values code-obj (or new-env env)))))
