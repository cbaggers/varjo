(in-package :varjo)

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
         (args-code (rest code))
         (f-result (find-function-for-args func-name args-code env)))
    (multiple-value-bind (code-obj new-env)
        (cond ((listp f-result)
               (destructuring-bind (func args) f-result
                 (if (v-special-functionp func)                 
                     (glsl-resolve-special-func-type func args env)
                     (merge-obs args :type (glsl-resolve-func-type func args)
                                :current-line (gen-function-string func args)))))
              ((typep f-result 'deferred-error) (raise-deffered-error f-result)
               (error 'problem-with-the-compiler)))
      (values code-obj (or new-env env)))))

(defun varjo->glsl (code env)
  (values (cond ((or (null code) (eq t code)) (compile-bool code env))
                ((numberp code) (compile-number code env))
                ((symbolp code) (compile-symbol code env))
                ((listp code) (compile-form code env))
                (t (error 'cannot-compile :code code)))
          env))

;;----------------------------------------------------------------------
;; Compiler Helper Functions

;; (let ((a 1)
;;       (b (+ 1 2))
;;       (c)
;;       ())
;;   (do-stuff))

;; [TODO] will we ever need qualifiers here?
;; [TODO] move the error
(defun compile-let-forms (forms env &optional qualify-types)
  (let ((env (clone-environment env)))
    (list
     (loop :for form :in forms :if form :collect
        (destructuring-bind ((name &optional type-spec qualifiers) var-form) 
            (list (listify (first form)) (second form))
          (declare (ignore qualifiers))
          ;; 
          ))
     env)))

;; need case for no value
;; (varjo->glsl `(%typify (setf (%make-var ,name ,type) ,value)))
;; (varjo->glsl `(setf (%make-var ,name ,type) ,value)))

;; (let* ((var-form )
;;        (form-code (if qualify-types
;;                       `(%qual-type ,name )
;;                       (varjo->glsl var-form env)))
;;        (type (when type-spec (type-spec->type type-spec))))
;;   (unless (or (null type) (eq (v-type-name (code-type form-code))
;;                               (v-type-name type)))
;;     (error "Type mismatch between var spec and inferred type:~%~s -v- ~s" (code-type form-code) type))

;;   (add-var name (make-instance 'v-value :type (or type form-code)) 
;;            env t)
;;   form-code)
