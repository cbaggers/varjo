(in-package :varjo)
(in-readtable fn:fn-reader)

(defmethod compile-form (code env)
  (labels ((ensure-env (code-obj &optional new-env)
             (let* ((new-env (or new-env env))
                    (code-with-meta (infer-meta code-obj new-env)))
               (values code-with-meta new-env))))
    ;;
    (multiple-value-bind (code-obj new-env)
        (compile-literal code env :errorp nil)
      (if code-obj ;; if it was a literal
          (ensure-env code-obj new-env)
          (multiple-value-call #'ensure-env
            (cond ((symbolp code) (compile-symbol code env))
                  ((and (listp code) (listp (first code)))
                   (error 'invalid-form-list :code code))
                  ((listp code) (compile-list-form code env))
                  ((typep code 'compiled) code)
                  (t (error 'cannot-compile :code code))))))))

(defmethod compile-place (code env &key allow-unbound)
  (labels ((ensure-env (code-obj &optional new-env)
             (let* ((new-env (or new-env env))
                    (code-with-meta (infer-meta code-obj new-env)))
               (values code-with-meta new-env))))
    ;;
    (multiple-value-call #'ensure-env
      (cond ((symbolp code) (compile-symbol
                             code env :allow-unbound allow-unbound))
            ((and (listp code) (listp (first code)))
             (error 'invalid-form-list :code code))
            ((listp code) (compile-list-form code env))
            (t (error 'cannot-compile :code code))))))

(defmethod compile-literal (code env &key (errorp t))
  (multiple-value-bind (code-obj new-env)
      (cond ((stringp code) (compile-string-literal code env))
            ((or (null code) (eq t code)) (compile-bool code env))
            ((numberp code) (compile-number code env))
            ((arrayp code) (compile-array-literal code env))
            (t :invalid))
    (if (eq code-obj :invalid)
        (when errorp
          (error 'cannot-compile :code code))
        (let* ((new-env (or new-env env))
               (code-with-meta (infer-meta code-obj new-env)))
          (values code-with-meta new-env)))))
