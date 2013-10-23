(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun vlambda (&key in-args output-type transform
                  context-restriction (packageless t))
  (list (mapcar #'flesh-out-type
                (mapcar #'second in-args))
        (flesh-out-type output-type)
        transform
        (mapcar #'(lambda (x) (find :compatible x)) in-args)
        (mapcar #'(lambda (x) (find :match x)) in-args)
        context-restriction))

(defun func-in-spec (x)
  (first x))

(defun func-out-spec (x)
  (second x))

(defun func-body (x)
  (third x))

(defun func-compatible-args (x)
  (fourth x))

(defun func-args-match (x)
  (fifth x))

(defun func-restriction (x)
  (sixth x))

(defun func-packageless (x)
  (seventh x))

(defun glsl-valid-function-args (func args)
  (let ((in-spec (func-in-spec func))
        (types (mapcar #'code-type args)))
    (and (eq (length args) (length in-spec))
         (every #'(lambda (c s) (if (get-place s)
                                    (get-place c)
                                    t)) types in-spec)
         (every #'(lambda (c s) (glsl-typep c s)) 
                args in-spec)
         (apply #'types-compatiblep
                (identity-filter types (func-compatible-args func)))
         (let* ((filtered-types (identity-filter 
                                 types (func-args-match func)))
                (comp (first filtered-types)))
           (notany #'null (mapcar #'(lambda (x)
                                      (type-equal x comp)) 
                                  filtered-types))))))

(defun glsl-resolve-func-type (func args)
  ;; return the output type spec except for where 
  ;; the spec part is a number, in which case we 
  ;; take that part from the number'th in-arg.
  ;; Note that in cases where the args are meant
  ;; to be compatible that means we need to take
  ;; it from the superior in-arg type
  (let* ((in-types (mapcar #'code-type args))
         (superior (apply #'superior-type 
                          (identity-filter 
                           in-types (func-compatible-args func))))
         (made-type
          (loop :for i in (func-out-spec func)
             :for part from 0
             :collect 
             (if (numberp i)
                 (nth part (if (nth i (func-compatible-args
                                       func))
                               superior
                               (nth i in-types)))
                 (if (consp i) (first i) i))))
         (final-type (flesh-out-type made-type)))
    final-type))

(defun oper-segment-list (list symbol)
  (if (rest list) 
      (list symbol 
            (first list) 
            (oper-segment-list (rest list) symbol)) 
      (first list)))



(defun glsl-multi-defun (&key name specs transform context-restriction)
  (let ((*types* *built-in-types*))
    (loop :for spec :in specs :do
       (destructuring-bind (&key in out) spec
         (let* ((func-spec (vlambda :in-args in
                                    :output-type out
                                    :transform transform
                                    :context-restriction 
                                    context-restriction)))
           (setf *glsl-functions*
                 (acons name (cons func-spec
                                   (assocr name *glsl-functions*
                                           :test #'symbol-name-equal))
                        *glsl-functions*)))))))

(defun glsl-defun (&key name in-args output-type
                     transform context-restriction)
  (let ((*types* *built-in-types*))
    (let* ((func-spec (vlambda :in-args in-args 
                               :output-type output-type
                               :transform transform
                               :context-restriction 
                               context-restriction)))
      (setf *glsl-functions*
            (acons name (cons func-spec
                              (assocr name *glsl-functions*
                                      :test #'symbol-name-equal))
                   *glsl-functions*)))))



(defun context-ok-given-restriction (context restriction)
  (every #'identity
         (loop :for item :in restriction :collect
            (if (listp item)
                (some #'identity (loop :for sub-item :in item :collect
                                    (find sub-item context)))
                (find item context)))))

(defun func-valid-for-contextp (context func)
  (let ((restriction (func-restriction func)))
    (if restriction
        (when (context-ok-given-restriction context restriction)
          func)
        func)))

(defun func-specs (name)
  (let ((all-matching (assocr name *glsl-functions* 
                              :test #'symbol-name-equal)))
    (remove-if 
     #'null (loop for spec in all-matching
               :collect (func-valid-for-contextp 
                         *shader-context* spec)))))

(defun vfunctionp (name)
  (not (null (func-specs name))))

(defun special-functionp (symbol)
  (not (null (assoc symbol *glsl-special-functions*
                    :test #'symbol-name-equal))))

(defun apply-special (symbol arg-objs)
  (if (special-functionp symbol)
      (apply (assocr symbol *glsl-special-functions*
                     :test #'symbol-name-equal) arg-objs)
      (error "Varjo: '~a' is not a special function" symbol)))

(defun register-special-function (symbol function)
  (setf *glsl-special-functions* 
        (cons (cons symbol function) *glsl-special-functions*)))

(defmacro vdefspecial (name args &body body)
  `(register-special-function ',name #'(lambda ,args ,@body)))

(defun register-substitution (symbol function 
                              &optional packageless context-restrictions)
  (setf *glsl-substitutions*
        (acons symbol (list function packageless context-restrictions)
               *glsl-substitutions*)))
