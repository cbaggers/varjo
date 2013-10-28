(in-package :varjo)

;; (glsl-defun :name 'atan
;;             :in-args '((y ((:float :vec2 :vec3 :vec4)) :compatible)
;;                        (x ((:float :vec2 :vec3 :vec4)) :compatible))
;;             :output-type '(0 nil)
;;             :transform "atan(~a, ~a)"
;;             :context-restriction '((:330)))

;; reasons for match
;; two situations
;; 1 - two arguments that allow (int or vec) now is that something compat with 
;;     int and then something compat with vec OR
;; 2 - two things either int or vec but not combo
;; Address with NORMAL type declarations


(defun old-flesh-out-type (type-spec)
  (if (consp type-spec)
      (if (> (length type-spec) 4)
          (error "Invalid GLSL Type Definition: ~s has more than 4 components." type-spec)
          (list (first type-spec)
                (second type-spec)
                (third type-spec)
                (or (first type-spec)
                    (when (symbolp (first type-spec))
                      (safe-gl-name (first type-spec))))))
      (old-flesh-out-type (list type-spec))))

(defun old-vlambda (&key in-args output-type transform
                  context-restriction (packageless t))
  (declare (ignore packageless))
  (list (mapcar #'old-flesh-out-type
                (mapcar #'second in-args))
        (old-flesh-out-type output-type)
        transform
        (mapcar #'(lambda (x) (find :compatible x)) in-args)
        (mapcar #'(lambda (x) (find :match x)) in-args)
        context-restriction))

(defun glsl-defun (&key name in-args output-type
                     transform context-restriction)
  (let* ((func-spec (old-vlambda :in-args in-args 
                                 :output-type output-type
                                 :transform transform
                                 :context-restriction 
                                 context-restriction)))
    (setf *old-funcs*
          (acons name (cons func-spec
                            (assocr name *old-funcs*
                                    :test #'symbol-name-equal))
                 *old-funcs*))))

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun vlambda (&key arg-spec return-spec code context-restriction place)
  (make-instance 'v-function :arg-spec arg-spec :return-spec return-spec
                 :code code :restriction context-restriction :place place)) 

(defmacro v-defun (name args &body body)
  (let* ((context-pos (position '&jam args :test #'symbol-name-equal))
         (context (subseq args (1+ context-pos)))
         (args (subseq args 0 context-pos)))
    (if (stringp (first body))
        (destructuring-bind (transform arg-types return-spec &key place) body
          `(setf (gethash ',name (v-functions *global-env*)) 
                 '(,transform '(,args ,arg-types) ,return-spec ,context ,place)))
        `(setf (gethash ',name (v-functions *global-env*)) 
               (lambda ,args ,@body) ,context))))


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

;;------------------------------------------------------------

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

;;------------------------------------------------------------

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
        (when (context-ok-given-restriction context restriction) func)
        func)))

;;------------------------------------------------------------

(defun func-specs (name)
  (let ((all-matching (assocr name *glsl-functions* 
                              :test #'symbol-name-equal)))
    (remove-if 
     #'null (loop for spec in all-matching
               :collect (func-valid-for-contextp 
                         *shader-context* spec)))))

(defun vfunctionp (name env)
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
