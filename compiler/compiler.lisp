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

(defun expand->varjo->glsl (code env)
  "Special case generally used by special functions that need to expand
   any macros in the form before compiling"
  (pipe-> (code env)
    (equal #'macroexpand-pass
           #'compiler-macroexpand-pass)
    #'varjo->glsl))

(defun compile-bool (code env)
  (declare (ignore env))
  (if code
      (make-instance 'code :current-line "true" :type 'v-bool)
      (make-instance 'code :current-line "false" :type 'v-bool)))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (cond ((floatp x) (type-spec->type 'v-float))
        ((integerp x) (type-spec->type 'v-int))
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
                 (let ((type (glsl-resolve-func-type func args env)))
                   (unless type (error 'unable-to-resolve-func-type
                                       :func-name func-name :args args))
                   (if (multi-return-vars func)                       
                       (compile-multi-return-func func-name func args 
                                                  type env)
                       (compile-func func-name func args type))))
           (push stemcells (stemcells code-obj))
           (values code-obj (or new-env env))))
          ((typep func 'v-error) (if (v-payload func)
                                     (error (v-payload func))
                                     (error 'cannot-compile :code code)))
          (t (error 'problem-with-the-compiler :target func))))))



(defun compile-func (func-name func args type)
  (let* ((c-line (gen-function-string func args)))
    (merge-obs args :type type :current-line c-line
               :to-top (append (second (v-required-glsl func)) 
                               (mapcan #'to-top args))
               :signatures (append (first (v-required-glsl func))
                                   (mapcan #'signatures args))
               :used-funcs (if (v-required-glsl func)
                               (cons func-name
                                     (mapcan #'used-external-functions args))
                               (mapcan #'used-external-functions args)))))

(defun compile-multi-return-func (func-name func args type env)  
  (let* ((m-r-base (or (v-multi-val-base env)
                       (safe-glsl-name-string (free-name 'nc))))
         (m-r-types (multi-return-vars func)))    
    (if (and (multi-return-vars func) (not (v-multi-val-base env)))
        (let* ((bindings (loop :for type :in m-r-types :collect
                            `((,(free-name 'nc) ,(type->type-spec type)))))
               (m-r-names (loop :for i :below (length m-r-types) :collect
                             (fmt "~a~a" m-r-base i)))         
               (o (merge-obs args :type type 
                             :current-line (gen-function-string func args m-r-names)
                             :to-top (append (second (v-required-glsl func)) 
                                             (mapcan #'to-top args))
                             :signatures (append (first (v-required-glsl func))
                                                 (mapcan #'signatures args))
                             :used-funcs (if (v-required-glsl func)
                                             (cons func-name
                                                   (mapcan #'used-external-functions args))
                                             (mapcan #'used-external-functions args)))))
          (expand->varjo->glsl 
           `(%clone-env-block
             (%env-multi-var-declare ,bindings t ,m-r-names)
             ,o) env))

        (let* ((m-r-names (loop :for i :below (1+ (length m-r-types)) :collect
                             (fmt "~a~a" m-r-base i)))
               (o (merge-obs
                   args :type type 
                   :current-line (gen-function-string func args (rest m-r-names))
                   :to-top (append (second (v-required-glsl func)) 
                                   (mapcan #'to-top args))
                   :signatures (append (first (v-required-glsl func))
                                       (mapcan #'signatures args))
                   :used-funcs (if (v-required-glsl func)
                                   (cons func-name
                                         (mapcan #'used-external-functions args))
                                   (mapcan #'used-external-functions args))))
               (bind `((,(free-name 'nr) ,o)))
               (c (varjo->glsl 
                   `(%clone-env-block
                     (%env-multi-var-declare ,bind nil (,(first m-r-names)))
                     (setf ,(caar bind) ,o)) 
                   env)))
          (merge-obs c
                     :multi-vals (cons (make-instance 'v-value :type type
                                                      :glsl-name (first m-r-names)) 
                                       (mapcar (lambda (x y) (make-instance 
                                                              'v-value :type x
                                                              :glsl-name y))
                                               m-r-types
                                               (rest m-r-names))))))))



(defun end-line (obj &optional force)
  (when obj
    (if (and (typep (code-type obj) 'v-none) (not force))
        obj
        (if (null (current-line obj))
            obj
            (merge-obs obj :current-line (format nil "~a;" (current-line obj)))))))

;; [TODO] this shouldnt live here
(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (stage-type :initarg :stage-type :accessor stage-type)
   (out-vars :initarg :out-vars :accessor out-vars)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (context :initarg :context :accessor context)
   (used-external-functions :initarg :used-external-functions 
                            :accessor used-external-functions)))
