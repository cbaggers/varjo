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
            ((typep code 'v-value) (%v-value->code code))
            (t (error 'cannot-compile :code code)))
    (values code (or new-env env))))

(defun expand->varjo->glsl (code env)
  "Special case generally used by special functions that need to expand
   any macros in the form before compiling"
  (pipe-> (code env)
    (equal #'symbol-macroexpand-pass
           #'macroexpand-pass
           #'compiler-macroexpand-pass)
    #'varjo->glsl))

(defun compile-bool (code env)
  (declare (ignore env))
  (if code
      (make-code-obj 'v-bool "true")
      (make-code-obj 'v-bool "false")))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (cond ((floatp x) (type-spec->type 'v-float))
        ((integerp x) (type-spec->type 'v-int))
        (t (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (declare (ignore env))
  (let ((num-type (get-number-type code)))
    (make-code-obj num-type (gen-number-string code num-type))))

(defun v-variable->code-obj (var-name v-value from-higher-scope)
  (let ((code-obj (make-code-obj (v-type v-value) (gen-variable-string var-name v-value))))
    (if from-higher-scope
        (add-higher-scope-val code-obj v-value)
        code-obj)))

(defun %v-value->code (v-val)
  (make-code-obj (v-type v-val) (v-glsl-name v-val)))

(defparameter *stemcell-infer-hook* (lambda (name) (declare (ignore name)) nil))

(defmacro with-stemcell-infer-hook (func &body body)
  (let ((func-name (gensym "hook")))
    `(let* ((,func-name ,func)
            (*stemcell-infer-hook* ,func-name))
       ,@body)))

;; [TODO] move error
(defun compile-symbol (code env)
  (let* ((var-name code)
         (v-value (get-var var-name env)))
    (if v-value
        (let* ((val-scope (v-function-scope v-value))
               (from-higher-scope (and (> val-scope 0)
                                       (< (v-function-scope env)))))
          (v-variable->code-obj var-name v-value from-higher-scope))
        (if (suitable-symbol-for-stemcellp var-name env)
            (let* ((scell (make-stem-cell code))
                   (assumed-type (funcall *stemcell-infer-hook* var-name)))
              (if assumed-type
                  (add-type-to-stemcell-code scell assumed-type)
                  scell))
            (error 'symbol-unidentified :sym code)))))

(defun suitable-symbol-for-stemcellp (symb env)
  (and (allows-stemcellsp env)
       (let ((str-name (symbol-name symb)))
         (and (char= (elt str-name 0) #\*)
              (char= (elt str-name (1- (length str-name)))
                     #\*)))))

(defun add-type-to-stemcell-code (code-obj type-name)
  (assert (stemcellp (code-type code-obj)))
  (let ((type (type-spec->type type-name)))
    (copy-code code-obj
               :type type
               :stemcells (let ((stemcell (first (stemcells code-obj))))
                            `((,(first stemcell)
                                ,(second stemcell)
                                ,type-name))))))

(defun compile-form (code env)
  (let* ((func-name (first code))
         (args-code (rest code)))
    (when (keywordp func-name)
      (error 'keyword-in-function-position :form code))
    (dbind (func args) (find-function-for-args func-name args-code env)
      (cond
        ((typep func 'v-function) (compile-function-call func-name func args env))
        ((typep func 'v-error) (if (v-payload func)
                                   (error (v-payload func))
                                   (error 'cannot-compile :code code)))
        (t (error 'problem-with-the-compiler :target func))))))

(defun make-stemcell-arguments-concrete (args func)
  (mapcar #'(lambda (arg actual-type)
              (if (stemcellp (code-type arg))
                  (let ((stemcell (first (stemcells arg))))
                    (copy-code arg
                               :type actual-type
                               :stemcells `((,(first stemcell)
                                              ,(second stemcell)
                                              ,(type->type-spec actual-type)))))
                  arg))
          args
          (v-argument-spec func)))

(defun compile-function-call (func-name func args env)
  (vbind (code-obj new-env)
      (cond
        ((v-special-functionp func) (compile-special-function func args env))

        ((multi-return-vars func) (compile-multi-return-function-call
                                   func-name func args env))

        (t (compile-regular-function-call func-name func args env)))
    (values code-obj (or new-env env))))

(defun compile-regular-function-call (func-name func args env)
  (let* ((args (make-stemcell-arguments-concrete args func))
         (c-line (gen-function-string func args))
         (type (resolve-func-type func args env)))
    (unless type (error 'unable-to-resolve-func-type
                        :func-name func-name :args args))
    (merge-obs args
               :type type
               :current-line c-line
               :to-top (mapcan #'to-top args)
               :signatures (mapcan #'signatures args)
               :stemcells (mapcan #'stemcells args))))

(defun compile-multi-return-function-call (func-name func args env)
  (let* ((args (make-stemcell-arguments-concrete args func))
         (type (resolve-func-type func args env)))
    (unless type (error 'unable-to-resolve-func-type :func-name func-name
                        :args args))
    (let* ((has-base (not (null (v-multi-val-base env))))
           (m-r-base (or (v-multi-val-base env)
                         (safe-glsl-name-string (free-name 'nc))))
           (mvals (multi-return-vars func))
           (start-index (if has-base 0 1))
           (m-r-names (loop :for i :from start-index
                         :below (+ start-index (length mvals)) :collect
                         (fmt "~a~a" m-r-base i))))
      (let* ((bindings (loop :for mval :in mvals :collect
                          `((,(free-name 'nc)
                              ,(type->type-spec
                                (v-type (slot-value mval 'value)))))))
             (o (merge-obs
                 args :type type
                 :current-line (gen-function-string func args m-r-names)
                 :to-top (mapcan #'to-top args)
                 :signatures (mapcan #'signatures args)
                 :stemcells (mapcan #'stemcells args)
                 :multi-vals (mapcar (lambda (_)
                                       (make-mval
                                        (make-instance
                                         'v-value
                                         :type (v-type (slot-value % 'value))
                                         :glsl-name _1)))
                                     mvals
                                     m-r-names))))
        (varjo->glsl
         `(%clone-env-block
           (%multi-env-progn
            ;; when has-base is true then a return or mvbind has already
            ;; written the lets for the vars
            ,@(unless has-base
                      (loop :for v :in bindings :for gname :in m-r-names
                         :collect `(%glsl-let ,v t ,gname))))
           ,o) env)))))

;;[TODO] Maybe the error should be caught and returned,
;;       in case this is a bad walk
;;{TODO} expand on this please. Future you couldnt work out what this meant
(defun compile-special-function (func args env)
  (let ((env (clone-environment env)))
    (multiple-value-bind (code-obj new-env)
        (handler-case (apply (v-return-spec func) (cons env args))
          (varjo-error (e) (invoke-debugger e)))
      (values code-obj (or new-env env)))))


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
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)))
