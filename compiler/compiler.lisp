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
      (make-code-obj 'v-bool "true" :flow-ids (flow-id!))
      (make-code-obj 'v-bool "false" :flow-ids (flow-id!))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (cond ((floatp x) (type-spec->type 'v-float))
        ((integerp x) (type-spec->type 'v-int))
        (t (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (declare (ignore env))
  (let ((num-type (get-number-type code)))
    (make-code-obj num-type (gen-number-string code num-type)
		   :flow-ids (flow-id!))))

(defun v-variable->code-obj (var-name v-value from-higher-scope)
  (let ((code-obj (make-code-obj (v-type v-value)
				 (gen-variable-string var-name v-value)
				 :flow-ids (flow-ids v-value)
				 :place-tree `((,var-name ,v-value)))))
    (if from-higher-scope
        (add-higher-scope-val code-obj v-value)
        code-obj)))

(defun %v-value->code (v-val)
  (make-code-obj (v-type v-val) (v-glsl-name v-val)
		 :flow-ids (flow-ids v-val)))

;; [TODO] move error
(defun compile-symbol (code env)
  (let* ((var-name code)
         (v-value (get-var var-name env)))
    (if v-value
        (let* ((val-scope (v-function-scope v-value))
               (from-higher-scope (and (> val-scope 0)
                                       (< val-scope (v-function-scope env)))))
          (v-variable->code-obj var-name v-value from-higher-scope))
        (if (suitable-symbol-for-stemcellp var-name env)
            (let* ((scell (make-stem-cell code))
                   (assumed-type (funcall *stemcell-infer-hook* var-name)))
              (if assumed-type
                  (add-type-to-stemcell-code scell assumed-type)
                  scell))
            (error 'symbol-unidentified :sym code)))))

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


(defun compile-function-call (func-name func args env)
  (vbind (code-obj new-env)
      (cond
        ((v-special-functionp func) (compile-special-function func args env))

        ((multi-return-vars func) (compile-multi-return-function-call
                                   func-name func args env))

        (t (compile-regular-function-call func-name func args env)))
    (values code-obj (or new-env env))))

(defun calc-function-return-ids-given-args (func func-name arg-code-objs)
  ;; {TODO} This only makes sense for regular funcs, replace with more
  ;;        sensible check once have flowcontrol types
  (unless (<= (length (flow-ids func)) 1)
    (error 'multiple-flow-ids-regular-func func-name func))
  ;;
  (labels ((calc (id)
	     (let ((pos (position id (flow-ids func))))
	       (if pos
		   (flow-ids (elt arg-code-objs pos))
		   (flow-id!)))))
    (mapcar #'calc (in-arg-flow-ids func))))

(defun compile-regular-function-call (func-name func args env)
  (print (cons func-name (mapcar #'flow-ids args)))
  (let* ((c-line (gen-function-string func args))
         (type (resolve-func-type func args env))
	 (flow-ids (calc-function-return-ids-given-args func func-name args)))
    (unless type (error 'unable-to-resolve-func-type
                        :func-name func-name :args args))
    (merge-obs args
               :type type
               :current-line c-line
               :to-top (mapcan #'to-top args)
               :signatures (mapcan #'signatures args)
               :stemcells (mapcan #'stemcells args)
	       :flow-ids flow-ids
	       :place-tree (calc-place-tree func args))))

(defun calc-place-tree (func args)
  (when (v-place-function-p func)
    (let ((i (v-place-index func)))
      (cons (list func (elt args i)) (place-tree (elt args i))))))

(defun compile-multi-return-function-call (func-name func args env)
  (let* ((type (resolve-func-type func args env)))
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
	     (flow-ids (flow-ids func))
             (o (merge-obs
                 args :type type
                 :current-line (gen-function-string func args m-r-names)
                 :to-top (mapcan #'to-top args)
                 :signatures (mapcan #'signatures args)
                 :stemcells (mapcan #'stemcells args)
                 :multi-vals (mapcar (lambda (_ _1 fid)
                                       (make-mval
					(v-make-value
					 (v-type (slot-value _ 'value))
					 env :glsl-name _1 :flow-ids fid
					 :function-scope 0)))
                                     mvals
                                     m-r-names
				     flow-ids)
		 :flow-ids (mapcar #'flow-ids args)
		 :place-tree (calc-place-tree func args))))
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
;;{TODO} expand on this please. 'Future-you' couldnt work out what this meant
;; {TODO} you from both of your futures here. I think he was saying that
;;        the errors coming out of a special function could have been the result
;;        of the special func using #'varjo->glsl which tried compiling a
;;        function call but while testing for the right function it threw and
;;        error. I think that is wrong as the handler-case in compiler/functions
;;        should catch those. We need to review all this stuff anyway.
;;        In the case of special funcs there should never be any ambiguity, it
;;        HAS to be the correct impl
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
            (merge-obs obj :current-line (format nil "~a;" (current-line obj))
		       :flow-ids (flow-ids obj))))))

;; [TODO] this shouldnt live here
(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (stage-type :initarg :stage-type :accessor stage-type)
   (out-vars :initarg :out-vars :accessor out-vars)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)))
