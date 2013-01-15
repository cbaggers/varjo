;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(defun glsl-defun (&key name in-args output-type transform)
  (let* ((func-spec (list (mapcar #'flesh-out-type
				  (mapcar #'second in-args))
			  (flesh-out-type output-type)
			  transform)))
    (setf (gethash name *glsl-functions*) 
          (cons func-spec (gethash name *glsl-functions*)))))

(defun special-functionp (symbol)
  (not (null (gethash symbol *glsl-special-functions*))))

(defun funcall-special (symbol arg-objs)
  (funcall (gethash symbol *glsl-special-functions*)
	   arg-objs))

(defun register-special-function (symbol function)
  (setf (gethash symbol *glsl-special-functions*) 
	function))

(defmacro vdefspecial (name (code-var) &body body)
  `(register-special-function
    ',name
    (lambda (,code-var)
      ,@body)))

(defun register-substitution (symbol function)
  (setf (gethash symbol *glsl-substitutions*) 
	function))

(defmacro vdefmacro (name lambda-list &body body)
  `(register-substitution
    ',name
    (lambda ,lambda-list
      ,@body)))

(defun varjo-type->glsl-type (type)
  (let ((principle (first type))
	(structure (second type))
	(len (third type)))
    (if (eq structure :array)
	(format nil "~a[~a]" principle (if len len ""))
	(format nil "~a" principle))))

;;------------------------------------------------------------
;; Core Language Definitions
;;---------------------------

(glsl-defun :name 'degrees
            :in-args '((radians ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "degrees(~a)")

(glsl-defun :name 'radians
            :in-args '((degrees ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "radians(~a)")

(glsl-defun :name 'sin
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "sin(~a)")

(glsl-defun :name 'cos
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "cos(~a)")

(glsl-defun :name 'tan
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "tan(~a)")

(glsl-defun :name 'asin
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "asin(~a)")

(glsl-defun :name 'acos
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "acos(~a)")

(glsl-defun :name 'atan
            :in-args '((y ((:float :vec2 :vec3 :vec4)))
		       (x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "atan(~a, ~a)")

(glsl-defun :name 'atan
            :in-args '((y-over-x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "atan(~a)")

(glsl-defun :name 'sinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "sinh(~a)")

(glsl-defun :name 'cosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "cosh(~a)")

(glsl-defun :name 'tanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "tanh(~a)")

(glsl-defun :name 'asinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "asinh(~a)")

(glsl-defun :name 'acosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "acosh(~a)")

(glsl-defun :name 'atanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "atanh(~a)")

(glsl-defun :name 'pow
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "pow(~a, ~a)")

(glsl-defun :name 'exp
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "exp(~a)")

(glsl-defun :name 'log
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "log(~a)")

(glsl-defun :name 'exp2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "exp2(~a)")

(glsl-defun :name 'log2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "log2(~a)")

(glsl-defun :name 'sqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "exp(~a)")

(glsl-defun :name 'inversesqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "inversesqrt(~a)")

;;------------------------------------------------------------
;; Special Function
;;------------------

(vdefspecial %progn (varjo-code)    
  (let ((arg-objs (mapcar #'varjo->glsl varjo-code)))
    (if (eq 1 (length arg-objs))
	(car arg-objs)
	(let ((last-arg (car (last arg-objs)))
	      (args (subseq arg-objs 0 (- (length arg-objs) 1))))
	  (make-instance 
	   'code 
	   :type (code-type last-arg)
	   :current-line (current-line last-arg)
	   :to-block (remove-if 
		      #'null
		      (append
		       (mapcan #'(lambda (x) 
				   (list (to-block x)
					 (current-line x))) 
			       args)
		       (list (to-block last-arg))))
	   :to-top (mapcan #'to-top arg-objs))))))

(vdefspecial %typify (varjo-code)    
  (let ((arg-objs (mapcar #'varjo->glsl varjo-code)))
    (if (> (length arg-objs) 1)
	(error "Typify cannot take more than one arg")
	(let* ((arg (car arg-objs))
	       (type (code-type arg)))
	  (make-instance 
	   'code 
	   :type type
	   :current-line (format nil "~a ~a" 
				 (varjo-type->glsl-type type)
				 (current-line arg))
	   :to-block (to-block arg)
	   :to-top (to-top arg))))))


(vdefspecial %make-var (varjo-code)  
  (if (> (length varjo-code) 1)
      (error "Make-var cannot take more than one form")
      (let ((form (first varjo-code)))
	(if (listp form)
	    (let ((name (first form))
		  (type (second form)))
	      (make-instance 'code 
			     :type type
			     :current-line (format nil "~a" name)))
	    (make-instance 'code 
			   :type :unknown
			   :current-line (format nil "~a" form))))))

;; check for name clashes between forms
;; create init forms, for each one 

(vdefspecial let (varjo-code)
  (labels ((var-name (form) 
	     (if (listp (first form)) (first (first form))
		 (first form)))
	   (var-type (form) 
	     (when (listp (first form))
	       (flesh-out-type (second (first form)))))
	   (val (form) 
	     (second form))
	   (compile-form (name type value)
	     (varjo->glsl `(%typify (setf (%make-var (,name ,type))
					  ,value)))))
    (let* ((form-code (first varjo-code))
	   (body-code (rest varjo-code))	 
	   (val-objs (loop :for form in form-code
			   :collect (varjo->glsl (val form))))
	   (var-names (mapcar #'var-name form-code))
	   (var-gl-names (mapcar #'glsl-gensym var-names))
	   (var-types (loop :for form :in form-code
			      :for obj :in val-objs
			      :collect (or (var-type form)
					   (code-type obj))))
	   (form-objs (mapcar #'compile-form 
				var-gl-names var-types val-objs))
	   (*glsl-variables*
	       (append (mapcar #'list 
			       var-names var-types var-gl-names)
		       *glsl-variables*)))
      (let* ((prog-ob (funcall-special '%progn body-code)))
	(make-instance 'code
		       :type (code-type prog-ob)
		       :current-line (current-line prog-ob)
		       :to-block (append 
				  (mapcan #'to-block form-objs)
				  (mapcar #'current-line form-objs)
				  (to-block prog-ob))
		       :to-top (append 
				(mapcan #'to-top form-objs)
				(to-top prog-ob)))))))

(vdefspecial setf (varjo-code)  
  (if (> (length varjo-code) 2)
      (error "varjo setf can only set one var")
      (let* ((setf-form (mapcar #'varjo->glsl varjo-code))
	     (var (first setf-form))
	     (val (second setf-form))
	     (type (if (equal (code-type var) (code-type val))
		       (code-type var)
		       (if (glsl-typep var '(:unknown nil nil))
			   (code-type val)
			   (error "Types of variable and value do not match ~s ~s" (code-type var) (code-type val))))))
	(make-instance 'code
		       :type type
		       :current-line (format nil "~a = ~a"
					     (current-line var) 
					     (current-line val))
		       :to-block (mapcan #'to-block setf-form)
		       :to-top (mapcan #'to-top setf-form)))))

(vdefspecial + (varjo-code)    
  (let* ((arg-objs (mapcar #'varjo->glsl varjo-code))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(make-instance 'code
		       :type (apply #'superior-type types)
		       :current-line (format nil "(~{~a~^ ~^+~^ ~})"
					     (mapcar #'current-line 
						     arg-objs))
		       :to-block (mapcan #'to-block arg-objs)
		       :to-top (mapcan #'to-top arg-objs))
	(error "The types of object passed to + are not compatible~%~{~s~^ ~}" types))))

(vdefspecial %- (varjo-code)    
  (let* ((arg-objs (mapcar #'varjo->glsl varjo-code))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(make-instance 'code
		       :type (apply #'superior-type types)
		       :current-line (format nil "(~{~a~^ ~^-~^ ~})"
					     (mapcar #'current-line 
						     arg-objs))
		       :to-block (mapcan #'to-block arg-objs)
		       :to-top (mapcan #'to-top arg-objs))
	(error "The types of object passed to - are not compatible~%~{~s~^ ~}" types))))

(vdefspecial %negate (varjo-code)  
  (if (> (length varjo-code) 1)
      (error "Negate cannot take more than one form")
      (let* ((arg-obj (varjo->glsl (first varjo-code))))
	(make-instance 'code
		       :type (code-type arg-obj)
		       :current-line (format nil "-~a"
					     (current-line arg-obj))
		       :to-block (to-block arg-obj)
		       :to-top (to-top arg-obj)))))

(vdefspecial %* (varjo-code)
  (let* ((arg-objs (mapcar #'varjo->glsl varjo-code))
	 (types (mapcar #'code-type arg-objs)))
    (if (apply #'types-compatiblep types)
	(make-instance 'code
		       :type (apply #'superior-type types)
		       :current-line (format nil "(~{~a~^ ~^*~^ ~})"
					     (mapcar #'current-line 
						     arg-objs))
		       :to-block (mapcan #'to-block arg-objs)
		       :to-top (mapcan #'to-top arg-objs))
	(error "The types of object passed to * are not compatible~%~{~s~^ ~}" types))))

;;------------------------------------------------------------
;; Lisp Function Substitutions
;;-----------------------------

;; (vdefmacro + (&rest args)
;;   (oper-segment-list args '%+))

(vdefmacro - (&rest args)
  (if (eq 1 (length args))
      `(%negate ,@args)
      `(%- ,@args)))

(vdefmacro * (&rest args)
  (oper-segment-list args '%*))

(vdefmacro / (&rest args)
  (oper-segment-list args '%/))

