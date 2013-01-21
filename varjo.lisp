;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;;------------------------------------------------------------
;; Translator
;;------------

(defun check-in-arg-forms (in-args)
  (loop for stream in in-args
	:do (when  (not (eq 2 (length stream)))
	      (error "Stream declaration ~a is badly formed.~%Should be (-var-name- -var-type-)" stream)))
  t)

(defun check-uniform-forms (uniforms)
  (loop for uniform in uniforms
	:do (when (not (eq 2 (length uniform)))
	      (error "Uniform declaration ~a is badly formed.~%Should be (-uniform-name- -uniform-type-) or ~%(-uniform-name- (-uniform-type- -default-value-))" uniform)))
  t)

(defun get-user-types-from-in-args (in-args)
  (remove-if #'null
	     (loop for arg in in-args
		   :collect (let ((type (if (symbolp (second arg))
					    (second arg)
					    (first (second arg)))))
			      (if (assoc type *built-in-types*)
				  nil
				  type)))))

(defun get-user-types-from-uniforms (uniforms)
  (remove-if #'null
	     (loop for arg in uniforms
		   :collect 
		   (let* ((form (second arg))
			  (type (flesh-out-type
				       (if (consp form)
					   (first form)
					   form)))
			  (priniciple (first type)))
		     (if (assoc priniciple *built-in-types*)
			 nil
			 priniciple)))))


(defmacro defshader (name type version (&rest args) &body code)  
  `(let ((source (translate ,type ,version ',args ',code)))
     ,(if (eq name :!test!)
	  `(first source)
	  (if (keywordp name)
	      (error "Cannot define shader with keyword name")
	      `(defun ,name ()
		 source)))))


(defun parse-shader-args (args)
  (let* ((uni-pos (position '&uniform args))
	 (in-vars (subseq args 0 uni-pos))
	 (uniforms (when uni-pos (group (subseq args (1+ uni-pos)) 
					2))))
   (when (and (check-uniform-forms uniforms)
	      (check-in-arg-forms in-vars))
     (let* ((fleshed-out-in-vars (flesh-out-in-vars in-vars))
	    (in-var-structs-and-types 
	      (create-fake-structs-from-in-vars
	       fleshed-out-in-vars))
	    (in-var-struct-type-maps
	      (mapcar #'first in-var-structs-and-types))
	    (in-var-struct-types 
	      (mapcar #'first in-var-struct-type-maps))
	    (in-var-struct-functions
	      (mapcan #'second in-var-structs-and-types))
	    (fleshed-out-uniforms (flesh-out-uniforms uniforms))
	    (uniform-struct-types (get-uniform-struct-types
				   fleshed-out-uniforms))
	    (uniform-struct-definitions
	      (when uniform-struct-types
		(get-struct-definitions uniform-struct-types)))
	    (uniform-struct-functions 
	      (mapcan #'struct-funcs uniform-struct-definitions)))
       (list (substitute-alternate-struct-types
	      fleshed-out-in-vars in-var-struct-type-maps)
	     (expand-struct-in-vars fleshed-out-in-vars)
	     (uniform-specs-to-vars fleshed-out-uniforms)
	     (append in-var-struct-functions
		     uniform-struct-functions)
	     uniform-struct-definitions
	     (append in-var-struct-types
		     uniform-struct-types))))))

(defun check-for-invalid-struct-types (types)
  (if (notany '#null )))

(defun flesh-out-in-vars (in-vars)
  (loop for var in in-vars
	:collect (list (var-name var)
		       (flesh-out-type (var-type var)))))

(defun flesh-out-uniforms (uniforms)
  (loop for uniform in uniforms
	:collect 
	(list (symb (first uniform))
	      (if (symbolp (second uniform))
		  (list (flesh-out-type (second uniform)))
		  (let ((form (second uniform)))
		    (list (flesh-out-type (first form))
			  (second form)))))))

(defun uniform-specs-to-vars (uniforms)
  (loop for u in uniforms :collect (list (uniform-name u)
					 (uniform-type u))))

(defun uniform-name (form)
  (first form))
(defun uniform-type (form)
  (caadr form))
(defun uniform-default-val (form)
  (cadadr form))

(defun get-uniform-struct-types (uniforms) 
  (remove-if #'null
   (remove-duplicates
    (loop for u in uniforms
	  :if (not (type-built-inp (uniform-type u)))
	    :collect (type-principle (uniform-type u))))))

(defun translate (shader-type version args code)
  (destructuring-bind (in-vars in-var-declarations uniform-vars
		       struct-functions struct-definitions types)
      (parse-shader-args args)
    (let* ((*shader-context* (list :core shader-type))
	   (*types* (acons-many (loop for i in types
				      collect (list i nil)) 
				*built-in-types*))
	   (*glsl-variables* (append (built-in-vars *shader-context*)
				     uniform-vars 
				     in-vars))
	   (*glsl-functions* (acons-many struct-functions 
					 *glsl-functions*))
	   (compiled-obj (compile-main code))
	   (compiled-in-vars (compile-in-var-declarations
			      in-var-declarations))
	   (compiled-uniforms (compile-uniform-declarations 
			       uniform-vars)))
      (list
       (write-output-string version struct-definitions compiled-obj
			    compiled-in-vars compiled-uniforms)))))

(defun write-output-string (version struct-definitions
			    code in-vars uniforms)
  
  (if (or (to-block code) (current-line code))
      (error "The following code not written to output.~%~a~%~a"
	     (to-block code) (current-line code))
      (format 
       nil 
       "#version ~a~%~{~%~{~a~%~}~}" 
       version
       (list
	(mapcar #'struct-init-form struct-definitions)
	(mapcar #'(lambda (x) (current-line (first x))) in-vars)
	(mapcar #'current-line uniforms)
	(to-top code)))))

(defun compile-main (code)
  (varjo->glsl (replace-literals 
		(macroexpand-and-substitute 
		 `(%make-function :main () ,@code)))))

(defun expand-struct-in-vars (in-vars)  
  "Transforms struct invars into the component variables"
  (loop for i in in-vars
	:if (type-built-inp (var-type i))
	  :collect i
	:else
	  :append (fake-struct-vars (var-name i) 
				    (type-principle (var-type i)))))

(defun create-fake-structs-from-in-vars (in-vars) 
  "Transforms struct invars into the component variables"
  (remove-duplicates
   (loop for i in in-vars
	 :if (not (type-built-inp (var-type i)))
	   :collect (make-fake-struct (type-principle (var-type i))))
   :test #'equal))

(defun substitute-alternate-struct-types (in-vars type-alist)
  (loop :for in-var in in-vars
	:collect (list (var-name in-var)
		       (or (first 
			    (assocr 
			     (type-principle (var-type in-var)) 
			     type-alist))
			   (var-type in-var)))))

(defun compile-var (name type &rest qualifiers)
  (let ((ob (varjo->glsl
	    `(%in-typify 
	      (%make-var ,name ,(flesh-out-type type))))))
    (merge-obs ob :current-line (format nil "~(~{~a ~}~)~a" 
					qualifiers
					(current-line ob)))))

(defun struct-layout-size (struct)
  (let ((slots (rest struct)))
    (apply #'+ (mapcar #'(lambda (slot) 
			   (glsl-layout-size (second slot)))
		       slots))))

(defun layout-size (type-spec)
  (let* ((type (flesh-out-type type-spec))
	 (principle (first type))
	 (length (second type))
	 (size (assocr principle *glsl-type-sizes*)))
    (cond ((null length) size)
	  ((integerp length) (* size length))
	  (t (error "Cannot have array of unknown size as an input")))))

(defun add-layout-qualifiers-to-in-vars (compiled-in-vars)
  (loop for ob in compiled-in-vars
	:with total = 0	
	:collect (list (qualify ob (format nil "layout(location=~a)"
					   total))
		  total)
	:do (setf total (+ total (layout-size (code-type ob))))))

(defun qualify (obj &rest qualifiers)
  (merge-obs obj
	     :current-line (format nil "~(~{~a ~}~)~a" 
				   qualifiers 
				   (current-line obj))))

(defun compile-in-var-declarations (vars)
  (add-layout-qualifiers-to-in-vars
   (loop for var in vars
	 :collect (qualify (compile-var (var-name var)
					(var-type var)) 
			   :in))))

(defun compile-uniform-declarations (vars)
  (loop for var in vars
	:collect (qualify (compile-var (uniform-name var) 
				       (uniform-type var)) 
			  :uniform)))

(defun varjo->glsl (varjo-code)
  (cond ((null varjo-code) nil)
	((typep varjo-code 'code) varjo-code)
	((atom varjo-code) 
	 (if (assoc varjo-code *glsl-variables*)
	     (instance-var varjo-code)
	     (error "Varjo: '~s' is unidentified." varjo-code)))
	((special-functionp (first varjo-code)) 
	 (apply-special (first varjo-code) (rest varjo-code)))
	((vfunctionp (first varjo-code))
	 (compile-function (first varjo-code) (rest varjo-code)))
	(t (error "Function '~s' is not available for ~A shaders in varjo." (first varjo-code) *shader-context*))))


(defun compile-function (func-name args)
  (let ((func-specs (func-specs func-name))
	(arg-objs (mapcar #'varjo->glsl args)))
    (loop :for f-spec :in func-specs 
       :if (glsl-valid-function-args f-spec arg-objs )
       :return (merge-obs arg-objs
                :type (glsl-resolve-func-type f-spec arg-objs)
                :current-line (apply #'format 
				     (append 
				      (list nil (func-body f-spec))
				      (mapcar #'current-line
					      arg-objs))))
       :finally (error "There is no applicable method for the glsl function '~s'~%when called with argument types:~%~s " func-name (mapcar #'code-type arg-objs)))))

;;------------------------------------------------------------

(defun macroexpand-and-substitute (varjo-code)
  (cond ((null varjo-code) nil)
	((listp varjo-code) 
	 (let ((sub (substitution (first varjo-code)))) 
	   (if sub
	       (mapcar #'macroexpand-and-substitute
		       (apply sub (rest varjo-code)))
	       (mapcar #'macroexpand-and-substitute
		       varjo-code))))
	(t varjo-code)))

;; [TODO] How should we specify unsigned?
(defun replace-literals (varjo-code)
  (cond ((null varjo-code) nil)	 
	((eq t varjo-code) 
	 (make-instance 'code :current-line "true" 
			      :type '(:bool nil)))
	((numberp varjo-code) 
	 (make-instance 'code :current-line (format nil "~a" 
						    varjo-code)
			      :type (get-number-type varjo-code)))
	((listp varjo-code) (mapcar #'replace-literals varjo-code))
	(t varjo-code)))

(defun get-number-type (x)
  (cond ((floatp x) '(:float nil))
	((integerp x) '(:int nil))
	(t (error "Varjo: Do not know the type of the number '~s'"
		  x))))
