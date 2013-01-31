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

(defmacro defshader (name (&rest args) &body code)  
  `(let ((source (translate ',args ',code)))
     ,(if (eq name :test)
	  `source
	  (if (keywordp name)
	      (error "Cannot define shader with keyword name")
	      `(defun ,name ()
		 source)))))

;; [TODO] Position doesnt work if &uniform is in another package
(defun parse-shader-args (args)
  (let* ((uni-pos (symbol-name-position '&uniform args))
	 (context-pos (symbol-name-position '&context args))
	 (in-vars (subseq args 0 (or uni-pos context-pos)))
	 (uniforms-raw (when uni-pos (subseq args (1+ uni-pos)
					     context-pos)))
	 (uniforms (mapcar #'uniform->var uniforms-raw))
	 (uniform-defaults (mapcar #'uniform-default-val 
				   uniforms-raw))
	 (context (when context-pos (subseq args 
					    (1+ context-pos)))))
    (declare (ignore uniform-defaults))
    (destructuring-bind (&key (type :vertex) (version :330))
	context
      (when (and (check-arg-forms uniforms)
		 (check-arg-forms in-vars))
	(let* ((fleshed-out-in-vars (flesh-out-args in-vars))
	       (in-var-structs-and-types 
		 (create-fake-structs-from-in-vars
		  fleshed-out-in-vars))
	       (in-var-struct-type-maps
		 (mapcar #'first in-var-structs-and-types))
	       (in-var-struct-types 
		 (mapcar #'first in-var-struct-type-maps))
	       (in-var-struct-functions
		 (mapcan #'second in-var-structs-and-types))
	       (fleshed-out-uniforms (flesh-out-args uniforms))
	       (uniform-struct-types (get-uniform-struct-types
				      fleshed-out-uniforms))
	       (uniform-struct-definitions
		 (when uniform-struct-types
		   (get-struct-definitions uniform-struct-types)))
	       (uniform-struct-functions 
		 (mapcan #'struct-funcs 
			 uniform-struct-definitions)))
	  (list type version
		(substitute-alternate-struct-types
		 fleshed-out-in-vars in-var-struct-type-maps)
		(expand-struct-in-vars fleshed-out-in-vars)
		fleshed-out-uniforms
		(append in-var-struct-functions
			uniform-struct-functions)
		uniform-struct-definitions
		(append in-var-struct-types
			uniform-struct-types)))))))

(defun translate (args code)
  (destructuring-bind (shader-type version in-vars 
		       in-var-declarations uniform-vars
		       struct-functions struct-definitions types)
      (parse-shader-args args)
    (let* ((*shader-context* (list :core shader-type))
	   (*types* (acons-many (loop for i in types
				      collect (list i nil)) 
				*built-in-types*))
	   (*glsl-variables* (append (built-in-vars 
				      *shader-context*)
				     uniform-vars 
				     in-vars))
	   (*glsl-functions* (acons-many struct-functions 
					 *glsl-functions*))
	   (compiled-obj (compile-main code))
	   (compiled-in-vars (add-layout-qualifiers-to-in-vars
			      (compile-declarations
			       in-var-declarations '(:in))))
	   (compiled-uniforms (compile-declarations uniform-vars
						    '(:uniform))))
      (list (write-output-string version struct-definitions
				 compiled-obj compiled-in-vars
				 compiled-uniforms)
	    (out-vars compiled-obj)))))

(defun compile-main (code)
  (varjo->glsl (replace-literals 
		(macroexpand-and-substitute 
		 `(%make-function :main () ,@code)))))

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


;;------------------------------------------------------------


(defun check-arg-forms (in-args)
  (loop for stream in in-args
	:do (when  (not (eq 2 (length stream)))
	      (error "Declaration ~a is badly formed.~%Should be (-var-name- -var-type-)" stream)))
  t)

(defun uniform-default-val (x)
  (when (consp (first x)) (second x)))

(defun uniform->var (x)
  (if (uniform-default-val x) (first x) x))

(defun flesh-out-args (in-vars)
  (loop for var in in-vars
	:collect (list (var-name var)
		       (flesh-out-type (var-type var)))))

(defun get-uniform-struct-types (uniforms) 
  (remove-if #'null
   (remove-duplicates
    (loop for u in uniforms
	  :if (not (type-built-inp (var-type u)))
	    :collect (type-principle (var-type u))))))

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
	:collect (list 
		  (qualify ob (fmt "layout(location=~a)" total))
		  total)
	:do (setf total (+ total (layout-size (code-type ob))))))

(defun compile-declarations (vars &optional qualifiers)
  (loop for var in vars
	:collect (end-line
		  (%compile-var (var-name var) (var-type var)
				qualifiers))))
