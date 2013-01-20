;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; (progn (ql:quickload :varjo) (in-package :varjo))

(in-package :varjo)

;;------------------------------------------------------------
;; Translator
;;------------

(defun parse-shader-args (args)
  "Takes a list of arguments for a shader and extracts the
   version, stream declarations and uniform declarations.
   It also runs some sanity checks to make sure the contents
   are well-formed."
  ;; extract details
  (let* ((uni-pos (position-if #'keywordp args))
	 (version (when (keywordp (first args))
		    (first args)))
	 (start-index (if version 1 0))
	 (streams (subseq args start-index uni-pos))
	 (uniforms (when uni-pos
		     (group (subseq args uni-pos) 2))))
    ;; sanity check streams
    (loop for stream in streams
	  :do (when (or (not (eq 2 (length stream))))
		(error "Stream declaration ~a is badly formed.~%Should be (-var-name- -var-type-)" stream)))
    ;; sanity check uniforms
    (loop for uniform in uniforms
	  :do (when (or (not (eq 2 (length uniform)))
			(and (not (keywordp (second uniform)))
			     (not (consp (second uniform))))
			(when (consp (second (second uniform)))
			  (not (eq 2 (length (second
					      (second uniform)))))))
		(error "Uniform declaration ~a is badly formed.~%Should be (-uniform-name- -uniform-type-) or ~%(-uniform-name- (-default-value- -uniform-type-))" uniform)))
    ;; thats all folks!
    (list (or version -default-version-) streams uniforms)))

(defmacro %defshader (name type (&rest args) &body code)
  "The debugging version of defshader"
  (declare (ignore name))
  (destructuring-bind (version streams uniforms)
      (parse-shader-args args)
    `(translate '(progn ,@code) :in-vars ',streams 
				:uniforms ',uniforms
				:shader-type ,type
				:version ,version)))

(defmacro defshader (name type (&rest args) &body code)
  (destructuring-bind (version streams uniforms)
      (parse-shader-args args)
    `(let ((source ',(translate `(progn ,@code) :in-vars streams 
						:uniforms uniforms
						:shader-type type
						:version version)))
       (defun ,name ()
	 source))))

(defun translate (varjo-code &key shader-type in-vars in-structs 
			       (version :330) uniforms)
  (declare (ignore uniforms))
  (when (not (find shader-type *shader-types*))
    (error "Cannot create shader of type ~a.~%Must be one of the following: ~s" shader-type *shader-types*))
  (let ((*shader-type* shader-type)
	(*glsl-variables* (append (mapcar #'flesh-out-var in-vars)
				  (assocr :core *built-in-vars*)
				  (assocr shader-type
					  *built-in-vars*)))
	(*glsl-functions* (add-functions (mapcan #'struct-funcs
						 in-structs)
					 *glsl-functions*)))
    (let ((compiled-obj (varjo->glsl 
			 (replace-literals 
			  (macroexpand-and-substitute 
			   `(%make-function :main () 
					    ,varjo-code))))))
      (list (code-object-to-string compiled-obj version in-structs
				   in-vars)
	    (out-vars compiled-obj)))))

(defun flesh-out-var (var)
  (if (null var)
      (error "Cannot flesh out empty var")
      (let ((template '(*no-name* (:void) nil t)))
	(append var (subseq template (length var))))))

(defun code-object-to-string (code-obj version in-structs in-vars)
  
  (if (or (to-block code-obj) (current-line code-obj))
      (error "~%-------------~%~a~%~a~%-------------" 
	     (to-block code-obj)
	     (current-line code-obj))
      (format 
       nil 
       "#version ~a~%~%~{~a~%~}~%~{~a~%~}~{~a~%~}" 
       version
       (mapcar #'struct-init-form in-structs)
       (in-var-init-form in-vars)
       (to-top code-obj))))

(defun in-var-init-form (in-vars)
  (let ((in-obs 
	  (loop :for var in in-vars 
		:collect 
		(varjo->glsl 
		 `(%in-typify (%make-var ,(first var)
					 ,(flesh-out-type 
					   (second var))))))))
    (loop for ob in in-obs
	  :collect (format nil "in ~a;" (current-line ob)))))

(defun varjo->glsl (varjo-code)
  (cond 
    ((null varjo-code) nil)
    ((typep varjo-code 'code) varjo-code)
    ((atom varjo-code) 
     (if (assoc varjo-code *glsl-variables*)
	 (instance-var varjo-code)
	 (error "Varjo: '~s' is unidentified." varjo-code)))
    ((special-functionp (first varjo-code)) 
     (apply-special (first varjo-code) (rest varjo-code)))
    ((vfunctionp (first varjo-code))
     (compile-function (first varjo-code) (rest varjo-code)))
    (t (error "Function '~s' is not available for ~A shaders in varjo." (first varjo-code) *shader-type*))))

;;------------------------------------------------------------

;; expects code objects 
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
