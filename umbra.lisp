;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :umbra)

;;------------------------------------------------------------
;; Translator
;;------------

(defun translate-cglsl (cglsl-code)
  (cglsl->glsl
   (macroexpand-and-substitute
    (replace-numbers cglsl-code))))

(defun cglsl->glsl (cglsl-code &optional (shader-type :vertex))
  (cond 
    ((null cglsl-code) nil)
    ((typep cglsl-code 'code) cglsl-code)
    ((atom cglsl-code) (error "'~s' is unidentified." cglsl-code))
    ((glsl-operatorp (first cglsl-code))
     (glsl-compile-operator 
      (first cglsl-code) (mapcar #'cglsl->glsl (rest cglsl-code))))
    ((glsl-function (first cglsl-code))
     (glsl-compile-function
      (first cglsl-code) (mapcar #'cglsl->glsl (rest cglsl-code))))
    (t (error "Function '~s' is not available for ~A shaders in cglsl." (first cglsl-code) shader-type))))

;;------------------------------------------------------------

(defun glsl-compile-operator (op-name arg-objs)
  (let* ((arg-code (mapcar #'code arg-objs))
	 (code 
	   (cond ((> (length arg-objs) 1)
		  (format nil 
			  (format nil "(~~{~~s ~~^~s ~~})" op-name)
			  arg-code))
		 ((eq 1 (length arg-objs)) 
		  (if (glsl-unary-operator op-name)
		      (format nil "(~s ~s)" op-name (car arg-code))
		      (error "The GLSL operator '~s' is not an unary operator. It must take at least 2 arguments" op-name)))
		 (t (error "Invalid number of arguments ~s for operator ~s" (length arg-objs) op-name)))))
    (make-instance 'code 
		   :type (glsl-resolve-oper-type op-name arg-objs)
		   :code code
		   :to-block (mapcan #'code-to-block arg-objs)
		   :to-top (mapcan #'code-to-top arg-objs))))

;;------------------------------------------------------------

;; expects code objects 
(defun glsl-compile-function (func-name arg-objs)
  (let ((func-specs (gethash func-name *glsl-functions*)))
    (loop :for f-spec :in func-specs 
	  :if (glsl-valid-function-args f-spec arg-objs )
	    :return (make-instance 
		     'code 
		     :type (glsl-resolve-func-type f-spec arg-objs)
		     :code (when (not (func-block f-spec))
			     (apply 
			      #'format 
			      (append (list nil (func-body f-spec))
				      (mapcar #'code arg-objs))))
		     :to-block (append (mapcan #'code-to-block
					       arg-objs)
				       (when (func-block f-spec)
					 (mapcar #'code arg-objs)))
		     :to-top (mapcan #'code-to-top arg-objs))
	  :finally (error "There is no applicable method for the ~%glsl function '~s'~%when called with argument types: ~s " func-name (mapcar #'code-type arg-objs)))))

;;------------------------------------------------------------

(defun macroexpand-and-substitute (cglsl-code)
  (cond ((null cglsl-code) nil)
	((listp cglsl-code) 
	 (let ((sub (gethash (first cglsl-code)
			     *glsl-substitutions*))) 
	   (if sub
	       (mapcar #'macroexpand-and-substitute
		       (apply sub cglsl-code))
	       (mapcar #'macroexpand-and-substitute
		       cglsl-code))))
	(t cglsl-code)))

(defun replace-numbers (cglsl-code)
  (cond ((null cglsl-code) nil)
	((numberp cglsl-code) (make-instance 'code 
				    :code cglsl-code
				    :type (get-number-type 
					   cglsl-code)))
	((listp cglsl-code) (mapcar #'replace-numbers cglsl-code))
	(t cglsl-code)))

(defun get-number-type (x)
  (cond ((floatp x) '(:float nil nil))
	(t '(:integer nil nil))))
