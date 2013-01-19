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

(defparameter *test-code* 
  '(let (((temp :vec4) (m* world-to-camera-matrix 
			(m*v model-to-world-matrix position))))
    (out ((:gl gl-position) (* camera-to-clip-matrix temp))
     ((vec4 interpColor :smooth) color))))

(defmacro t! (code &key shader-type in-vars in-structs)
  `(translate ',code :in-vars ,in-vars :shader-type ,shader-type
	      :in-structs ,in-structs))

(defun translate (varjo-code &key shader-type in-vars in-structs 
			       (version :330))
  (let ((*shader-type* shader-type)
	(*glsl-variables* (append in-vars
				  (assocr :core *built-in-vars*)
				  (assocr shader-type
					  *built-in-vars*)))
	(*glsl-functions* (append *glsl-functions*
				  (mapcan #'struct-funcs
					  in-structs))))
    (let ((compiled-obj (varjo->glsl 
			 (replace-literals 
			  (macroexpand-and-substitute 
			   (list 'progn varjo-code))))))
      (code-object-to-string compiled-obj version in-structs))))

(defun code-object-to-string (code-obj version in-structs)
  (format 
   nil 
   "#version ~a~%~%~{~a~%~}~{~a~%~}~%void main() {~%~{~a~%~}~@[~a;~%~]}" 
   version
   (mapcar #'struct-init-form in-structs)
   (to-top code-obj)
   (to-block code-obj)
   (current-line code-obj)))

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
  (when (null varjo-code) (print "wooo"))
  (cond ((null varjo-code) 
	 (make-instance 'code :current-line "false" 
			      :type '(:bool nil)))	 
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
