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

(defun translate (varjo-code)
  (varjo->glsl
   (macroexpand-and-substitute
    (replace-numbers varjo-code))))

(defun instance-var (symbol)
  (let ((var-spec (assoc symbol *glsl-variables*)))
    (make-instance 'code
		   :type (flesh-out-type (second var-spec))
		   :current-line (format nil "~a" symbol))))

(defun varjo->glsl (varjo-code &optional (shader-type :vertex))
  (cond 
    ((null varjo-code) nil)
    ((typep varjo-code 'code) varjo-code)
    ((atom varjo-code) 
     (if (assoc varjo-code *glsl-variables*)
	 (instance-var varjo-code)
	 (error "'~s' is unidentified." varjo-code)))
    ((special-functionp (first varjo-code)) 
     (funcall-special (first varjo-code) (rest varjo-code)))
    ((glsl-function (first varjo-code))
     (compile-function (first varjo-code)
		       (mapcar #'varjo->glsl (rest varjo-code))))
    (t (error "Function '~s' is not available for ~A shaders in varjo." (first varjo-code) shader-type))))

;;------------------------------------------------------------

;; expects code objects 
(defun compile-function (func-name arg-objs)
  (let ((func-specs (gethash func-name *glsl-functions*)))
    (loop :for f-spec :in func-specs 
       :if (glsl-valid-function-args f-spec arg-objs )
       :return (make-instance 
                'code 
                :type (glsl-resolve-func-type f-spec arg-objs)
                :current-line (apply #'format 
				     (append 
				      (list nil (func-body f-spec))
				      (mapcar #'current-line
					      arg-objs)))
                :to-block  (mapcan #'to-block arg-objs)
                :to-top (mapcan #'to-top arg-objs))
       :finally (error "There is no applicable method for the ~%glsl function '~s'~%when called with argument types: ~s " func-name (mapcar #'code-type arg-objs)))))

;;------------------------------------------------------------

(defun macroexpand-and-substitute (varjo-code)
  (cond ((null varjo-code) nil)
	((listp varjo-code) 
	 (let ((sub (gethash (first varjo-code)
			     *glsl-substitutions*))) 
	   (if sub
	       (mapcar #'macroexpand-and-substitute
		       (apply sub varjo-code))
	       (mapcar #'macroexpand-and-substitute
		       varjo-code))))
	(t varjo-code)))

;; [TODO] How should we specify unsigned?
(defun replace-numbers (varjo-code)
  (cond ((null varjo-code) nil)
	((numberp varjo-code) 
	 (make-instance 'code :current-line varjo-code
			      :type (get-number-type varjo-code)))
	((listp varjo-code) (mapcar #'replace-numbers varjo-code))
	(t varjo-code)))

(defun get-number-type (x)
  (cond ((floatp x) '(:float nil nil))
	(t '(:int nil nil))))
