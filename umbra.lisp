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

(defun translate-umbra (umbra-code)
  (umbra->glsl
   (macroexpand-and-substitute
    (replace-numbers umbra-code))))

(defun umbra->glsl (umbra-code &optional (shader-type :vertex))
  (cond 
    ((null umbra-code) nil)
    ((typep umbra-code 'code) umbra-code)
    ((atom umbra-code) (error "'~s' is unidentified." umbra-code))
    ((special-functionp (first umbra-code)) 
     (call-special (first umbra-code) 
		   (mapcar #'umbra->glsl (rest umbra-code))))
    ((glsl-function (first umbra-code))
     (compile-function (first umbra-code)
		       (mapcar #'umbra->glsl (rest umbra-code))))
    (t (error "Function '~s' is not available for ~A shaders in umbra." (first umbra-code) shader-type))))

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
				      (mapcar #'code arg-objs)))
                :to-block  (mapcan #'to-block arg-objs)
                :to-top (mapcan #'to-top arg-objs))
       :finally (error "There is no applicable method for the ~%glsl function '~s'~%when called with argument types: ~s " func-name (mapcar #'code-type arg-objs)))))

;;------------------------------------------------------------

(defun macroexpand-and-substitute (umbra-code)
  (cond ((null umbra-code) nil)
	((listp umbra-code) 
	 (let ((sub (gethash (first umbra-code)
			     *glsl-substitutions*))) 
	   (if sub
	       (mapcar #'macroexpand-and-substitute
		       (apply sub umbra-code))
	       (mapcar #'macroexpand-and-substitute
		       umbra-code))))
	(t umbra-code)))

;; [TODO] How should we specify unsigned?
(defun replace-numbers (umbra-code)
  (cond ((null umbra-code) nil)
	((numberp umbra-code) 
	 (make-instance 'code :current-line umbra-code
			      :type (get-number-type umbra-code)))
	((listp umbra-code) (mapcar #'replace-numbers umbra-code))
	(t umbra-code)))

(defun get-number-type (x)
  (cond ((floatp x) '(:float nil nil))
	(t '(:int nil nil))))
