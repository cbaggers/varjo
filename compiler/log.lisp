(in-package :varjo)

(defvar *noisy* nil)

(defmacro with-noise (&body body)
  `(let ((*noisy* t))
     ,@body))

(defun printf (control-string &rest format-arguments)
  (when *noisy*
    (if (stringp control-string)
	(apply #'format (append (list t control-string) format-arguments))
	(format t "~%~{~s~}" (cons control-string format-arguments)))))
