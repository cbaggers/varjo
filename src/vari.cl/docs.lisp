(in-package :vari.cl)

(defun vari-describe (name &optional (stream *standard-output*))
  (let ((doc (or (gethash name glsl-docs:*variables*)
                 (gethash name glsl-docs:*functions*))))
    (when doc
      (format stream "~a~%~%~a" name doc))))
