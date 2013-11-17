(in-package :varjo)

;;[TODO] needs to take and environment, if no var defined then use raw name
(let ((count 0))
  (defun glsl-gensym (&optional (name 'var))
    (setf count (+ 1 count))
    (let ((safe-name (safe-gl-name name)))
      (format nil "_~a_~a" safe-name count))))

;;[TODO] needs to take and environment, not safe if its taken
(defun safe-gl-name (&rest name-parts)
  (let* ((n (string-downcase (string (apply #'symb name-parts))))
         (matches (cl-ppcre:all-matches "[^a-zA-Z0-9-]" n)))
    (if (or matches
            (not (or (< (length n) 2) (not (equal "gl" (subseq n 0 2))))))
        (error "Varjo: Names of variables and functions must be only contain~%alpha-numeric characters and the hyphen character (-).~%They also may not start with 'gl'~%~a" n)
        (cl-ppcre:regex-replace-all "[-]" n "_"))))
