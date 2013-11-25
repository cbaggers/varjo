(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defun free-name (name env &aux)
  (if (valid-user-defined-name name)
      (loop :for i = (append (listify (get-var name env))
                             (get-function name env)) 
         :for num from 0
         :while i :do (setf name (symb name '- num))
         :finally (return name))
      (error 'name-unsuitable :name name)))

(defun valid-user-defined-name (name-symbol)
  (let* ((name (string-downcase (symbol-name name-symbol)))
         (matches (cl-ppcre:all-matches "[^a-zA-Z0-9-]" name)))
    (not (or matches (glsl-var-namep name-symbol)))))

(defun glsl-var-namep (name-symbol)
  (let ((name (symbol-name name-symbol)))
    (when (> (length name) 2) 
      (equal "GL-" (subseq name 0 3)))))
