(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

;;[TODO] this smells a bit, it is only used for glsl strings, and we should 
;;       rename this to that end
(let ((num 0))
  (defun free-name (name &optional env)
    (declare (ignore env))
    (if (valid-user-defined-name name)
        (progn (incf num) (symb name '- onum 'v))
        (error 'name-unsuitable :name name))))

(defun valid-user-defined-name (name-symbol)
  (let* ((name (string-downcase (symbol-name name-symbol)))
         (matches (cl-ppcre:all-matches "[^a-zA-Z0-9-]" name)))
    (not (or matches (glsl-var-namep name-symbol)))))

(defun glsl-var-namep (name-symbol)
  (let ((name (symbol-name name-symbol)))
    (or (when (> (length name) 2) (equal "GL-" (subseq name 0 3)))
        (when (> (length name) 2) (equal "FK-" (subseq name 0 3)))
        (when (> (length name) 3) (equal "-SC-" (subseq name 0 4))))))

(let ((count -1))
  (defun free-stemcell-name (name)
    (incf count)
    (symb '-sc- name '- count)))
